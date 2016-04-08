{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char
import Control.Monad.Trans
import Control.Monad.ST
import Control.Monad.Trans.Either

import Data.Array.MArray
import Data.Array.ST
import Data.Array

import Control.Monad
import Control.Monad.Loops

import Debug.Trace
import Text.Printf

import qualified Data.Set as S
import Data.Set (Set)

import qualified Data.Map as M
import Data.Map (Map)

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import Data.Maybe
import Control.Monad.State.Lazy

import System.Environment

{-| the backtracking sudoku solver -}

type SudokuArrayFreeze = Array (Int, Int) (Maybe Int)
type SudokuArray s = STArray s (Int, Int) (Maybe Int)
type SolvedSudoku = Array (Int, Int) Int

{- I wish haskell had some dependent typing for here -}
data SudokuPossible =
        SudokuPossible {
              gameSize :: Int -- sqrt n

            , boxSets :: Map (Int, Int) (Set Int) -- set of possible values left for each box
            , rowSets :: IntMap (Set Int) -- set of possible values left for each row
            , colSets :: IntMap (Set Int) -- set of possible values left for each col
        }

readSudokuFile :: FilePath -> IO (Either String SudokuArrayFreeze)
readSudokuFile path =
    runEitherT $ do
        lns <- lift $ lines <$> readFile path

        let nsq = length lns
            n = round (sqrt $ fromIntegral nsq)

        when (n * n /= nsq) $
            left $ printf "Invalid soduku dimensions %dx%d. (Must be of form n^2xn^2" nsq nsq

        when (any (\x -> length x /= n) lns) $
            left $ printf "Some line not equal to %d in sudoku"

        let list =
             concatMap (\(row, l) ->
                    map (\(col, char) ->
                            ((row, col), fromChar char)) (zip [0..] l)
                ) (zip [0..] lns)

        return (array ((0,0), (nsq-1, nsq-1)) list)

    where
        fromChar '.' = Nothing
        fromChar c = Just $ ord c - ord 'A' + 1


mkSudokuPossible :: Int -> SudokuPossible
mkSudokuPossible sqrtN =
    let allset = S.fromList [1..sqrtN*sqrtN] in
    SudokuPossible sqrtN
        (M.fromList $ map (,allset) (range ((0, 0), (sqrtN, sqrtN))))
        (IM.fromList $ map (,allset) [0..sqrtN*sqrtN - 1])
        (IM.fromList $ map (,allset) [0..sqrtN*sqrtN - 1])


-- ^ takes a stat of possible numbers and a position and a list of
-- possible number with updated states.
getPossibleValues :: SudokuPossible -> (Int, Int) -> [(Int, SudokuPossible)]
getPossibleValues (SudokuPossible size boxes rows cols) (row, col) =
    fromMaybe (error $ printf "Bad row and column %s" (show (row, col))) ans

    where
        ans = do
            let boxidx = (row `div` size, col `div` size)

            boxset <- M.lookup boxidx boxes
            rowset <- IM.lookup row rows
            colset <- IM.lookup row rows

            let intersect = boxset `S.intersection` rowset `S.intersection` colset
            return $ flip map (S.toList intersect) $ \value ->
                let update = Just . S.delete value
                    boxes' = M.update update boxidx boxes
                    rows' = IM.update update row rows
                    cols' = IM.update update col cols
                    in
                    (value, SudokuPossible size boxes' rows' cols')

traceOn = True

traceM' x = when traceOn $ traceM x

matrix :: ST s (SudokuArray s)
matrix = newListArray ((0,0), (24, 24)) (repeat Nothing)

isInt x = x == fromInteger (round x)

pruneSudokuPossible :: SudokuArray s -> SudokuPossible -> ST s SudokuPossible
pruneSudokuPossible arr possible@SudokuPossible {gameSize=size} =
    flip execStateT possible $

        forM_ [0..size*size-1] $ \row ->
            forM_ [0..size*size-1] $ \col -> do

                value' <- lift $ readArray arr (row, col)

                case value' of
                    Just value -> do
                        let boxidx = (row `div` size, col `div` size)
                        let update = Just . S.delete value

                        modify (\s@SudokuPossible{boxSets = boxes} ->
                                    s {boxSets = M.update update boxidx boxes})

                        modify (\s@SudokuPossible{rowSets = rows} ->
                                    s {rowSets = IM.update update row rows})

                        modify (\s@SudokuPossible{colSets = cols} ->
                                    s {colSets = IM.update update col cols})

                    Nothing -> return ()

solve :: forall s. SudokuArray s -> ST s (Either String SolvedSudoku)
solve arr = do
        ((r1, c1), (r2, c2)) <- getBounds arr
        let (w, h) = (r2 - r1 + 1, c2 - c1 + 1)

        let n = round (sqrt $ fromIntegral w)
        if w == h && isInt (sqrt $ fromIntegral w) then do
                possible <- pruneSudokuPossible arr (mkSudokuPossible n)
                solve' possible (0, 0) n >> serialize
            else
                return (Left ("Invalid sudoku game bounds: " ++ show (w,h)))
    where
        serialize :: ST s (Either String SolvedSudoku)
        serialize = do
            assocs <- getAssocs arr
            let x = mapM (\(i, v) -> case v of
                             Nothing -> Left $ "unset value at " ++ show i
                             Just v' -> Right (i, v')) assocs

            case x of
                Left v -> return (Left v)
                Right mp -> do
                    ret <- (flip newArray 0 =<< getBounds arr) :: ST s (STArray s (Int, Int) Int)
                    forM_ mp $ uncurry (writeArray ret)
                    Right <$> freeze ret


        {- No news is good news -- NNIGN-}
        solve' :: SudokuPossible -> (Int, Int) -> Int -> ST s (Maybe String)
        solve' possible (row, col) size | col == size*size && row < size*size =
            solve' possible (row+1, 0) size

        solve' possible (row, col) size | row == size*size = do
            traceM $ "row = " ++ show row
            return Nothing

        solve' possible idx@(row, col) size =
            let subTrees = getPossibleValues possible idx

                loop :: [(Int, SudokuPossible)] -> ST s (Maybe String)
                loop trees =
                    case trees of
                        [] -> return (Just $ "Fail at " ++ show idx)
                        ((i, newpossible):ts) -> do
                            val <- solve' newpossible (row, col + 1) size
                            case val of
                                Just _ -> loop ts
                                Nothing -> do
                                    traceM $ printf "%s = %d" (show idx) i
                                    writeArray arr idx (Just i)
                                    return Nothing
            in

            loop subTrees

        (^&&^) = liftM2 (&&)


        isValidPosition :: (Int, Int) -> Int -> Int ->  ST s Bool
        isValidPosition _ value sz | sz*sz < value = return False
        isValidPosition (row, col) value sz =
            isValidRow row value sz ^&&^
            isValidCol col value sz ^&&^
            isValidBox (row, col) value sz

        isValidRow :: Int -> Int -> Int -> ST s Bool
        isValidRow row value sz =
            let size = sz*sz in
            allM (\col -> liftM (/=Just value) (readArray arr (row, col))) [0..size-1]

        isValidCol :: Int -> Int -> Int -> ST s Bool
        isValidCol col value sz =
            let size = sz*sz in
            allM (\row -> liftM (/=Just value) (readArray arr (row, col))) [0..size-1]

        isValidBox :: (Int, Int) -> Int -> Int -> ST s Bool
        isValidBox (row, col) value sz =
            let boxRowIdx = (row `div` sz) * sz
                boxColIdx = (col `div` sz) * sz in

                allM (\(row, col) -> liftM (/=Just value) (readArray arr (row, col)))
                    (range ((boxRowIdx, boxColIdx), (boxRowIdx + sz - 1, boxColIdx + sz - 1)))




    -- b@((r0, c0), (r1, c1)) <-  getBounds arr

    -- ret <- newArray b 0 :: ST s (STArray s (Int, Int) Int)

    -- forM_ [r0 .. r1] $ \row ->
    --     forM_ [c0 .. c1] $ \col -> do
    --         elem <-  readArray arr (row, col)
    --         case elem of
    --             Nothing -> writeArray ret (row, col) 0
    --             Just x -> writeArray ret (row, col) x

    -- Right <$> freeze ret


main :: IO ()
main = do
    args <- getArgs

    forM_ args $ \filePath -> do
        array <- readSudokuFile filePath

        case array of
            Left err -> putStrLn err
            Right matrix ->
                let earr = runST (solve =<< thaw matrix) in

                case earr of
                    Left err -> putStrLn err
                    Right arr ->
                        let ((r0, c0), (r1, c1)) = bounds arr in

                        forM_ [r0 .. r1] $ \row -> do
                            forM_ [c0 .. c1] $ \col ->
                                putStr $ show (arr ! (row, col)) ++ " "
                            putStrLn ""
