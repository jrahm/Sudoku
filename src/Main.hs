{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.PooledIO.Independent
import Control.DeepSeq
import Control.Exception.Base
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.ST
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Writer

import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Char
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Time.Clock.POSIX

import Debug.Trace

import System.CPUTime
import System.Environment
import System.IO.Unsafe
import System.Random
import System.Random.Shuffle
import System.Timeout

import Text.Printf

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

{-| the backtracking sudoku solver -}

type SudokuArray = Array (Int, Int) Int
type ThawedSudokuArray s = STUArray s (Int, Int) Int

{- I wish haskell had some dependent typing for here -}
data SudokuPossible =
        SudokuPossible {
              gameSize :: Int -- sqrt n

            , boxSets :: Map (Int, Int) (Set Int) -- set of possible values left for each box
            , rowSets :: IntMap (Set Int) -- set of possible values left for each row
            , colSets :: IntMap (Set Int) -- set of possible values left for each col
        }

readSudokuFile :: FilePath -> IO (Either String SudokuArray)
{-^ Read from a sudoku array from a file. The file is structured
 - as a text file with n^2 lines and n^2 columns in each line. A dot
 - ('.') is considered an empty cell, and a -}
readSudokuFile path =
    runEitherT $ do
        lns <- lift $ lines <$> readFile path

        let nsq = length lns
            n = round (sqrt $ fromIntegral nsq)

        when (n * n /= nsq) $
            left $ printf "Invalid soduku dimensions %dx%d. (Must be of form n^2xn^2)" nsq nsq

        when (any (\x -> length x /= nsq) lns) $
            left $ printf "Some line not equal to %d in sudoku" n

        let list =
             concatMap (\(row, l) ->
                    map (\(col, char) ->
                            ((row, col), fromChar char)) (zip [0..] l)
                ) (zip [0..] lns)

        return (array ((0,0), (nsq-1, nsq-1)) list)

    where
        fromChar '.' = 0
        fromChar c | isDigit c = ord c - ord '0'
        fromChar c = ord c - ord 'A' + 10

sudokuArrayToString :: SudokuArray -> String
sudokuArrayToString array =
    let ((r0, c0), (r1, c1)) = bounds array in
    execWriter $
      forM_ [r0 .. r1] $ \row -> do
        forM_ [c0 .. c1] $ \col ->
          tell $ return $ fromInt (array ! (row, col))
        tell "\n"

    where
        fromInt 0 = '.'
        fromInt i | i < 10 = chr (i + ord '0')
        fromInt i = chr (i - 10 + ord 'A')


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
            colset <- IM.lookup col cols

            let intersect = boxset `S.intersection` rowset `S.intersection` colset
            return $ flip map (S.toList intersect) $ \value ->
                let update = Just . S.delete value
                    boxes' = M.update update boxidx boxes
                    rows' = IM.update update row rows
                    cols' = IM.update update col cols
                    in
                    (value, SudokuPossible size boxes' rows' cols')

matrix :: Int -> ST s (ThawedSudokuArray s)
matrix n = newListArray ((0,0), (n*n-1, n*n-1)) (repeat 0)

isInt x = x == fromInteger (round x)

pruneSudokuPossible :: ThawedSudokuArray s -> SudokuPossible -> ST s SudokuPossible
pruneSudokuPossible arr possible@SudokuPossible {gameSize=size} =
    flip execStateT possible $

        forM_ [0..size*size-1] $ \row ->
            forM_ [0..size*size-1] $ \col -> do

                value' <- lift $ readArray arr (row, col)

                case value' of
                    0 -> return ()
                    value -> do
                        let boxidx = (row `div` size, col `div` size)
                        let update = Just . S.delete value

                        modify (\s@SudokuPossible{boxSets = boxes} ->
                                    s {boxSets = M.update update boxidx boxes})

                        modify (\s@SudokuPossible{rowSets = rows} ->
                                    s {rowSets = IM.update update row rows})

                        modify (\s@SudokuPossible{colSets = cols} ->
                                    s {colSets = IM.update update col cols})

generateSudoku :: (RandomGen g) => Int -> Rand g SudokuArray
{- Generates a random sudoku board. -}
generateSudoku n = liftRand $
      \stgen ->
        let (st1, st2) = split stgen
            ret = runST $
                    evalRandT (
                       lift (matrix n) >>=
                         (\mat ->
                           solveA mat >> lift (freeze mat))
                     ) st1
            in
            (ret, st1)

removeNumbers :: (RandomGen g) => Int -> SudokuArray -> Rand g SudokuArray
{- Deletes numbers from sudoku. The number provided is the number
 - of hints to keep -}
removeNumbers n array = do
    indicies <- drop n <$> shuffleM (range $ bounds array)
    return $ runST (shuffle indicies =<< thaw array)
    where
     shuffle :: [(Int, Int)] -> ThawedSudokuArray s -> ST s SudokuArray
     shuffle indicies arrM = do
         forM_ indicies $ \i ->
             writeArray arrM i 0
         freeze arrM

solve :: SudokuArray -> SudokuArray
solve arr = runST $ evalRandT (do
                        arrM <- lift $ thaw arr
                        solveA arrM
                        lift $ freeze arrM) (mkStdGen 0)

solveA :: forall s g. (RandomGen g) => ThawedSudokuArray s -> RandT g (ST s) ()
solveA arr = do
        ((r1, c1), (r2, c2)) <- lift $ getBounds arr
        let (w, h) = (r2 - r1 + 1, c2 - c1 + 1)
        let n = round (sqrt $ fromIntegral w)

        unless ((w == h) && isInt (sqrt (fromIntegral w))) $
            error "Invalid Sudoku Bounds"

        possible <- lift $ pruneSudokuPossible arr (mkSudokuPossible n)
        void $ solve' possible (0, 0) n

    where

        solve' :: forall g. (RandomGen g) => SudokuPossible -> (Int, Int) -> Int -> RandT g (ST s) (Maybe String)
        solve' possible (row, col) size | col == size*size && row < size*size =
            solve' possible (row+1, 0) size

        solve' possible (row, col) size | row == size*size =
            return Nothing

        solve' possible idx@(row, col) size = do
            let subTrees = getPossibleValues possible idx

                loop trees =
                    case trees of
                        [] -> return (Just $ "Fail at " ++ show idx)
                        ((i, newpossible):ts) -> do
                            val <- solve' newpossible (row, col + 1) size
                            case val of
                                Just _ -> loop ts
                                Nothing -> do
                                    lift $ writeArray arr idx i
                                    return Nothing

            value <- lift $ readArray arr idx
            newTrees <- shuffleM subTrees
            case value of
                0 -> loop newTrees
                _ -> solve' possible (row, col + 1) size

-- main :: IO ()
-- main = do
--     array <- readSudokuFile "test/9x9-empty.txt"
--     forM_ array $ \matrix ->
--       forM_ [1..1000] $ \num -> do
--         gen <- newStdGen
--         let earr = runST (solve gen =<< thaw matrix)
--
--         case earr of
--           Left err -> putStrLn err
--           Right arr -> do
--             let ((r0, c0), (r1, c1)) = bounds arr
--
--             let str = execWriter $
--                   forM_ [r0 .. r1] $ \row -> do
--                     forM_ [c0 .. c1] $ \col ->
--                       case arr ! (row, col) of
--                           0 -> tell "."
--                           i -> tell $ return $ chr (i + ord 'A' - 1)
--                     tell "\n"
--
--             writeFile ("test/9x9-" ++ show num ++ ".txt") str

getTime = round <$> ((*1000) <$> getPOSIXTime)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

main :: IO ()
main =

    let lst = [(n, rnd, percent) | rnd <- [0::Int .. 999],
                                   percent <- [0.8::Double, 0.6, 0.4, 0.2],
                                   n <- [2, 3]
                                   ]
                                   in
       run $ flip map lst $ \(n, roundNumber, percentKeep) -> do
              let n4 = n * n * n * n
              let h = round (percentKeep * fromIntegral n4) :: Int
              let header = printf "- %02d %03d %03d -" n h roundNumber

              let fname = printf "runs/run-%02d-%03d-%03d.txt" n h roundNumber


              val <- timeout (10 * 60 * 1000000) $ do
                putStrLn header

                arr <- evalRandIO (generateSudoku n)
                arr' <- evalRandIO (removeNumbers h arr)

                startTime <- getTime
                solved <- evaluate (solve arr')
                endTime <- getTime

                return $
                    sudokuArrayToString arr ++ "\n\n" ++
                    sudokuArrayToString arr' ++ "\n\n" ++
                    sudokuArrayToString solved ++ "\n\n" ++
                    printf "time: " ++ show (endTime - startTime) ++ "\n"

              case val of
                  Nothing -> do
                      putStrLn "TIMEOUT!"
                      writeFile fname "Timeout"

                  Just s ->
                      writeFile fname s





-- main :: IO ()
-- main = do
--     args <- getArgs
--     random <- newStdGen
--
--     forM_ args $ \filePath -> do
--         putStrLn filePath
--         array <- readSudokuFile filePath
--
--         case array of
--             Left err -> putStrLn err
--             Right matrix ->
--                 let earr = runST (solve random =<< thaw matrix) in
--
--                 case earr of
--                     Left err -> putStrLn err
--                     Right arr ->
--                         let ((r0, c0), (r1, c1)) = bounds arr in
--
--                         forM_ [r0 .. r1] $ \row -> do
--                             forM_ [c0 .. c1] $ \col ->
--                                 putStr $ return $ chr (arr ! (row, col) + ord 'A' - 1)
--                             putStrLn ""
