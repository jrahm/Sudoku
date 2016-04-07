{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

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

{-| the backtracking sudoku solver -}

type SudokuArray s = STArray s (Int, Int) (Maybe Int)
type SolvedSudoku = Array (Int, Int) Int

traceOn = True

traceM' x = when traceOn $ traceM x

matrix :: ST s (SudokuArray s)
matrix = newListArray ((0,0), (3,3)) (repeat Nothing)

isInt x = x == fromInteger (round x)

solve :: forall s. SudokuArray s -> ST s (Either String SolvedSudoku)
solve arr = do
        ((r1, c1), (r2, c2)) <- getBounds arr
        let (w, h) = (r2 - r1 + 1, c2 - c1 + 1)

        let n = round (sqrt $ fromIntegral w)
        if w == h && isInt (sqrt $ fromIntegral w) then
                solve' (0, 0) n 1 >> serialize
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


        solve' :: (Int, Int) -> Int -> Int ->  ST s (Maybe String)
        solve' place size n | n > size*size = return (Just $ "Fail at " ++ show place)
        solve' (row, col) size n | col == size*size && row < size*size =
            solve' (row+1, 0) size n

        solve' (row, col) size n | row == size*size = do
            traceM $ "row = " ++ show row
            return Nothing

        solve' idx@(row, col) size n = do
            traceM' $ printf "(%d, %d) = %d" row col n

            valid <- isValidPosition idx n size

            if valid then do
                writeArray arr idx (Just n)
                err <- solve' (row, col + 1) size 1
                case err of
                    Just err -> solve' idx size (n + 1)
                    Nothing -> return Nothing

            else
                solve' idx size (n + 1)

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
main =
    let earr = runST (solve =<< matrix)
        in

        case earr of
            Left err -> putStrLn err
            Right arr ->
                let ((r0, c0), (r1, c1)) = bounds arr in

                forM_ [r0 .. r1] $ \row -> do
                    forM_ [c0 .. c1] $ \col ->
                        putStr $ show (arr ! (row, col)) ++ " "
                    putStrLn ""
