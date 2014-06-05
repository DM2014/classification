module Census.Evaluation where

import Census.Type
import Census.Classifier

validate :: Labeled -> IO Result
validate (Labeled _ True) = do
    x <- classify
    case x of
        True -> return TruePositive
        False -> return FalseNegative
validate (Labeled _ False) = do
    x <- classify
    case x of
        True -> return FalsePositive
        False -> return TrueNegative

truePositive :: [Result] -> Int
truePositive = length . filter (== TruePositive)

falsePositive :: [Result] -> Int
falsePositive = length . filter (== FalsePositive)

trueNegative :: [Result] -> Int
trueNegative = length . filter (== TrueNegative)

falseNegative :: [Result] -> Int
falseNegative = length . filter (== FalseNegative)

accuracy :: [Result] -> Double
accuracy = weightedAccuracy 1 1 1 1

precision :: [Result] -> Double
precision = weightedAccuracy 1 0 1 0

recall :: [Result] -> Double
recall = weightedAccuracy 1 1 0 0

fMeasure :: [Result] -> Double
fMeasure = weightedAccuracy 2 1 1 0

weightedAccuracy :: Double -> Double -> Double -> Double -> [Result] -> Double
weightedAccuracy wA wB wC wD rs = (a + d) / (a + b + c + d)
    where   a = wA * fromIntegral (truePositive rs)
            b = wB * fromIntegral (falseNegative rs)
            c = wC * fromIntegral (falsePositive rs)
            d = wD * fromIntegral (trueNegative rs)

kFold :: Int -> [Labeled] -> IO [Result]
kFold k rs = mapM (uncurry crossValidate) samples >>= return . concat
    where   samples = map (isolate k rs) [0..k - 1]

isolate :: Int -> [a] -> Int -> ([a], [a])
isolate n rs i = (others, picked)
    where   chunks = split n rs
            picked = chunks !! i
            others = concat . map ((!!) chunks) $ filter (/= i) [0 .. n - 1]

split :: Int -> [a] -> [[a]]
split m = split' m m
    where   split' _ 1 xs = [xs]
            split' n i xs = take n xs : split' n (i - 1) (drop n xs)

crossValidate :: [Labeled] -> [Labeled] -> IO [Result]
crossValidate _ = mapM validate