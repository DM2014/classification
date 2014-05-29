module Census.Evaluation where

import Census.Type
import Census.Classifier

test :: Labeled -> IO Result
test (Labeled _ True) = do
    x <- classify
    case x of
        True -> return TruePositive
        False -> return FalseNegative
test (Labeled _ False) = do
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
--
--kFold :: Int -> [Result] ->