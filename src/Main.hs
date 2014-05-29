module Main where

import Census.Parser
import Census.Evaluation
import Census.Type

import              Control.Monad.Trans.Resource
import              Data.Conduit
import qualified    Data.Conduit.Binary as CB
import qualified    Data.Conduit.List as CL
--import              System.IO (stdin, stdout)

main :: IO ()
main = do

    labeled <- runResourceT $ CB.sourceFile "data/census" $$ parser =$= CL.catMaybes =$ CL.consume
    
    putStrLn $ "# there are " ++ show (length labeled) ++ " data points\n"

    putStrLn "# cross validation"
    mapM_ (printCrossValidation labeled) [1 .. 20]
    putStrLn "# kFold"
    mapM_ (printKFold labeled) [1 .. 10]

printKFold :: [Labeled] -> Int -> IO ()
printKFold labeled k = do
    putStrLn $ "  k: " ++ show k
    kFold k labeled >>= printStat


printCrossValidation :: [Labeled] -> Int -> IO ()
printCrossValidation labeled firstNk = do
    putStrLn $ "  training data: 1 - " ++ show (1000 * firstNk)

    let trainingData = take (1000 * firstNk) labeled
    let testingData  = drop 20000 labeled

    crossValidate trainingData testingData >>= printStat

    putStrLn ""

printStat :: [Result] -> IO ()
printStat results = do

    putStrLn $ "  accuracy:  " ++ show (accuracy  results)
    putStrLn $ "  precision: " ++ show (precision results)
    putStrLn $ "  recall:    " ++ show (recall    results)
    putStrLn $ "  f-measure: " ++ show (fMeasure  results)