module Main where

import Census.Parser
import Census.Evaluation

import              Control.Monad.Trans.Resource
import              Data.Conduit
import qualified    Data.Conduit.Binary as CB
import qualified    Data.Conduit.List as CL
--import              System.IO (stdin, stdout)

main :: IO ()
main = do

    labeled <- runResourceT $ CB.sourceFile "data/census" $$ parser =$= CL.catMaybes =$ CL.consume
    --census <- runResourceT $ CB.sourceHandle stdin $$ parser =$ CL.consume
    a' <- mapM test labeled
    print $ accuracy a'
    print $ precision a'
    print $ recall a'
    print $ fMeasure a'

