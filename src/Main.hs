module Main where

import Census.Parser

import              Control.Monad.Trans.Resource
import              Data.Conduit
import qualified    Data.Conduit.Binary as CB
import qualified    Data.Conduit.List as CL
import              System.IO (stdin, stdout)

main :: IO ()
main = do

    census <- runResourceT $ CB.sourceFile "data/census" $$ parser =$ CL.consume
    --census <- runResourceT $ CB.sourceHandle stdin $$ parser =$ CL.consume
    print $ census !! 430
    print $ census !! 431