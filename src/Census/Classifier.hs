module Census.Classifier where

import Census.Type
import System.Random

classify :: IO Label
classify = randomIO

classifyBatch :: [Labeled] -> [Unlabeled] -> IO [Label]
classifyBatch _ = mapM (\ _ -> classify)