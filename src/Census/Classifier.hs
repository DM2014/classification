module Census.Classifier where

import Census.Type
import System.Random

classify :: IO Label
classify = randomIO
