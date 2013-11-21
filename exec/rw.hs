module Main where

import System.Environment (getArgs)
import Data.Array.Repa.IO.Sndfile.Examples (copySF)

{-
Using storable vector runs faster, depending on file size.
-}
main :: IO ()
main = do
  ifile:ofile:_ <- getArgs
  copySF ifile ofile
  return ()
