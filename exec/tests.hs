module Main where

import Data.Array.Repa (Array, DIM2, computeP)
import Data.Array.Repa.Repr.ForeignPtr (F)
import System.Directory (getTemporaryDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import Data.Array.Repa.IO.Sndfile (readSF, writeSF)
import Data.Array.Repa.IO.Sndfile.Examples (genSine, waveMonoPcm16)

main :: IO ()
main = do
    tmpdir <- getTemporaryDirectory
    let outFile = tmpdir </> "sin440.wav"
        dur, frq, sr :: Num a => a
        dur     = 3
        frq     = 440
        sr      = 48000
        fmt     = waveMonoPcm16 (dur * sr)
    arr <- computeP (genSine dur frq) :: IO (Array F DIM2 Double)
    writeSF outFile fmt arr
    (fmt', _arr') <- asTypeOf (fmt, arr) `fmap` readSF outFile
    if fmt' == fmt then exitSuccess else exitFailure
