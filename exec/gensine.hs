{-# LANGUAGE TypeOperators #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Generates sine wave.

-}
module Main where

import Control.Monad (void)
import Data.Array.Repa (Array, DIM2, computeP)
import Data.Array.Repa.Repr.ForeignPtr (F)
import qualified Sound.File.Sndfile as S
import Sound.File.Sndfile.Buffer.Vector
import System.Environment (getArgs)

import Data.Array.Repa.IO.Sndfile
import Data.Array.Repa.IO.Sndfile.Examples (genSine, genSineV)


main :: IO ()
main = do
  args <- getArgs
  case args of
    dur:frq:path:mode:_ ->
      let dur' = read dur
          frq' = read frq
          fmt  = wav16 {frames = 48000 * dur'}
      in  case mode of
        "buf" -> do
          arr <- computeP (genSine dur' frq') :: IO (Array F DIM2 Double)
          writeSF path fmt arr
        "vec" -> void $ S.writeFile fmt path (toBuffer $ genSineV dur' frq')
        _     -> usage
    _ -> usage

usage :: IO ()
usage = error "Usage: duration freq path [buf|vec]"

{- ---------------------------------------------------------------------------
 - Write stereo sound, using different frequency for each channel.
 - Modify header format to 'channels = 2' and run.

genSine :: Int -> Double -> Array DIM2 Double
genSine dur frq =
  let sh = Z :. 2 :. (dur * 48000) :: DIM2
  in  fromFunction sh $ \(_ :. i :. j) ->
        sin (frq * (fromIntegral i + 1) * fromIntegral j * pi * 2 / 48000)

Generating 30 seconds of 440 hz sine wav took about 0.2 sec with repa, in 8 core
machine, using 8 threads. It uses 4x more memory of the size of result file.

For just reading and writing sound files as haskell data, storable vector took
almost same execution time. It took about 0.25sec in same machine, memory usage
was less.

-}
