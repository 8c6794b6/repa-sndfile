{-# LANGUAGE BangPatterns, TypeOperators, ScopedTypeVariables #-}
{-|
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Example for reading and writing sound files, and generating sine wave.

-}
module Test.Examples where

import Foreign.Ptr
import Foreign.Storable (Storable(..))
import Foreign.Marshal
import System.IO.Unsafe (unsafePerformIO)

import Data.Array.Repa ((:.)(..), Array(..), DIM2, Z(..))
import Data.Array.Repa.Repr.ForeignPtr (F)
import Sound.File.Sndfile (Sample(..))

import qualified Data.Array.Repa as R
import qualified Data.Vector.Storable as V
import qualified Sound.File.Sndfile as S
import qualified Sound.File.Sndfile.Buffer.Vector as SV

import Data.Array.Repa.IO.Sndfile

-- ---------------------------------------------------------------------------
-- Read sound file as repa array, then write it without modification.

test_copy :: FilePath -> FilePath -> IO ()
test_copy i o = do
  (info, arr) <- readSF i :: IO (Info, Array F DIM2 Double)
  writeSF o info arr

-- ---------------------------------------------------------------------------
-- Using storable vector, run faster when input file is small.

test_read_vec :: Sample a => FilePath -> IO (Info, V.Vector a)
test_read_vec file = do
  (i, res) <- S.readFile file
  case res of
    Nothing  -> error "Fail"
    Just sig -> return $ (i, SV.fromBuffer sig)

test_write_vec :: Sample a => Info -> FilePath -> V.Vector a -> IO ()
test_write_vec info file vec = do
  _ <- S.writeFile info file (SV.toBuffer vec)
  return ()

test_copy_vec :: FilePath -> FilePath -> IO ()
test_copy_vec ifile ofile = do
  (i, vec) <- test_read_vec ifile :: IO (Info, V.Vector Double)
  test_write_vec i ofile vec

-- -- ---------------------------------------------------------------------------
-- -- Raw operations

read_raw :: FilePath -> IO (Info, [Double])
read_raw path = do
  info <- S.getFileInfo path
  hdl <- S.openFile path S.ReadMode info
  ptr <- malloc :: IO (Ptr Double)

  let go p n acc
        | n == S.frames info = return acc
        | otherwise          = do
          _ <- S.hGetBuf hdl p 1
          !v <- peek p
          go p (n+1) (v:acc)

  vs <- go ptr 0 []
  free ptr
  S.hClose hdl
  return $ (info, reverse vs)

read_arr :: FilePath -> IO (Info, Array F DIM2 Double)
read_arr path = do
  info <- S.getFileInfo path
  hdl <- S.openFile path S.ReadMode info
  let sizeOfDouble = sizeOf (undefined :: Double)
      nf = S.frames info
      nc = S.channels info
  arr <- allocaBytes (nf*nc*sizeOfDouble) $ \ptr -> do
    _ <- S.hGetBuf hdl ptr (nf * nc)
    let sh = Z :. 1 :. nf :: DIM2
        go (_ :. (!i)) = do
          !v <- peekElemOff ptr i
          return v
    return $ R.fromFunction sh $ \ix -> unsafePerformIO (go ix)
  S.hClose hdl
  arr' <- R.computeP arr
  return (info, arr')

write_raw :: FilePath -> Info -> [Double] -> IO ()
write_raw path info vs = do
  hdl <- S.openFile path S.WriteMode info
  ptr <- malloc :: IO (Ptr Double)
  let go p xs = case xs of
        []      -> return ()
        (!y:ys) -> poke p y >> S.hPutBuf hdl p 1 >> go p ys
  go ptr vs
  free ptr
  S.hClose hdl

sin440 :: [Double]
sin440 = [sin (440 * i * pi * 2 / 48000) | i <- [0..]]

write_sin_raw :: FilePath -> IO ()
write_sin_raw path = write_raw path (waveMonoPcm16 ns) (take ns sin440)
  where
    ns = 48000

genSine :: Int -> Double -> Array R.D DIM2 Double
genSine dur frq = R.fromFunction sh go
  where
    sh = Z :. 1 :. (dur * sr)
    go (_:._:.j) = sin (frq * fromIntegral j * pi * 2 / sr)
    sr :: Num a => a
    sr = 48000

waveMonoPcm16 :: Int -> Info
waveMonoPcm16 nsample = Info
  { samplerate = 48000
  , frames = nsample
  , channels = 1
  , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
  , sections = 1
  , seekable = True }

copy_raw :: FilePath -> FilePath -> IO ()
copy_raw ifile ofile = do
  (info, arr) <- read_raw ifile
  write_raw ofile info arr

sin440_ex :: IO ()
sin440_ex = do
  let dur = 3; freq = 440; sr = 48000
      hdr = wav16 {samplerate = sr, frames = sr * dur}
      sig = R.fromFunction (Z :. 1 :. dur * sr) $ \(_ :. _ :. i) ->
        sin (fromIntegral i * freq * pi * 2 / fromIntegral sr)
  sig' <- R.computeP sig :: IO (Array F DIM2 Double)
  writeSF "sin440.wav" hdr sig'

main_ex :: IO ()
main_ex = do
  (i, a) <- readSF "sin440.wav" :: IO (Info, Array F DIM2 Double)
  writeSF "out.wav" i a