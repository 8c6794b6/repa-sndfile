{-# LANGUAGE BangPatterns, TypeOperators, ScopedTypeVariables #-}
{-|
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Example for reading and writing sound files, and generating sine wave.

-}
module Data.Array.Repa.IO.Sndfile.Examples where

import Control.Monad (void)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (allocaBytes, free, malloc)
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
-- * Using Repa

-- | Read sound file as repa array, then write it without modification.
copySF :: FilePath -> FilePath -> IO ()
copySF i o =
  uncurry (writeSF o) =<< (readSF i :: IO (Info, Array F DIM2 Double))

-- | Generate sine wave.
genSine ::
    Int       -- ^ Duration in seconds.
    -> Double -- ^ Frequency.
    -> Array R.D DIM2 Double
genSine dur frq = R.fromFunction sh go
  where
    sh = Z :. 1 :. (dur * sr)
    {-# INLINE sh #-}
    go (_:._:.j) = sin (frq * fromIntegral j * pi * 2 / sr)
    {-# INLINE go #-}
    sr :: Num a => a
    {-# INLINE sr #-}
    sr = 48000

sin440and880 :: IO ()
sin440and880 = do
    let dur = 3; freq1 = 440; freq2 = 880; sr = 480000
        hdr = wav16 {samplerate = sr, channels = 2, frames = sr * dur * 2}
        gen f i = sin (fromIntegral i * f * pi * 2 / fromIntegral sr)
        sig = R.fromFunction (Z :. 2 :. dur * sr) $ \(_ :. c :. i) ->
            case c of
                0 -> gen freq1 i
                1 -> gen freq2 i
                _ -> 0
    sig' <- R.computeP sig :: IO (Array F DIM2 Double)
    writeSF "sin440and880.wav" hdr sig'

-- ---------------------------------------------------------------------------
-- * Using storable vector
--
-- Runs faster when input file is small.
--

read_vec :: Sample a => FilePath -> IO (Info, V.Vector a)
read_vec file = do
  (i, res) <- S.readFile file
  maybe (error "Fail") (\sig -> return (i, SV.fromBuffer sig)) res

write_vec :: Sample a => FilePath -> Info -> V.Vector a -> IO ()
write_vec file info vec = void $ S.writeFile info file (SV.toBuffer vec)

copy_vec :: FilePath -> FilePath -> IO ()
copy_vec ifile ofile =
    (read_vec ifile :: IO (Info, V.Vector Double)) >>= uncurry (write_vec ofile)

-- | Like 'genSine', but with 'V.Vector'.
genSineV ::
    Int       -- ^ Duration in seconds.
    -> Double -- ^ Frequency.
    -> V.Vector Double
genSineV dur frq = V.generate (sr * dur) gen
  where
    gen i = sin (frq * fromIntegral i * pi * 2 / sr)
    sr :: Num a => a
    sr = 48000
{-# INLINE genSineV #-}

-- ---------------------------------------------------------------------------
-- * Raw operations

read_raw :: FilePath -> IO (Info, [Double])
read_raw path = do
  info <- S.getFileInfo path
  hdl <- S.openFile path S.ReadMode info
  ptr <- malloc :: IO (Ptr Double)
  let go p n acc
        | n == S.frames info = return acc
        | otherwise          = do
          void $ S.hGetBuf hdl p 1
          !v <- peek p
          go p (n+1) (v:acc)
  vs <- go ptr 0 []
  free ptr
  S.hClose hdl
  return $ (info, reverse vs)

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

copy_raw :: FilePath -> FilePath -> IO ()
copy_raw ifile ofile = uncurry (write_raw ofile) =<< read_raw ifile

genSineL :: Int -> Double -> [Double]
genSineL dur frq = take (48000*dur) [sin (frq * i * pi * 2 / 48000) | i <- [0..]]

read_arr :: FilePath -> IO (Info, Array F DIM2 Double)
read_arr path = do
  info <- S.getFileInfo path
  hdl <- S.openFile path S.ReadMode info
  let sizeOfDouble = sizeOf (undefined :: Double)
      nf = S.frames info
      nc = S.channels info
  arr <- allocaBytes (nf*nc*sizeOfDouble) $ \ptr -> do
    void $ S.hGetBuf hdl ptr (nf * nc)
    let sh = Z :. 1 :. nf :: DIM2
        go (_ :. (!i)) = do
          !v <- peekElemOff ptr i
          return v
    return $ R.fromFunction sh $ \ix -> unsafePerformIO (go ix)
  S.hClose hdl
  arr' <- R.computeP arr
  return (info, arr')


-- --------------------------------------------------------------------------
-- * Auxilliary

waveMonoPcm16 :: Int -> Info
waveMonoPcm16 nsample = Info
  { samplerate = 48000
  , frames = nsample
  , channels = 1
  , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
  , sections = 1
  , seekable = True }
