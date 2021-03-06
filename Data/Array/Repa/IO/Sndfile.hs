{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : $Header$
CopyRight   : (c) 2011-2013, 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Read and write audio file with repa arrays using libsndfile via hsndfile.
Note that this module re-exports header related types from hsndfile.

For more info about supported format, visit libsndfile web site:

* libsndfile: <http://www.mega-nerd.com/libsndfile/>

* hsndfile <http://haskell.org/haskellwiki/Hsndfile>

-}
module Data.Array.Repa.IO.Sndfile
  (
    -- * Examples
    -- $examples

    -- * Sound file reader and writer
    readSF
  , writeSF
  , withSF

    -- * Sound file headers (re-exports from hsndfile)
  , S.Info(..)
  , S.Format(..)
  , S.HeaderFormat(..)
  , S.EndianFormat(..)
  , S.SampleFormat(..)
  , S.Count

    -- * Utils
  , toMC
  , fromMC
  , wav16
  , wav32

  ) where

import Foreign.ForeignPtr (ForeignPtr)

import Data.Array.Repa (Array, DIM1, DIM2, Z(..), (:.)(..), Source)
import Data.Array.Repa.Eval (Target)
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Int (Int16, Int32)
import Sound.File.Sndfile (Buffer(..), Info(..), Sample)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import qualified Sound.File.Sndfile as S

{-$examples

Read \"in.wav\", write to \"out.wav\" with same format.

> module Main where
>
> import Data.Array.Repa
>   ((:.)(..), Array, Z(..), DIM2, computeP, fromFunction)
> import Data.Array.Repa.Repr.ForeignPtr (F)
> import Data.Array.Repa.IO.Sndfile
>
> main :: IO ()
> main = do
>   (i, a) <- readSF "in.wav" :: IO (Info, Array F DIM2 Double)
>   writeSF "out.wav" i a

Write 440hz sine wave for 3 seconds to monaural file \"sin440.wav\".

> sin440 :: IO ()
> sin440 = do
>   let dur = 3; freq = 440; sr = 48000
>       hdr = wav16 {samplerate = sr, frames = sr * dur}
>       sig = fromFunction (Z :. 1 :. dur * sr) $ \(_ :. _ :. i) ->
>         sin (fromIntegral i * freq * pi * 2 / fromIntegral sr)
>   sig' <- computeP sig :: IO (Array F DIM2 Double)
>   writeSF "sin440.wav" hdr sig'

Write 440hz sine wave to channel 0, 880hz sine wave to channel 1, for 3 seconds
to stereo file \"sin440and880.wav\".

> sin440and880 :: IO ()
> sin440and880 = do
>     let dur = 3; freq1 = 440; freq2 = 880; sr = 480000
>         hdr = wav16 {samplerate = sr, channels = 2, frames = sr * dur * 2}
>         gen f i = sin (fromIntegral i * f * pi * 2 / fromIntegral sr)
>         sig = fromFunction (Z :. 2 :. dur * sr) $ \(_ :. c :. i) ->
>             case c of
>                 0 -> gen freq1 i
>                 1 -> gen freq2 i
>                 _ -> 0
>     sig' <- computeP sig :: IO (Array F DIM2 Double)
>     writeSF "sin440and880.wav" hdr sig'

-}

-- ---------------------------------------------------------------------------
-- Wrapper actions

-- | Read sound file from given path.
--
-- Returns a tuple of Info and array containing the samples of sound
-- file.  Returned pair contains sound file information and array which
-- is indexed with channel number and frame.  Info could used later for
-- writing sound file.
--
readSF ::
  forall a r. (Sample a, Source r a, Target r a, Buffer (Array F DIM1) a)
  => FilePath -> IO (Info, Array r DIM2 a)
readSF path = do
  (info, arr) <- S.readFile path :: IO (Info, Maybe (Array F DIM1 a))
  case arr of
    Nothing   -> error $ "readSF: failed reading " ++ path
    Just arr' -> do
      arr'' <- toMC (S.channels info) arr'
      return (info, arr'')

{-# INLINEABLE readSF #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Double) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Float) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Int16) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Int32) #-}

-- | Write array contents to sound file with given header information.
--
-- Expecting an array indexed with channel and frame, as returned from readSF.
-- i.e. 2-dimensional array with its contents indexed with channel.
--
writeSF ::
  forall a r.
  (Sample a, Source r a, Buffer (Array r DIM1) a, Target r a)
  => FilePath -> Info -> Array r DIM2 a -> IO ()
writeSF path info arr = do
  arr' <- fromMC arr :: IO (Array r DIM1 a)
  _ <- S.writeFile info path arr'
  return ()

{-# INLINEABLE writeSF #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Double -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Float -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Int16 -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Int32-> IO () #-}

-- | Wrapper for invoking array with reading sound file.
--
-- Performs given action using sound file info and samples as arguments.
--
withSF
  :: forall a b r. (Sample a, Target r a, Source r a)
  => FilePath -> (Info -> Array r DIM2 a -> IO b) -> IO b
withSF path act = do
  (info, arr) <- S.readFile path :: IO (Info, Maybe (Array F DIM1 a))
  case arr of
    Nothing   -> error ("withSF: failed to read " ++ path)
    Just arr' -> do
      arr'' <- toMC (S.channels info) arr' :: IO (Array r DIM2 a)
      act info arr''

{-# INLINEABLE withSF #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Double -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Float -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Int16 -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Int32 -> IO b) -> IO b #-}


-- ---------------------------------------------------------------------------
-- Internal work

-- | Orphan instance for reading/wriging sound file to array via ForeignPtr.
--
instance Sample e => Buffer (Array F DIM1) e where

  -- Read the whole contents to DIM1 array, ignoring channel number.
  --
  fromForeignPtr fptr _ count = return $ RF.fromForeignPtr (Z :. count) fptr

  {-# INLINEABLE fromForeignPtr #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Double -> Int -> Int -> IO (Array F DIM1 Double) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Float -> Int -> Int -> IO (Array F DIM1 Float) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Int16 -> Int -> Int -> IO (Array F DIM1 Int16) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Int32 -> Int -> Int -> IO (Array F DIM1 Int32) #-}

  -- Allocate whole memory for writing, fill in with element of array.
  --
  toForeignPtr arr = do
    let nelem = R.size (R.extent arr)
        fptr = RF.toForeignPtr arr
    return (fptr, 0, nelem)

  {-# INLINEABLE toForeignPtr #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Double -> IO (ForeignPtr Double, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Float -> IO (ForeignPtr Float, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Int16 -> IO (ForeignPtr Int16, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Int32 -> IO (ForeignPtr Int32, Int, Int) #-}


-- | Converts multi channel signal to vector signal.
fromMC ::
  (Source r1 e, Source r2 e, Target r2 e, Monad m)
  => Array r1 DIM2 e -> m (Array r2 DIM1 e)
fromMC arr = R.computeP $ R.backpermute sh' f arr where
  sh' = Z :. (nc * nf)
  {-# INLINE sh' #-}
  _ :. nc :. nf = R.extent arr
  f (Z :. i) = Z :. i `mod` nc :. i `div` nc
  {-# INLINE f #-}
{-# INLINE fromMC #-}


-- | Converts vector signal to multi channel signal.
toMC ::
  (Monad m, Source r1 e, Source r2 e, Target r2 e)
  => Int -> Array r1 DIM1 e -> m (Array r2 DIM2 e)
toMC nc arr = R.computeP $ R.backpermute sh' f arr where
  sh' = Z :. nc :. (nf `div` nc)
  _ :. nf = R.extent arr
  f (Z :. i :. j) = Z :. i + (j * nc)
{-# INLINE toMC #-}

-- | 16 bit MS wave, single channel, sampling rate = 48000.
wav16 :: S.Info
wav16 = S.Info
  { samplerate = 48000
  , channels = 1
  , frames = 0
  , format = S.Format S.HeaderFormatWav S.SampleFormatPcm16 S.EndianFile
  , sections = 1
  , seekable = True }
{-# INLINE wav16 #-}

-- | 32 bit MS wave, single channel, sampling rate = 48000.
wav32 :: S.Info
wav32 = wav16
  { format = S.Format S.HeaderFormatWav S.SampleFormatPcm32 S.EndianFile }
{-# INLINE wav32 #-}
