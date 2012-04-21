{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Read and write audio file with repa arrays using libsndfile via hsndfile.
Note that this module re-exports header related types from hsndfile.

For more info about supported format, visit libsndfile web site.

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

    -- * Util
  , toMC
  , fromMC
  , wav16
  , wav32

    -- * References
    -- $references

  ) where

import Data.Word (Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr)

import Data.Array.Repa
  (Array, DIM1, DIM2, Z(..), (:.)(..), Repr)
import Data.Array.Repa.Eval (Fillable)
import Data.Array.Repa.Repr.ForeignPtr (F)
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

Write 440hz sine wav for 3 seconds to \"sin440.wav\".

> sin440 :: IO ()
> sin440 = do
>   let dur = 3; freq = 440; sr = 48000
>       hdr = wav16 {samplerate = sr, frames = sr * dur}
>       sig = fromFunction (Z :. 1 :. dur * sr) $ \(_ :. _ :. i) ->
>         sin (fromIntegral i * freq * pi * 2 / fromIntegral sr)
>   sig' <- computeP sig :: IO (Array F DIM2 Double)
>   writeSF "sin440.wav" hdr sig'

-}

{-$references

* libsndfile: <http://www.mega-nerd.com/libsndfile/>

* hsndfile <http://haskell.org/haskellwiki/Hsndfile>

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
  forall a r. (Sample a, Fillable r a, Repr r a)
  => FilePath -> IO (Info, Array r DIM2 a)
readSF path = do
  (info, arr) <- S.readFile path :: IO (Info, Maybe (Array F DIM1 a))
  case arr of
    Nothing   -> error $ "readSF: failed reading " ++ path
    Just arr' -> do
      arr'' <- toMC (S.channels info) arr'
      return (info, arr'')

{-# INLINE readSF #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Double) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Float) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Word16) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array F DIM2 Word32) #-}

-- | Write array contents to sound file with given header information.
--
-- Expecting an array indexed with channel and frame, as returned from readSF.
-- i.e. 2-dimensional array with its contents indexed with channel.
--
writeSF ::
  forall a r.
  ( Sample a, Repr r a, Buffer (Array r DIM1) a
  , Fillable r a )
  => FilePath -> Info -> Array r DIM2 a -> IO ()
writeSF path info arr = do
  arr' <- fromMC arr :: IO (Array r DIM1 a)
  S.writeFile info path arr'
  return ()

{-# INLINE writeSF #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Double -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Float -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Word16 -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array F DIM2 Word32-> IO () #-}

-- | Wrapper for invoking array with reading sound file.
--
-- Performs given action using sound file info and samples as arguments.
--
withSF
  :: forall a b r. (Sample a, Fillable r a, Repr r a)
  => FilePath -> (Info -> Array r DIM2 a -> IO b) -> IO b
withSF path act = do
  (info, arr) <- S.readFile path :: IO (Info, Maybe (Array F DIM1 a))
  case arr of
    Nothing   -> error ("withSF: failed to read " ++ path)
    Just arr' -> do
      arr'' <- toMC (S.channels info) arr' :: IO (Array r DIM2 a)
      act info arr''

{-# INLINE withSF #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Double -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Float -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Word16 -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array F DIM2 Word32 -> IO b) -> IO b #-}

-- ---------------------------------------------------------------------------
-- Internal work

-- | Orphan instance for reading/wriging sound file to array via ForeignPtr.
--
instance (Sample e) => Buffer (Array F DIM1) e where

  -- Read the whole contents to DIM1 array, ignoring channel number.
  --
  fromForeignPtr fptr _ count = return $ RF.fromForeignPtr (Z :. count) fptr

  {-# INLINE fromForeignPtr #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Double -> Int -> Int -> IO (Array F DIM1 Double) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Float -> Int -> Int -> IO (Array F DIM1 Float) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Word16 -> Int -> Int -> IO (Array F DIM1 Word16) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Word32 -> Int -> Int -> IO (Array F DIM1 Word32) #-}

  -- Allocate whole memory for writing, fill in with element of array.
  --
  toForeignPtr arr = do
    let nelem = R.size (R.extent arr)
        fptr = RF.toForeignPtr arr
    return (fptr, 0, nelem)

  {-# INLINE toForeignPtr #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Double -> IO (ForeignPtr Double, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Float -> IO (ForeignPtr Float, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Word16 -> IO (ForeignPtr Word16, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array F DIM1 Word32 -> IO (ForeignPtr Word32, Int, Int) #-}

-- | Converts multi channel signal to vector signal.
-- fromMC :: Elt a => Array DIM2 a -> Array DIM1 a
fromMC ::
  (Repr r1 e, Repr r2 e, Fillable r2 e, Monad m)
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
  (Repr r1 e, Repr r2 e, Fillable r2 e, Monad m)
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
