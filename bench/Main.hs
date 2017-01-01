--
-- Benchmarks for the ‘identicon’ package.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import Codec.Picture
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Word (Word8)
import Graphics.Identicon
import Graphics.Identicon.Primitive
import System.Random
import System.Random.TF.Init
import qualified Data.ByteString as B

main :: IO ()
main = defaultMain
  [ bgen 0  0 gen0
  , bgen 4  1 gen1
  , bgen 8  2 gen2
  , bgen 12 3 gen3 ]

-- | Run an identicon benchmark given its name and rendering function.

bgen
  :: Int               -- ^ Number of bytes the generator expects
  -> Int               -- ^ Number of layers the generator has
  -> (Int -> Int -> ByteString -> Maybe (Image PixelRGB8)) -- ^ Generator
  -> Benchmark         -- ^ Benchmark
bgen bytes layers gen = bgroup groupName (f <$> testSizes)
  where
    groupName = show bytes ++ " bytes/" ++ show layers ++ " layers"
    f n       =
      let n' = show n
      in env (getBS bytes) (bench (n' ++ " × " ++ n') . nf (gen n n))

-- | Obtain a quite random 'ByteString' of specified length.

getBS :: Int -> IO ByteString
getBS n = do
  gen <- initTFGen
  (return . B.pack . take n . randoms) gen

-- | We render a series of rectangular icons in 'bgen', this list contains
-- size of a side of icon in pixels.

testSizes :: [Int]
testSizes = [16,32,64,128,256,512,1024]

----------------------------------------------------------------------------
-- Identicon generators

type Gen0 = Identicon 0

gen0 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen0 = renderIdenticon (Proxy :: Proxy Gen0) Identicon

type Gen1 = Identicon 4 :+ Consumer 4

gen1 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen1 = renderIdenticon (Proxy :: Proxy Gen1) i
  where
    i = Identicon :+ stdLayer

type Gen2 = Identicon 8 :+ Consumer 4 :+ Consumer 4

gen2 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen2 = renderIdenticon (Proxy :: Proxy Gen2) i
  where
    i = Identicon :+ stdLayer :+ stdLayer

type Gen3 = Identicon 12 :+ Consumer 4 :+ Consumer 4 :+ Consumer 4

gen3 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen3 = renderIdenticon (Proxy :: Proxy Gen3) i
  where
    i = Identicon :+ stdLayer :+ stdLayer :+ stdLayer

stdLayer :: Pixel8 -> Pixel8 -> Pixel8 -> Word8 -> Layer
stdLayer r g b n = rsym $ onGrid 4 4 n $
  circle $ gradientLR (edge . mid) black (PixelRGB8 r g b)
