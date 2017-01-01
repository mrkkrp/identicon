--
-- Tests for the ‘identicon’ package.
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
import Control.Monad
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Proxy
import Data.Word (Word8)
import Graphics.Identicon
import Graphics.Identicon.Primitive
import Test.Hspec
import qualified Data.ByteString as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "renderIdenticon" renderIdenticonSpec
  ω gen0 [0x00,0x00,0x00,0x00] "data-examples/identicon-00.png"
  ω gen0 [0x8f,0x55,0x6e,0x93] "data-examples/identicon-01.png"
  ω gen0 [0x31,0xa8,0x29,0x5b] "data-examples/identicon-02.png"
  ω gen1 [0x3e,0xf1,0xde,0x08,0x85,0x0b,0x9c,0x81,0x25,0xf0,0x53,0x0b]
    "data-examples/identicon-10.png"
  ω gen1 [0xa9,0xf7,0x66,0xf0,0xd7,0xf9,0xb0,0x8e,0x57,0x21,0xc5,0x06]
    "data-examples/identicon-11.png"
  ω gen1 [0x23,0x29,0x2d,0x29,0x2f,0x05,0x28,0x11,0x1e,0x0e,0x0d,0x06]
    "data-examples/identicon-12.png"
  ω gen2 [0xcf,0xe7,0xb9,0x49,0x93,0xb1,0x01]
    "data-examples/identicon-20.png"
  ω gen2 [0xc8,0xa4,0xda,0xa1,0xe9,0x93,0x86]
    "data-examples/identicon-21.png"
  ω gen2 [0xf9,0x9b,0xb7,0x11,0x5b,0xca,0x00]
    "data-examples/identicon-22.png"

renderIdenticonSpec :: Spec
renderIdenticonSpec = do
  context "when we pass too short byte string" $
    it "returns Nothing" $
      shouldBeNothing (gen0 100 100 "aaa")
  context "when we pass nonsential width value" $
    it "returns Nothing" $
      shouldBeNothing (gen0 0 100 "aaaa")
  context "when we pass nonsential height value" $
    it "returns Nothing" $
      shouldBeNothing (gen0 100 0 "aaaa")

----------------------------------------------------------------------------
-- Identicon generators

type Gen0 = Identicon 4 :+ Consumer 4

gen0 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen0 = renderIdenticon (Proxy :: Proxy Gen0) i
  where
    i = Identicon :+ a
    a r g b n = rsym $ onGrid 4 4 n $
      circle $ gradientLR (edge . mid) black (PixelRGB8 r g b)

type Gen1 = Identicon 12 :+ Consumer 4 :+ Consumer 4 :+ Consumer 4

gen1 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen1 = renderIdenticon (Proxy :: Proxy Gen1) i
  where
    i = Identicon :+ a0 :+ a1 :+ a2
    a0 r g b n = hsym $ onGrid 3 3 n $
      gradientTLBR id black (PixelRGB8 r g b)
    a1 r g b n = vsym $ onGrid 4 4 n $
      gradientXY id black (PixelRGB8 r g b)
    a2 r g b n = hvsym $ onGrid 5 5 n $
      circle $ gradientTRBL mid (PixelRGB8 r g b) black

type Gen2 = Identicon 7 :+ Consumer 3 :+ Consumer 4

gen2 :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
gen2 = renderIdenticon (Proxy :: Proxy Gen2) i
  where
    i = Identicon :+ a0 :+ a1
    a0 r g b = gradientTB edge (PixelRGB8 r g b) black
    a1 r g b n = oneof [gradientXY id black, color] n (PixelRGB8 r g b)

----------------------------------------------------------------------------
-- Helpers

-- | A helper to check that 'Nothing' is returned without requiring that
-- argument is an instance of 'Show' or 'Eq' type class.

shouldBeNothing :: Maybe a -> Expectation
shouldBeNothing m =
  case m of
    Nothing -> return ()
    Just _  ->
      expectationFailure "it returned not Nothing"

-- | A shorthand for test cases.

ω
  :: (Int -> Int -> ByteString -> Maybe (Image PixelRGB8))
     -- ^ Identicon generator
  -> [Word8]           -- ^ Input to use for identicon generation
  -> FilePath          -- ^ Where to get image to compare with
  -> Spec
ω f bs path = describe path $ it ("reproduces " ++ path) $
  compareWithFile f (B.pack bs) path

-- | Take function that produces identicon, binary input for it, path to
-- already rendered identicon and compare them. Fail with informative
-- message if they differ.

compareWithFile
  :: (Int -> Int -> ByteString -> Maybe (Image PixelRGB8))
     -- ^ Identicon generator
  -> ByteString        -- ^ Input to use for identicon generation
  -> FilePath          -- ^ Where to get image to compare with
  -> Expectation
compareWithFile f bs path = do
  (Right (ImageRGB8 img)) <- readImage path
  let mimg = f (imageWidth img) (imageHeight img) bs
  case mimg of
    Nothing -> expectationFailure "failed to generate an image"
    Just img' ->
      unless (imageEq img img') $
        expectationFailure ("generated image is different from " ++ path)

-- | Since 'Image' for some reason is not an instance of 'Eq', we use this
-- to compare 'Image's.

imageEq :: Image PixelRGB8 -> Image PixelRGB8 -> Bool
imageEq a b =
  ((==) `on` imageWidth)  a b &&
  ((==) `on` imageHeight) a b &&
  ((==) `on` imageData)   a b
