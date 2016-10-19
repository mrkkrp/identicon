-- |
-- Module      :  Graphics.Identicon
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Core types and definitions for flexible generation of identicons. Please
-- see the "Graphics.Identicon.Primitive" module for collection of building
-- blocks to code layers of your identicon.
--
-- A basic complete example looks like this:
--
-- > import Codec.Picture
-- > import Data.ByteString (ByteString)
-- > import Data.Proxy
-- > import Data.Word (Word8)
-- > import Graphics.Identicon
-- > import Graphics.Identicon.Primitive
-- >
-- > myImageType :: Proxy (Identicon 4 :+ Consumer 4)
-- > myImageType = Proxy
-- >
-- > myImpl = Identicon :+ a
-- >   where
-- >     a :: Word8 -> Word8 -> Word8 -> Word8 -> Layer
-- >     a r g b n = rsym $ onGrid 6 6 n $
-- >       circle $ gradientLR (edge . mid) black (PixelRGB8 r g b)
-- >
-- > myGenerator :: Int -> Int -> ByteString -> Maybe (Image PixelRGB8)
-- > myGenerator = renderIdenticon myImageType myImpl
-- >
-- @myGenerator@ takes desired width, height, and hash that should have at
-- least 4 bytes in it and returns an identicon corresponding to that hash
-- or 'Nothing' if the hash has less than 4 bytes in it or width or height
-- don't make sense. The identicon has randomly placed circle with gradient
-- filling changing (horizontally) from black to some color and back to
-- black. The circle is mirrored 4 times, and every repetition is rotated by
-- 90°. This identicon consumes 4 bytes and has one layer.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Identicon
  ( -- * Basic types
    Identicon (..)
  , Consumer
  , (:+) (..)
  , Layer (..)
  , BytesAvailable
  , BytesConsumed
  , Implementation
  , ToLayer
    -- * Identicon rendering
  , Renderable (..)
  , ApplyBytes (..)
  , renderIdenticon )
where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Word (Word8)
import GHC.TypeLits
import qualified Data.ByteString as B

----------------------------------------------------------------------------
-- Basic types

-- | 'Identicon' is a type that represents an identicon consisting of zero
-- layers. The type is parametrized over the phantom type @n@ which is a
-- natural number on type level that represents the number of bytes that
-- should be provided to generate this type of identicon. Bytes typically
-- come from some sort of hash that has fixed size.

data Identicon (n :: Nat) = Identicon

-- | 'Consumer' is a type that represents an entity that consumes bytes that
-- are available for identicon generation. It's parametrized over the
-- phantom type @n@ which is a natural number on type level that represents
-- the number of bytes that this entity consumes. At this moment, a
-- 'Consumer' always adds one 'Layer' to identicon when attached to it. The
-- number of bytes, specified as type parameter of 'Identicon' type must be
-- completely consumed by a collection of consumers attached to it. To
-- attach a consumer to 'Identicon', you use the ':+' type operator, see
-- below.

data Consumer (n :: Nat)

-- | The ':+' type operator is used to attach 'Consumer's to 'Identicon',
-- thus adding layers to it and exhausting bytes that are available for
-- identicon generation. An example of identicon that can be generated from
-- 16 byte hash is shown below:
--
-- > type Icon = Identicon 16 :+ Consumer 5 :+ Consumer 5 :+ Consumer 6
--
-- The identicon above has three layers.

infixl 8 :+
data a :+ b = a :+ b

-- | 'Layer' is the basic building block of an identicon. It's a function
-- that takes the following arguments (in order):
--
--     * Width of identicon
--     * Height of identicon
--     * Position on X axis
--     * Position on Y axis
--
-- …and returns a 'PixelRGB8' value. In this library, an identicon is
-- generated as “superposition” of several 'Layers'.

newtype Layer = Layer
  { unLayer :: Int -> Int -> Int -> Int -> PixelRGB8 }

-- | The 'BytesAvailable' type function calculates how many bytes available
-- for consumption in a given identicon.

type family BytesAvailable a :: Nat where
  BytesAvailable (Identicon n) = n
  BytesAvailable (x :+ y)      = BytesAvailable x

-- | The 'BytesConsumed' type function calculates how many bytes is consumed
-- in a given identicon.

type family BytesConsumed a :: Nat where
  BytesConsumed (Identicon n) = 0
  BytesConsumed (Consumer  n) = n
  BytesConsumed (x :+ y)      = BytesConsumed x + BytesConsumed y

-- | The 'Implementation' type function returns type of the code which can
-- implement the given identicon.

type family Implementation a where
  Implementation (Identicon n)     = Identicon n
  Implementation (a :+ Consumer n) = Implementation a :+ ToLayer n

-- | The 'ToLayer' type function calculates type that layer-producing
-- function should have to consume given number of bytes @n@.

type family ToLayer (n :: Nat) :: k where
  ToLayer 0 = Layer
  ToLayer n = Word8 -> ToLayer (n - 1)

----------------------------------------------------------------------------
-- Identicon rendering

-- | Identicons that can be rendered as an image implement this class.

class Renderable a where
  render
    :: Proxy a         -- ^ A 'Proxy' clarifying identicon type
    -> Implementation a -- ^ Corresponding implementation
    -> Int             -- ^ Width in pixels
    -> Int             -- ^ Height in pixels
    -> ByteString      -- ^ Bytes to consume
    -> (ByteString, Int -> Int -> PixelRGB8)
       -- ^ The rest of bytes and producing function

instance Renderable (Identicon n) where
  render _ _ _ _ bs = (bs, \_ _ -> PixelRGB8 0 0 0)

instance (Renderable a, ApplyBytes (ToLayer n))
    => Renderable (a :+ Consumer n) where
  render _ (a :+ b) weight height bs0 =
    let (bs1, x) = render (Proxy :: Proxy a) a weight height bs0
        (bs2, y) = applyBytes b bs1
    in (bs2, mixPixels x (unLayer y weight height))

-- | Combine results of two rending functions.

mixPixels
  :: (Int -> Int -> PixelRGB8)
  -> (Int -> Int -> PixelRGB8)
  ->  Int -> Int -> PixelRGB8
mixPixels a b x y = mixWith (const saturatedAddition) (a x y) (b x y)
{-# INLINE mixPixels #-}

-- | An implementation of saturated addition for bytes. This is a reasonably
-- efficient thing.

saturatedAddition :: Word8 -> Word8 -> Word8
saturatedAddition x y = let z = x + y in
  if z < x then 0xff else z
{-# INLINE saturatedAddition #-}

-- | Consume bytes from strict 'ByteString' and apply them to a function
-- that takes 'Word8' until it produces a 'Layer'.

class ApplyBytes a where
  applyBytes
    :: a               -- ^ Function that produces a layer
    -> ByteString      -- ^ Bytes to consume
    -> (ByteString, Layer) -- ^ The rest of 'ByteString' and produced 'Layer'

instance ApplyBytes Layer where
  applyBytes f bs = (bs, f)

instance ApplyBytes f => ApplyBytes (Word8 -> f) where
  applyBytes f bs =
    let (b,bs') = fromJust (B.uncons bs)
    in applyBytes (f b) bs'

-- | Render an identicon. The function returns 'Nothing' if given
-- 'ByteString' is too short or when width or height is lesser than 1.

renderIdenticon :: forall a.
     ( Renderable a
     , KnownNat (BytesAvailable a)
     , BytesAvailable a ~ BytesConsumed a )
  => Proxy a           -- ^ Type that defines an identicon
  -> Implementation a  -- ^ Implementation that generates layers
  -> Int               -- ^ Width in pixels
  -> Int               -- ^ Height in pixels
  -> ByteString        -- ^ Collection of bytes to use, should be long enough
  -> Maybe (Image PixelRGB8)
     -- ^ Rendered identicon, or 'Nothing' if there is not enough bytes
renderIdenticon proxy impl width height bs =
  if B.length bs < fromIntegral (natVal (Proxy :: Proxy (BytesAvailable a)))
     || width  < 1
     || height < 1
    then Nothing
    else Just $ generateImage
         (snd $ render proxy impl width height bs) width height
