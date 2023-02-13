{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Graphics.Identicon
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Core types and definitions for flexible generation of identicons. Please
-- see the "Graphics.Identicon.Primitive" module for a collection of
-- building blocks to code layers of your identicon.
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
--
-- @myGenerator@ takes the desired width, height, and a hash that should
-- have at least 4 bytes in it and returns an identicon corresponding to
-- that hash or 'Nothing' if the hash has less than 4 bytes in it or when
-- width\/height don't make sense. The identicon has randomly placed circle
-- with gradient filling changing (horizontally) from black to some color
-- and back to black. The circle is mirrored 4 times, and every repetition
-- is rotated by 90°. This identicon consumes 4 bytes and has one layer.
module Graphics.Identicon
  ( -- * Basic types
    Identicon (..),
    Consumer,
    (:+) (..),
    Layer (..),
    BytesAvailable,
    BytesConsumed,
    Implementation,
    ToLayer,

    -- * Identicon rendering
    Renderable (..),
    ApplyBytes (..),
    renderIdenticon,
  )
where

import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Semigroup as S
import Data.Word (Word8)
import GHC.TypeLits

----------------------------------------------------------------------------
-- Basic types

-- | 'Identicon' is a type that represents an identicon consisting of zero
-- layers. The type is parametrized over a phantom type @n@ which is a
-- natural number on the type level that represents the number of bytes that
-- should be provided to generate this type of identicon. Bytes typically
-- come from some sort of hash that has a fixed size.
data Identicon (n :: Nat) = Identicon

-- | 'Consumer' is a type that represents an entity that consumes bytes that
-- are available for identicon generation. It's parametrized over a phantom
-- type @n@ which is a natural number on the type level that represents the
-- number of bytes that this entity consumes. At this moment, a 'Consumer'
-- always adds one 'Layer' to an 'Identicon' when attached to it. The number
-- of bytes, specified as type parameter of 'Identicon' type must be
-- completely consumed by a collection of consumers attached to it. To
-- attach a consumer to 'Identicon', you use the @(':+')@ type operator, see
-- below.
data Consumer (n :: Nat)

-- | The @(':+')@ type operator is used to attach 'Consumer's to
-- 'Identicon', thus adding layers to it and exhausting the bytes that are
-- available for identicon generation. An example of identicon that can be
-- generated from 16 byte hash is shown below:
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
-- generated as a “superposition” of several 'Layers'.
newtype Layer = Layer
  {unLayer :: Int -> Int -> Int -> Int -> PixelRGB8}

instance S.Semigroup Layer where
  Layer a <> Layer b = Layer (\w h -> mixPixels (a w h) (b w h))

instance Monoid Layer where
  mempty = Layer $ \_ _ _ _ -> PixelRGB8 0 0 0
  mappend = (S.<>)

-- | The 'BytesAvailable' type function calculates how many bytes are
-- available for consumption in a given identicon.
type family BytesAvailable a :: Nat where
  BytesAvailable (Identicon n) = n
  BytesAvailable (x :+ y) = BytesAvailable x

-- | The 'BytesConsumed' type function calculates how many bytes are
-- consumed in a given identicon.
type family BytesConsumed a :: Nat where
  BytesConsumed (Identicon n) = 0
  BytesConsumed (Consumer n) = n
  BytesConsumed (x :+ y) = BytesConsumed x + BytesConsumed y

-- | The 'Implementation' type function returns the type of the code which
-- can implement the given identicon.
type family Implementation a where
  Implementation (Identicon n) = Identicon n
  Implementation (a :+ Consumer n) = Implementation a :+ ToLayer n

-- | The 'ToLayer' type function calculates type that a layer-producing
-- function should have to consume the given number of bytes @n@.
type family ToLayer (n :: Nat) :: k where
  ToLayer 0 = Layer
  ToLayer n = Word8 -> ToLayer (n - 1)

----------------------------------------------------------------------------
-- Identicon rendering

-- | Identicons that can be rendered as an image implement this class.
class Renderable a where
  render ::
    -- | A 'Proxy' clarifying identicon type
    Proxy a ->
    -- | Corresponding implementation
    Implementation a ->
    -- | Width in pixels
    Int ->
    -- | Height in pixels
    Int ->
    -- | Bytes to consume
    ByteString ->
    -- | The rest of bytes and producing function
    (ByteString, Int -> Int -> PixelRGB8)

instance Renderable (Identicon n) where
  render _ _ _ _ bs = (bs, \_ _ -> PixelRGB8 0 0 0)

instance
  (Renderable a, ApplyBytes (ToLayer n)) =>
  Renderable (a :+ Consumer n)
  where
  render _ (a :+ b) weight height bs0 =
    let (bs1, x) = render (Proxy :: Proxy a) a weight height bs0
        (bs2, y) = applyBytes b bs1
     in (bs2, mixPixels x (unLayer y weight height))

-- | Combine results of two rending functions.
mixPixels ::
  (Int -> Int -> PixelRGB8) ->
  (Int -> Int -> PixelRGB8) ->
  Int ->
  Int ->
  PixelRGB8
mixPixels a b x y = mixWith (const saturatedAddition) (a x y) (b x y)
{-# INLINE mixPixels #-}

-- | An implementation of saturated addition for bytes.
saturatedAddition :: Word8 -> Word8 -> Word8
saturatedAddition x y =
  let z = x + y
   in if z < x then 0xff else z
{-# INLINE saturatedAddition #-}

-- | Consume bytes from a strict 'ByteString' and apply them to a function
-- that takes 'Word8' until it produces a 'Layer'.
class ApplyBytes a where
  applyBytes ::
    -- | Function that produces a layer
    a ->
    -- | Bytes to consume
    ByteString ->
    -- | The rest of 'ByteString' and produced 'Layer'
    (ByteString, Layer)

instance ApplyBytes Layer where
  applyBytes f bs = (bs, f)

instance (ApplyBytes f) => ApplyBytes (Word8 -> f) where
  applyBytes f bs =
    let (b, bs') = fromJust (B.uncons bs)
     in applyBytes (f b) bs'

-- | Render an identicon. The function returns 'Nothing' if given
-- 'ByteString' is too short or when width or height is lesser than 1.
renderIdenticon ::
  forall a.
  ( Renderable a,
    KnownNat (BytesAvailable a),
    BytesAvailable a ~ BytesConsumed a
  ) =>
  -- | Type that defines an identicon
  Proxy a ->
  -- | Implementation that generates layers
  Implementation a ->
  -- | Width in pixels
  Int ->
  -- | Height in pixels
  Int ->
  -- | Collection of bytes to use, should be long enough
  ByteString ->
  -- | Rendered identicon, or 'Nothing' if there is not enough bytes
  Maybe (Image PixelRGB8)
renderIdenticon proxy impl width height bs =
  if B.length bs < fromIntegral (natVal (Proxy :: Proxy (BytesAvailable a)))
    || width < 1
    || height < 1
    then Nothing
    else
      Just $
        generateImage
          (snd $ render proxy impl width height bs)
          width
          height
{-# NOINLINE renderIdenticon #-}
