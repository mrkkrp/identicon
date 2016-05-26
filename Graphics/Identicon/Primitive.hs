-- |
-- Module      :  Graphics.Identicon.Primitive
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Various primitives and combinators that help you write code for your
-- identicon. Filling combinators is where you start. They create color
-- layers that occupy all available space. If you want to limit layer in
-- size, specify where this smaller part should be, take a look at the
-- “Position, size, and shape” section. It also contains a 'circle'
-- combinator that limits given filling is such a way that it forms a
-- circle. Finally, we have combinators that add symmetry to your icon and
-- other auxiliary functions.
--
-- TODO Some examples here may help.

module Graphics.Identicon.Primitive
  ( -- * Filling
    black
  , color
  , gradientLR
  , gradientTB
  , gradientTLBR
  , gradientTRBL
    -- * Position, size, and shape
  , circle
    -- * Symmetry
  , hsym
  , vsym
  , hvsym
    -- * Other
  , oneof )
where

import Codec.Picture
import Data.Word (Word8)
import Graphics.Identicon

----------------------------------------------------------------------------
-- Filling

-- | Black is a special color, it means absence of light.

black :: PixelRGB8
black = PixelRGB8 0 0 0

-- | Layer filled with given color.

color :: PixelRGB8 -> Layer
color a = Layer $ \_ _ _ _ -> a
{-# INLINE color #-}

-- | Gradient changing from left to right.

gradientLR
  :: PixelRGB8         -- ^ Left color
  -> PixelRGB8         -- ^ Right color
  -> Layer
gradientLR a b = Layer $ \w _ x _ ->
  mixWith (const $ ξ x w) a b
{-# INLINE gradientLR #-}

-- | Gradient changing from top to bottom.

gradientTB
  :: PixelRGB8         -- ^ Top color
  -> PixelRGB8         -- ^ Bottom color
  -> Layer
gradientTB a b = Layer $ \_ h _ y ->
  mixWith (const $ ξ y h) a b
{-# INLINE gradientTB #-}

-- | Gradient changing from top left corner to bottom right corner.

gradientTLBR
  :: PixelRGB8         -- ^ Top left color
  -> PixelRGB8         -- ^ Bottom right color
  -> Layer
gradientTLBR a b = Layer $ \w h x y ->
  mixWith (const $ ξ (x + y) (w + h)) a b
{-# INLINE gradientTLBR #-}

-- | Gradient changing from top right corner to bottom left corner.

gradientTRBL
  :: PixelRGB8         -- ^ Top right color
  -> PixelRGB8         -- ^ Bottom left color
  -> Layer
gradientTRBL a b = Layer $ \w h x y ->
  mixWith (const $ ξ (w - x + y) (w + h)) a b
{-# INLINE gradientTRBL #-}

-- TODO Do we need more gradient combinators or these in combination with
-- symmetry combinators will do?

-- | A gradient helper function.

ξ
  :: Int               -- ^ Actual value of coordinate
  -> Int               -- ^ Maximum value of coordinate
  -> Word8             -- ^ Color at the beginning of the range
  -> Word8             -- ^ Color at the end of the range
  -> Word8             -- ^ Resulting color
ξ v l x y = floor $ (1 - n) * fromIntegral x + n * fromIntegral y
  where
    n :: Float
    n = fromIntegral v / fromIntegral l
{-# INLINE ξ #-}

----------------------------------------------------------------------------
-- Position, size, and shape

-- | Limit given layer so it forms a circle.

circle :: Layer -> Layer
circle l = Layer $ \w h x y ->
  let w', h', v, r0, r1 :: Float
      w' = fromIntegral w
      h' = fromIntegral h
      sqr a = a * a
      v = sqr (fromIntegral x - w' / 2) + sqr (fromIntegral y - h' / 2)
      r0 = min w' h' / 2
      r1 = sqr r0
      β = 2.0 * r0
      δ = (r1 - v) / β
      τ = floor . (* δ) . fromIntegral
      ~px@(PixelRGB8 r g b) = unLayer l w h x y
      e | v <  r1 - β = px
        | v <= r1     = PixelRGB8 (τ r) (τ g) (τ b)
        | otherwise   = black
  in e
{-# INLINE circle #-}

----------------------------------------------------------------------------
-- Symmetry

-- | Add horizontal symmetry to a layer.

hsym :: Layer -> Layer
hsym l = Layer $ \w h x y ->
  let w' = w `quot` 2
  in unLayer l w' h (if x > w' then w - x else x) y
{-# INLINE hsym #-}

-- | Add vertical symmetry to a layer.

vsym :: Layer -> Layer
vsym l = Layer $ \w h x y ->
  let h' = h `quot` 2
  in unLayer l w h' x (if y > h' then h - y else y)
{-# INLINE vsym #-}

-- | Add horizontal and vertical symmetry to layer. Result is an image with
-- four mirrored repetitions of the same figure.

hvsym :: Layer -> Layer
hvsym l = Layer $ \w h x y ->
  let h' = h `quot` 2
      w' = w `quot` 2
  in unLayer l w' h' (if x > w' then w - x else x)
                     (if y > h' then h - y else y)
{-# INLINE hvsym #-}

----------------------------------------------------------------------------
-- Other

-- | Select one of provided alternatives given a number.

oneof :: Integral n => [a] -> n -> a
oneof xs n = xs !! (fromIntegral n `rem` length xs)
