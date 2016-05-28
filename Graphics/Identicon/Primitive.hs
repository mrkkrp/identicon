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
-- identicon. Filling functions is where you start. They create color layers
-- that occupy all available space. If you want to limit a layer in size,
-- specify where this smaller part should be, take a look at the “Position,
-- size, and shape” section. It also contains a 'circle' combinator that
-- limits given filling is such a way that it forms a circle. Finally, we
-- have combinators that add symmetry to layers and other auxiliary
-- functions.
--
-- As a starting point, here is the function that generates a circle with
-- gradient filling changing from black (on the left hand side) to some
-- color (on the right hand side):
--
-- > f :: Word8 -> Word8 -> Word8 -> Layer
-- > f r g b = circle $ gradientLR id black (PixelRGB8 r g b)
--
-- The function consumes 3 bytes from hash when it's used in identicon.

module Graphics.Identicon.Primitive
  ( -- * Filling
    black
  , color
  , gradientLR
  , gradientTB
  , gradientTLBR
  , gradientTRBL
  , gradientXY
    -- ** Gradient transforming functions
    -- $gtrans
  , mid
  , edge
    -- * Position, size, and shape
  , onGrid
  , circle
    -- * Symmetry
  , hsym
  , vsym
  , hvsym
  , rsym
    -- * Other
  , oneof )
where

import Codec.Picture
import Data.Word (Word8)
import Graphics.Identicon

----------------------------------------------------------------------------
-- Filling

-- | Black is a special color, it means absence of light. We give this pixel
-- a name because it's used very frequently in layer coding.

black :: PixelRGB8
black = PixelRGB8 0 0 0

-- | Layer filled with a given color.

color :: PixelRGB8 -> Layer
color a = Layer $ \_ _ _ _ -> a
{-# INLINE color #-}

-- | Gradient changing from left to right.

gradientLR
  :: (Float -> Float)  -- ^ Gradient transforming function
  -> PixelRGB8         -- ^ Left color
  -> PixelRGB8         -- ^ Right color
  -> Layer
gradientLR f a b = Layer $ \w _ x _ ->
  mixWith (const $ ξ f x w) a b
{-# INLINE gradientLR #-}

-- | Gradient changing from top to bottom.

gradientTB
  :: (Float -> Float)  -- ^ Gradient transforming function
  -> PixelRGB8         -- ^ Top color
  -> PixelRGB8         -- ^ Bottom color
  -> Layer
gradientTB f a b = Layer $ \_ h _ y ->
  mixWith (const $ ξ f y h) a b
{-# INLINE gradientTB #-}

-- | Gradient changing from top left corner to bottom right corner.

gradientTLBR
  :: (Float -> Float)  -- ^ Gradient transforming function
  -> PixelRGB8         -- ^ Top left color
  -> PixelRGB8         -- ^ Bottom right color
  -> Layer
gradientTLBR f a b = Layer $ \w h x y ->
  mixWith (const $ ξ f (x + y) (w + h)) a b
{-# INLINE gradientTLBR #-}

-- | Gradient changing from top right corner to bottom left corner.

gradientTRBL
  :: (Float -> Float)  -- ^ Gradient transforming function
  -> PixelRGB8         -- ^ Top right color
  -> PixelRGB8         -- ^ Bottom left color
  -> Layer
gradientTRBL f a b = Layer $ \w h x y ->
  mixWith (const $ ξ f (w - x + y) (w + h)) a b
{-# INLINE gradientTRBL #-}

-- | Gradient with one color everywhere and another in the center.

gradientXY
  :: (Float -> Float)  -- ^ Gradient transforming function
  -> PixelRGB8         -- ^ “Edge” color
  -> PixelRGB8         -- ^ Color in the center
  -> Layer
gradientXY f a b = Layer $ \w h x y ->
  let g x' y' = floor $ (1 - n) * fromIntegral x' + n * fromIntegral y'
      n  = f (nx * ny)
      nx = mid (fromIntegral x / fromIntegral w)
      ny = mid (fromIntegral y / fromIntegral h)
  in mixWith (const g) a b
{-# INLINE gradientXY #-}

-- | A gradient helper function.

ξ
  :: (Float -> Float)  -- ^ Gradient transforming function
  -> Int               -- ^ Actual value of coordinate
  -> Int               -- ^ Maximum value of coordinate
  -> Word8             -- ^ Color at the beginning of the range
  -> Word8             -- ^ Color at the end of the range
  -> Word8             -- ^ Resulting color
ξ f v l x y = floor $ (1 - n) * fromIntegral x + n * fromIntegral y
  where
    n = f (fromIntegral v / fromIntegral l)
{-# INLINE ξ #-}

----------------------------------------------------------------------------
-- Gradient transforming functions

-- $gtrans
--
-- A note about “gradient transforming functions”: these normally map value
-- changing from 0 to 1 somehow, but they should not produce values outside
-- of that range. With help of such functions you can change character of
-- gradient transitions considerably.

-- | A built-in gradient transforming function. It maps continuous floating
-- value changing from 0 to 1 to value changing from 0 to 1 (in the middle)
-- and back to 0.

mid :: Float -> Float
mid x = 2 * (if x >= 0.5 then 1.0 - x else x)
{-# INLINE mid #-}

-- | This sharpens gradient transitions.

edge :: Float -> Float
edge x = x * x
{-# INLINE edge #-}

----------------------------------------------------------------------------
-- Position, size, and shape

-- | @onGrid w h n l@, given grid that has @w@ horizontal discrete positions
-- (of equal length) and @h@ vertical positions, it makes given layer @l@
-- occupy cell at index @n@. This approach allows you control position and
-- size at the same time.
--
-- The index @n@ can be greater than maximal index, in this case reminder of
-- division of @n@ by @w * h@ is used.

onGrid
  :: Int               -- ^ Number of horizontal positions
  -> Int               -- ^ Number of vertical positions
  -> Int               -- ^ Index of this cell
  -> Layer             -- ^ Layer to insert
  -> Layer             -- ^ Resulting layer
onGrid α β n' l = Layer $ \w h x y ->
  let n = n' `rem` (α * β)
      (y', x') = n `quotRem` α
      xu, yu :: Float
      xu = fromIntegral w / fromIntegral α
      yu = fromIntegral h / fromIntegral β
      xA = floor (fromIntegral x' * xu)
      xB = floor (fromIntegral (x' + 1) * xu)
      yA = floor (fromIntegral y' * yu)
      yB = floor (fromIntegral (y' + 1) * yu)
  in if x < xA || x >= xB || y < yA || y >= yB
       then black
       else unLayer l (xB - xA) (yB - yA) (x - xA) (y - yA) -- FIXME gaps
{-# INLINE onGrid #-}

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

-- | Add horizontal and vertical symmetry to layer. Result is a layer with
-- four mirrored repetitions of the same figure.

hvsym :: Layer -> Layer
hvsym l = Layer $ \w h x y ->
  let h' = h `quot` 2
      w' = w `quot` 2
  in unLayer l w' h' (if x > w' then w - x else x)
                     (if y > h' then h - y else y)
{-# INLINE hvsym #-}

-- | Just like 'hvsym', but every repetition is rotated by 90°. Only works
-- with square layers because for speed it just swaps coordinates.

rsym :: Layer -> Layer
rsym l = Layer $ \w h x y ->
  let h' = h `quot` 2
      w' = w `quot` 2
      α  = x > w'
      β  = y > h'
  in unLayer l w' h'
     (if α then (if β then w - x else y) else (if β then h - y else x))
     (if β then (if α then h - y else x) else (if α then w - x else y))
{-# INLINE rsym #-}

----------------------------------------------------------------------------
-- Other

-- | Select one of provided alternatives given a number.

oneof :: Integral n => [a] -> n -> a
oneof xs n = xs !! (fromIntegral n `rem` length xs)
