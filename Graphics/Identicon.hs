-- |
-- Module      :  Graphics.Identicon
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Flexible generation of identicons.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Identicon
  (  )
where

import Codec.Picture
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Word (Word8, Word16)
import GHC.TypeLits
import qualified Data.ByteString as B

import Test.QuickCheck -- FIXME

-- data Object (h :: Nat) (w :: Nat) (n :: Nat) -- = newtype?
  -- Object { unObject :: ObjectFnc h w n }

infixl 8 :+
data a :+ b = a :+ b

data Img (n :: Nat) = Img

data Obj (n :: Nat)

type family ObjectFnc (n :: Nat) :: k where
  ObjectFnc 0 = Int -> Int -> Int -> Int -> PixelRGB8
  ObjectFnc n = Word8 -> ObjectFnc (n - 1)

type family Impl a

type instance Impl (Img n) = Img n

type instance Impl (a :+ Obj n) = Impl a :+ ObjectFnc n

class HasObjects a where
  -- bgf ::
  bgf :: Proxy a -> Impl a -> Int -> Int -> ByteString
      -> (ByteString, Int -> Int -> PixelRGB8)

instance HasObjects (Img n) where
  bgf :: Proxy (Img n) -> Img n -> Int -> Int -> ByteString
      -> (ByteString, Int -> Int -> PixelRGB8)
  bgf _ _ _ _ bs = (bs, \_ _ -> PixelRGB8 0 0 0)

instance (HasObjects a, ApplyWords (ObjectFnc n)) => HasObjects (a :+ Obj n) where
  bgf :: Proxy (a :+ Obj n) -> Impl (a :+ Obj n) -> Int -> Int -> ByteString
      -> (ByteString, Int -> Int -> PixelRGB8)
  bgf _ (a :+ b) weight height bs0 =
    let (bs1, x) = bgf (Proxy :: Proxy a) a weight height bs0
        (bs2, y) = applyWords b bs1
    in (bs2, mixPixels x (y weight height)) -- should be more flexible

mixPixels
  :: (Int -> Int -> PixelRGB8)
  -> (Int -> Int -> PixelRGB8)
  ->  Int -> Int -> PixelRGB8
mixPixels a b x y = PixelRGB8 (s ar br) (s ag bg) (s ab bb)
  where
    (PixelRGB8 ar ag ab) = a x y
    (PixelRGB8 br bg bb) = b x y
    s i j = fromIntegral $ min 255 (fromIntegral i + fromIntegral j :: Word16)

class ApplyWords a where
  applyWords :: a -> ByteString -> (ByteString, Int -> Int -> Int -> Int -> PixelRGB8)

instance ApplyWords (Int -> Int -> Int -> Int -> PixelRGB8) where
  applyWords f bs = (bs, f)

instance ApplyWords f => ApplyWords (Word8 -> f) where
  applyWords f bs =
    let (b,bs') = fromJust (B.uncons bs)
    in applyWords (f b) bs'

-- instance HasObjects (a :+ b) where
--   rere :: Proxy (a :+ b) -> ByteString -> Int -> Int -> Image PixelRGB8

type family HasSlots a :: Nat where
  HasSlots (Img n) = n
  HasSlots (x :+ y) = HasSlots x

type family SlotsOccupied a :: Nat where
  SlotsOccupied (Img n) = 0
  SlotsOccupied (Obj n) = n
  SlotsOccupied (x :+ y) = SlotsOccupied x + SlotsOccupied y

generateIdenticon
  :: forall a. ( HasObjects a
     , KnownNat (HasSlots a)
     , KnownNat (SlotsOccupied a)
     , HasSlots a ~ SlotsOccupied a )
  => Proxy a
  -> Impl a
  -> Int -- width
  -> Int -- height
  -> ByteString
  -> Maybe (Image PixelRGB8)
generateIdenticon proxy impl width height bs =
  if B.length bs < fromIntegral (natVal (Proxy :: Proxy (HasSlots a)))
    then Nothing
    else Just $ generateImage (snd $ bgf proxy impl width height bs) width height

----------------------------------------------------------------------------
-- Playground

myImageType :: Proxy (Img 5 :+ Obj 3 :+ Obj 1 :+ Obj 1)
myImageType = Proxy

myImplType = Img :+ a :+ b :+ c
  where
    a :: Word8 -> Word8 -> Word8 -> Int -> Int -> Int -> Int -> PixelRGB8
    a w0 w1 w2 height width x y =
      if (((x - width `div` 2) ^ 2) +
         ((y - height `div` 2) ^ 2)) <= (fromIntegral w0 + fromIntegral w1)
        then PixelRGB8 w0 w1 w2
        else PixelRGB8 0 0 0
    b :: Word8 -> Int -> Int -> Int -> Int -> PixelRGB8
    b w0 height weight x y =
      if x > fromIntegral w0 then PixelRGB8 0 0x60 0 else PixelRGB8 0 0 w0
    c :: Word8 -> Int -> Int -> Int -> Int -> PixelRGB8
    c w0 height weight x y =
       if (x > weight `div` 2) && (y > height `div` 2)
         then PixelRGB8 0 w0 0
         else PixelRGB8 0 0 (fromIntegral x)

myGener :: ByteString -> Maybe (Image PixelRGB8)
myGener = generateIdenticon myImageType myImplType 120 120

rere :: IO ()
rere = do
  bs <- B.pack <$> generate (vector 5)
  case myGener bs of
    Nothing -> putStrLn "That doesn't work!"
    Just img -> savePngImage "indenticon.png" (ImageRGB8 img)
