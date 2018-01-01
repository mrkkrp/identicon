# Identicon

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/identicon.svg?style=flat)](https://hackage.haskell.org/package/identicon)
[![Stackage Nightly](http://stackage.org/package/identicon/badge/nightly)](http://stackage.org/nightly/package/identicon)
[![Stackage LTS](http://stackage.org/package/identicon/badge/lts)](http://stackage.org/lts/package/identicon)
[![Build Status](https://travis-ci.org/mrkkrp/identicon.svg?branch=master)](https://travis-ci.org/mrkkrp/identicon)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/identicon/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/identicon?branch=master)

The package implements a flexible framework for identicons generation on top
of the [Juicy Pixels](https://hackage.haskell.org/package/JuicyPixels)
package.

## Quick start

To use the package you usually need the following set of imports (and a
couple of language extensions for the type level magic):

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import Codec.Picture -- JuicyPixels
import Data.ByteString (ByteString) -- we use strict byte strings
import Data.Proxy
import Data.Word (Word8)
import Graphics.Identicon -- core definitions
import Graphics.Identicon.Primitive -- some visual primitives
```

You first write a type that holds information about total number of bytes
your identicon consumes and number of distinct visual components it has
(they are called “layers” in the terminology of the package):

```haskell
type MyIcon = Identicon 12 :+ Consumer 4 :+ Consumer 4 :+ Consumer 4
```

Here we have an identicon that needs 12 bytes to be generated. It has three
consumers that take 4 bytes each and generate layers, i.e. visual objects
(circles, squares, etc.).

The second step is to write implementation of every layer. We can use the
primitives available out-of-the-box, they live in the
`Graphics.Identicon.Primitive` module:

```haskell
myImpl :: Implementation MyIcon
myImpl = Identicon :+ a :+ a :+ a
  where
    a :: Word8 -> Word8 -> Word8 -> Word8 -> Layer
    a r g b n = rsym $ onGrid 3 3 n $
      gradientXY (edge . mid) black (PixelRGB8 r g b)
```

We could choose to code every layer differently, but since position and
color of every layer are unlikely to be the same, this approach will work
well too.

Every byte is available to the layer-generating function as a distinct
`Word8` argument. The type system makes sure that:

* you consume exactly as many bytes as you promised in type of your
  identicon;

* you have as many layers as you described in type of your identicon;

* every function in your implementation has a correct signature (i.e. it
  grabs as many `Word8`s as promised and produces a `Layer` in the end).

Mixing of layers and generation is handled by the library like this:

```haskell
-- | Here is the function that generates your identicons. It's usually
-- convenient to wrap the 'renderIdenticon' function that comes with the
-- library.

genMyIdenticon
  :: Int               -- ^ Identicon width
  -> Int               -- ^ Identicon height
  -> ByteString        -- ^ Input (some sort of hash or something)
  -> Maybe (Image PixelRGB8)
     -- ^ Identicon, unless 'ByteString' is too short
genMyIdenticon = renderIdenticon (Proxy :: Proxy MyIcon) myImpl
```

For more information head straight to the Haddocks. BTW, I have
written
[a blog post](https://markkarpov.com/post/the-identicon-package.html) about
the package where I demonstrate some pictures generated with it.

## License

Copyright © 2016–2018 Mark Karpov

Distributed under BSD 3 clause license.
