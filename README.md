# Identicon

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/identicon.svg?style=flat)](https://hackage.haskell.org/package/identicon)
[![Stackage Nightly](http://stackage.org/package/identicon/badge/nightly)](http://stackage.org/nightly/package/identicon)
[![Stackage LTS](http://stackage.org/package/identicon/badge/lts)](http://stackage.org/lts/package/identicon)
[![CI](https://github.com/mrkkrp/identicon/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/identicon/actions/workflows/ci.yaml)

The package implements a flexible framework for generation of identicons on
top of [Juicy Pixels](https://hackage.haskell.org/package/JuicyPixels).

## Quick start

To use the package you usually need the following imports and language
extensions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Codec.Picture -- JuicyPixels
import Data.ByteString (ByteString) -- we use strict byte strings
import Data.Proxy
import Data.Word (Word8)
import Graphics.Identicon -- core definitions
import Graphics.Identicon.Primitive -- some visual primitives
```

You first write a type that represents the total number of bytes your
identicon consumes and the number of distinct visual components it has (they
are called “layers” in the terminology of the package):

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
    a r g b n =
      rsym $ onGrid 3 3 n $
        gradientXY (edge . mid) black (PixelRGB8 r g b)
```

Every byte is available to the layer-generating function as a distinct
`Word8` argument. The type system makes sure that:

* you consume exactly as many bytes as you promised in the type of your
  identicon;

* you have as many layers as you have described in the type of your
  identicon;

* every function in your implementation has a correct signature (i.e. it
  takes as many `Word8`s as promised and produces a `Layer` in the end).

Mixing of layers and generation is handled by the library like this:

```haskell
-- | Here is the function that generates your identicons. It's usually
-- convenient to wrap the 'renderIdenticon' function that comes with the
-- library.
genMyIdenticon ::
  -- | Identicon width
  Int ->
  -- | Identicon height
  Int ->
  -- | Input (some sort of hash or something)
  ByteString ->
  -- | Identicon, unless 'ByteString' is too short
  Maybe (Image PixelRGB8)
genMyIdenticon = renderIdenticon (Proxy :: Proxy MyIcon) myImpl
```

For more information head straight to the Haddocks. BTW, I have
written
[a blog post](https://markkarpov.com/post/the-identicon-package.html) about
the package where I demonstrate some pictures generated with it.

## Related packages

The following packages are designed to be used with `identicon`:

* [`identicon-style-squares`](https://hackage.haskell.org/package/identicon-style-squares)

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/identicon/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.
