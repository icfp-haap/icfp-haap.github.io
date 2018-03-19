HAAP - Haskell Automatic Assessment Platform
======

This repository includes an anonymised snapshot of the HAAP project, described in a submitted publication for ICFP 2018.

# Installation

To install HAAP in a new sandbox simply run:
```
cabal sandbox init
cabal install HAAP.cabal
```

For CodeWorld animations you need to have `ghcjs` installed:
1. install `ghcjs` from https://github.com/commercialhaskell/stack/blob/master/doc/ghcjs.md
2. add `ghcjs` to your path
3. install the codeworld packages
```
cabal install --ghcjs codeworld-haap-api
cabal install --ghcjs codeworld-haap-base
```

# Example

You can experiment with a minimalistic example.

```
cd example
cabal exec -- ghc Example.hs
```

You can find the generated feedback of a pre-compiled example in https://icfp-haap.github.io/examples/plab/.

Checkout also the hall-of-fame of screenshots from student assignments in the last 5 years in https://icfp-haap.github.io/examples/hall-of-fame/

