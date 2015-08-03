spacewΛr is a Haskell implementation of [the classic video game](https://en.wikipedia.org/wiki/Spacewar_(video_game\)).
It is backend-agnostic, adaptible to any device that can draw a set of vectors.

More details are available on the [project page](http://mattkeeter.com/projects/spacewar).

To build and run, use `cabal`:

```
git clone https://github.com/mkeeter/spacewar
cd spacewar
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal run
```
