haskell-smallpt
===

> Haskell implementation for "smallpt" (small path tracing). The code is written for read clarity, so it is a little longer than the original C version.
>This version of the Haskell implementation for "smallpt" brings support of object files to the table, as seen in the picture, where the Standford  bunny has been rendered correctly.

## Usage

```shell
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
$ cabal run [width] [height] [samples]
```

## Result

![result](result.png)

(800 x 600, 512 samples per pixel, computation took 8200 secs with 1.8 GHz CPU)

## License

MIT License 2015 (c) tatsy, Tatsuya Yatagawa, Miguel Lemos
