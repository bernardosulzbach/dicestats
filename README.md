# Dice Statistics

Command line tool to generate statistics about the rolling of fair die.

## Development Build

```
$ cabal sandbox init
$ cabal install --enable-tests -j
```

### Running the tests

```
$ cabal test
```

## Usage

```
$ ./dicestats 2d8
P(2d8 == 2) = 1.56%
P(2d8 == 3) = 3.12%
P(2d8 == 4) = 4.69%
P(2d8 == 5) = 6.25%
P(2d8 == 6) = 7.81%
P(2d8 == 7) = 9.38%
P(2d8 == 8) = 10.94%
P(2d8 == 9) = 12.50%
P(2d8 == 10) = 10.94%
P(2d8 == 11) = 9.38%
P(2d8 == 12) = 7.81%
P(2d8 == 13) = 6.25%
P(2d8 == 14) = 4.69%
P(2d8 == 15) = 3.12%
P(2d8 == 16) = 1.56%
```

```
$ ./dicestats 2d8
P(2d8 > 4) = 90.62%
```

```
$ ./dicestats 2d8
P(2d8 <= 4) = 9.38%
```
