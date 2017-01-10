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
$ ./dicestats
Usage: dicestats xdy [(lt|le|eq|ge|gt) [z]]
```

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
$ ./dicestats 2d8 gt 4
P(2d8 > 4) = 90.62%
```

```
$ ./dicestats 2d8 le 4
P(2d8 <= 4) = 9.38%
```

```
$ ./dicestats ge
P(2d8 >= 2) = 100.00%
P(2d8 >= 3) = 98.44%
P(2d8 >= 4) = 95.31%
P(2d8 >= 5) = 90.62%
P(2d8 >= 6) = 84.38%
P(2d8 >= 7) = 76.56%
P(2d8 >= 8) = 67.19%
P(2d8 >= 9) = 56.25%
P(2d8 >= 10) = 43.75%
P(2d8 >= 11) = 32.81%
P(2d8 >= 12) = 23.44%
P(2d8 >= 13) = 15.62%
P(2d8 >= 14) = 9.38%
P(2d8 >= 15) = 4.69%
P(2d8 >= 16) = 1.56%
```
