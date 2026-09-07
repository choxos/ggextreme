# Colors used by the bar chart race

A qualitative palette of muted teals, greens, blues, purples and warm
earth tones, chosen to stay readable side by side in a dense stack of
bars.

## Usage

``` r
race_palette(n = 26)
```

## Arguments

- n:

  Number of colors to return. The palette is recycled when `n` is larger
  than the number of base colors.

## Value

A character vector of `n` hex colors.

## Examples

``` r
race_palette(5)
#> [1] "#22928F" "#B66399" "#BF6662" "#757CC6" "#4BAAA9"
```
