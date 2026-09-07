# Colors used by the bar chart race

A qualitative palette of muted teals, greens, blues, purples and warm
earth tones, chosen to stay readable side by side in a dense stack of
bars. Consecutive colours are far apart in hue, so the first few remain
easy to tell apart when only a handful are used, as with `group` in
[`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md).

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
#> [1] "#22928F" "#B66399" "#757CC6" "#568E4F" "#C76253"
```
