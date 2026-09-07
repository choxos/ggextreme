# The pixel size of a race frame

A frame has a fixed aspect ratio, set by the width and by how many bars
are visible.
[`animate_race()`](https://choxos.github.io/ggextreme/reference/animate_race.md)
uses this to size its device; use it when drawing a single frame
yourself, so the layout is not stretched.

## Usage

``` r
race_size(x, width = x$width)
```

## Arguments

- x:

  A `ggrace` object from
  [`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md).

- width:

  Output width in pixels. Defaults to the width the race was built for.

## Value

A named numeric vector with the `width` and `height` in pixels.

## Examples

``` r
race <- ggrace(clefts_qci, qci, country, year, top_n = 10)
race_size(race)
#>  width height 
#>    736    459 
```
