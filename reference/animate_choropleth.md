# Animate a choropleth map

Plays the years of a
[`ggchoropleth()`](https://choxos.github.io/ggextreme/reference/ggchoropleth.md)
map, easing each region from one year's shade to the next, and writes
the result to a GIF or MP4.

## Usage

``` r
animate_choropleth(
  x,
  file = "map.gif",
  step = 0.6,
  end_pause = 2,
  fps = 20,
  res = 150,
  loop = TRUE,
  cores = max(1L, parallel::detectCores() - 1L),
  quiet = FALSE
)
```

## Arguments

- x:

  A map from
  [`ggchoropleth()`](https://choxos.github.io/ggextreme/reference/ggchoropleth.md).

- file:

  Output path, ending in `.gif` or `.mp4`.

- step:

  Seconds from one time to the next.

- end_pause:

  Seconds to hold the final frame.

- fps:

  Frames per second.

- res:

  Output resolution in pixels per inch.

- loop:

  Loop the GIF. Ignored for video.

- cores:

  Number of cores to draw frames on.

- quiet:

  Suppress the progress bar.

## Value

`file`, invisibly.

## Examples

``` r
# \donttest{
m <- ggchoropleth(subset(clefts_qci_world, year >= 2015), iso3, year,
                  values = c(QCI = "qci"))
animate_choropleth(m, tempfile(fileext = ".gif"), step = 0.3, fps = 6,
                   res = 60, cores = 1)
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |========                                                              |  11%
#>   |                                                                              |================                                                      |  22%
#>   |                                                                              |=======================                                               |  33%
#>   |                                                                              |===============================                                       |  44%
#>   |                                                                              |=======================================                               |  56%
#>   |                                                                              |===============================================                       |  67%
#>   |                                                                              |======================================================                |  78%
#>   |                                                                              |==============================================================        |  89%
#>   |                                                                              |======================================================================| 100%
# }
```
