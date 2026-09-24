# Animate a Kaplan-Meier plot

Draws the curves of a
[`ggkm()`](https://choxos.github.io/ggextreme/reference/ggkm.md) plot
over follow-up, as though the trial were being watched, and writes the
result to a GIF or MP4. The numbers at risk appear as each break is
reached and the time is shown large behind the curves.

## Usage

``` r
animate_km(
  x,
  file = "km.gif",
  duration = 5,
  end_pause = 2,
  fps = 30,
  res = 150,
  loop = TRUE,
  cores = max(1L, parallel::detectCores() - 1L),
  quiet = FALSE
)
```

## Arguments

- x:

  A plot from
  [`ggkm()`](https://choxos.github.io/ggextreme/reference/ggkm.md).

- file:

  Output path, ending in `.gif` or `.mp4`.

- duration:

  Seconds to draw the full follow-up.

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
if (requireNamespace("survival", quietly = TRUE)) {
  colon <- subset(survival::colon, etype == 2)
  km <- ggkm(survival::Surv(time / 365.25, status) ~ rx, data = colon,
             ph_tests = FALSE, xlab = "Years")
  animate_km(km, tempfile(fileext = ".gif"), duration = 2, fps = 8,
             cores = 1)
}
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |====                                                                  |   6%
#>   |                                                                              |=========                                                             |  12%
#>   |                                                                              |=============                                                         |  19%
#>   |                                                                              |==================                                                    |  25%
#>   |                                                                              |======================                                                |  31%
#>   |                                                                              |==========================                                            |  38%
#>   |                                                                              |===============================                                       |  44%
#>   |                                                                              |===================================                                   |  50%
#>   |                                                                              |=======================================                               |  56%
#>   |                                                                              |============================================                          |  62%
#>   |                                                                              |================================================                      |  69%
#>   |                                                                              |====================================================                  |  75%
#>   |                                                                              |=========================================================             |  81%
#>   |                                                                              |=============================================================         |  88%
#>   |                                                                              |==================================================================    |  94%
#>   |                                                                              |======================================================================| 100%
# }
```
