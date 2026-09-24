# Animate a cumulative meta-analysis

Replays a meta-analysis one study at a time, in the order of the data,
and writes it to a GIF or MP4. Each study fades in as it is added, and
the pooled diamond eases to its new position over `swap` seconds, the
same motion as
[`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md),
before holding for `hold` seconds.

## Usage

``` r
animate_meta(
  x,
  file = "meta.gif",
  time = NULL,
  hold = 0.8,
  swap = 0.45,
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

  A forest plot from
  [`ggmeta()`](https://choxos.github.io/ggextreme/reference/ggmeta.md).

- file:

  Output path, ending in `.gif` or `.mp4`.

- time:

  Optional labels, one per study, shown large behind the plot as each
  study is added, such as the year of publication.

- hold:

  Seconds to hold on each step.

- swap:

  Seconds the pooled estimate takes to move to its new value.

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
if (requireNamespace("metafor", quietly = TRUE)) {
  dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
                         ci = cpos, di = cneg, data = metadat::dat.bcg,
                         slab = paste(author, year))
  dat <- dat[order(dat$year), ]
  fit <- metafor::rma(yi, vi, data = dat)
  animate_meta(ggmeta(fit), tempfile(fileext = ".gif"), time = dat$year,
               fps = 10, cores = 1)
}
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |=                                                                     |   2%
#>   |                                                                              |===                                                                   |   4%
#>   |                                                                              |====                                                                  |   6%
#>   |                                                                              |======                                                                |   8%
#>   |                                                                              |=======                                                               |  10%
#>   |                                                                              |=========                                                             |  12%
#>   |                                                                              |==========                                                            |  14%
#>   |                                                                              |===========                                                           |  16%
#>   |                                                                              |=============                                                         |  18%
#>   |                                                                              |==============                                                        |  20%
#>   |                                                                              |================                                                      |  22%
#>   |                                                                              |=================                                                     |  24%
#>   |                                                                              |===================                                                   |  27%
#>   |                                                                              |====================                                                  |  29%
#>   |                                                                              |=====================                                                 |  31%
#>   |                                                                              |=======================                                               |  33%
#>   |                                                                              |========================                                              |  35%
#>   |                                                                              |==========================                                            |  37%
#>   |                                                                              |===========================                                           |  39%
#>   |                                                                              |=============================                                         |  41%
#>   |                                                                              |==============================                                        |  43%
#>   |                                                                              |===============================                                       |  45%
#>   |                                                                              |=================================                                     |  47%
#>   |                                                                              |==================================                                    |  49%
#>   |                                                                              |====================================                                  |  51%
#>   |                                                                              |=====================================                                 |  53%
#>   |                                                                              |=======================================                               |  55%
#>   |                                                                              |========================================                              |  57%
#>   |                                                                              |=========================================                             |  59%
#>   |                                                                              |===========================================                           |  61%
#>   |                                                                              |============================================                          |  63%
#>   |                                                                              |==============================================                        |  65%
#>   |                                                                              |===============================================                       |  67%
#>   |                                                                              |=================================================                     |  69%
#>   |                                                                              |==================================================                    |  71%
#>   |                                                                              |===================================================                   |  73%
#>   |                                                                              |=====================================================                 |  76%
#>   |                                                                              |======================================================                |  78%
#>   |                                                                              |========================================================              |  80%
#>   |                                                                              |=========================================================             |  82%
#>   |                                                                              |===========================================================           |  84%
#>   |                                                                              |============================================================          |  86%
#>   |                                                                              |=============================================================         |  88%
#>   |                                                                              |===============================================================       |  90%
#>   |                                                                              |================================================================      |  92%
#>   |                                                                              |==================================================================    |  94%
#>   |                                                                              |===================================================================   |  96%
#>   |                                                                              |===================================================================== |  98%
#>   |                                                                              |======================================================================| 100%
# }
```
