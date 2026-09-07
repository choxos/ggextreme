# Render a bar chart race to a file

Draws every frame with 'ragg' and encodes them. The encoder follows the
file extension: `.gif` uses 'gifski' and falls back to 'magick',
anything else is treated as video and uses 'av', falling back to an
`ffmpeg` binary on the search path.

## Usage

``` r
animate_race(
  x,
  file = "race.mp4",
  loop = TRUE,
  cores = max(1L, parallel::detectCores() - 1L),
  quiet = FALSE
)
```

## Arguments

- x:

  A `ggrace` object from
  [`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md).

- file:

  Output path, ending in `.gif` or `.mp4`.

- loop:

  Loop the GIF. Ignored for video.

- cores:

  Number of cores to draw frames on. Frames are independent, so this
  scales close to linearly. Forced to 1 on Windows, where
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  cannot fork.

- quiet:

  Suppress the progress bar. Progress is not reported when drawing on
  more than one core.

## Value

`file`, invisibly.
