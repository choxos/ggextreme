# Draw one frame of a bar chart race

The frame is a single
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
laid out in card units, so the title, axis, bars, timeline and footer
all live in one coordinate system and land on fixed positions rather
than wherever a layout engine puts them. That is what keeps the panel
from shifting sideways between frames.

## Usage

``` r
race_frame(x, frame = 1L)
```

## Arguments

- x:

  A `ggrace` object from
  [`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md).

- frame:

  Frame index, between 1 and the number of frames.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

Frames are set in Lato, which the package registers with 'systemfonts'.
Draw them on a device that understands registered fonts, such as
[`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html),
which is what
[`animate_race()`](https://choxos.github.io/ggextreme/reference/animate_race.md)
uses. On other devices pass `family = ""` to
[`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md) to
fall back to the device default.

## Examples

``` r
phones <- as.data.frame.table(datasets::WorldPhones, responseName = "phones")
names(phones)[1:2] <- c("year", "region")
phones$year <- as.numeric(as.character(phones$year))

race <- ggrace(phones, phones, region, year, top_n = 7, family = "")
race_frame(race, 1)
```
