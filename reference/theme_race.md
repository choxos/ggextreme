# The bare theme a race frame is drawn on

A race frame paints its own title, axis, timeline and footer as layers
in one canvas coordinate system, so the theme only has to supply the
page color and get everything else out of the way.

## Usage

``` r
theme_race(page = race_ink$page)
```

## Arguments

- page:

  Background color of the page behind the card.

## Value

A [ggplot2::theme](https://ggplot2.tidyverse.org/reference/theme.html)
object.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_race()
```
