# Use an interactive graph as a widget, a ggplot or a file

A graph built by
[`ggcausal()`](https://choxos.github.io/ggextreme/reference/ggcausal.md),
[`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md),
[`ggmeta()`](https://choxos.github.io/ggextreme/reference/ggmeta.md) or
[`ggleague()`](https://choxos.github.io/ggextreme/reference/ggleague.md)
prints as an interactive widget. These functions give the other forms it
can take. `graph_widget()` returns the 'htmlwidgets' object, for use in
'shiny' or to save with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).
`graph_plot()` returns the underlying ggplot, which draws as an ordinary
static plot. `graph_save()` writes either form to a file chosen by its
extension.

## Usage

``` r
graph_widget(x)

graph_plot(x)

graph_save(x, file, res = 300)

# S3 method for class 'ggx_graph'
knit_print(x, ...)
```

## Arguments

- x:

  A graph from
  [`ggcausal()`](https://choxos.github.io/ggextreme/reference/ggcausal.md),
  [`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md),
  [`ggmeta()`](https://choxos.github.io/ggextreme/reference/ggmeta.md)
  or
  [`ggleague()`](https://choxos.github.io/ggextreme/reference/ggleague.md).

- file:

  Output path. `.html` writes the widget as a single file, which needs
  'pandoc'; `.png` writes a static image with 'ragg'.

- res:

  Resolution of a PNG in pixels per inch.

- ...:

  Passed to
  [`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html).

## Value

`graph_widget()` returns an htmlwidget and `graph_plot()` a ggplot.
`graph_save()` returns `file`, invisibly.

## Details

The graph is laid out at a fixed size, so its boxes always fit their
labels. The widget scales to the width of the page it sits in; a static
copy should be drawn at `x$width` by `x$height` inches, which is what
`graph_save()` does. The widget embeds a web copy of Lato, so it looks
the same on machines without the font.

## Examples

``` r
dag <- ggcausal(cleft_dag$edges, cleft_dag$nodes)
w <- graph_widget(dag)
p <- graph_plot(dag)
# \donttest{
graph_save(dag, tempfile(fileext = ".png"))
# }
```
