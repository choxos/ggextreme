# Draw an interactive network plot for a network meta-analysis

Draws the network of treatment comparisons from arm level data. Each
node is a treatment and each line joins two treatments compared directly
in at least one study. Hovering over a node shows a compact table of
every arm on that treatment; hovering over a line shows the arms of each
study that makes that comparison. The card holds the columns named in
`hover`. Clicking either opens a panel under the plot with the full arm
level data side by side, one column per arm, in the manner of a trial's
baseline table: every column of `data` other than `study`, `treatment`
and `group` becomes a row, so baseline characteristics and outcomes are
shown as they were given.

## Usage

``` r
ggnma(
  data,
  study,
  treatment,
  n = NULL,
  group = NULL,
  hover = NULL,
  multiarm = TRUE,
  positions = NULL,
  palette = NULL,
  legend = TRUE,
  legend_title = NULL,
  title = NULL,
  caption = NULL,
  family = "Lato",
  contributions = NULL
)
```

## Arguments

- data:

  A data frame with one row per study arm.

- study, treatment:

  Bare column names identifying the study and the treatment of each arm.
  Each treatment may appear once per study.

- n:

  Optional bare column giving the number of participants in each arm.
  Sets node area and the participant counts in the hover cards.

- group:

  Optional bare column naming a class for each treatment, such as a drug
  class. Nodes are then colored by class and a legend is drawn. Each
  treatment must belong to exactly one class.

- hover:

  Names of the columns shown in the hover card, one row each, such as
  the sample size, an outcome and a key baseline characteristic.
  Defaults to the first four columns of the arm table. Use
  `character(0)` for a card that only lists the studies. The click panel
  always shows every column.

- multiarm:

  Shade a polygon for each set of treatments compared in a study with
  more than two arms.

- positions:

  Optional data frame placing the nodes by hand, with columns
  `treatment`, `x` and `y`, one row per treatment and `y` pointing up.
  Any units will do: the layout is scaled to fit the plot, keeping its
  shape. Labels point away from the middle of the layout.

- palette:

  Node colors. With `group`, a vector named by class, or an unnamed
  vector recycled over the classes; without it, a single color. Defaults
  to
  [`race_palette()`](https://choxos.github.io/ggextreme/reference/race_palette.md).

- legend:

  Draw the legend when `group` is given.

- legend_title:

  Text in front of the legend, such as `"Class"`.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

- contributions:

  Show where the evidence for each comparison comes from: a fit from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html)
  on the same network, whose contributions are then computed with
  [`netmeta::netcontrib()`](https://rdrr.io/pkg/netmeta/man/netcontrib.html),
  or an object that function returned. The widget gains a menu of every
  comparison; picking one widens and colors each line by the share of
  that network estimate flowing through it, labels the shares and says
  them in words under the plot.

## Value

An object of class `ggnma`, which prints as an interactive widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The fields `nodes` and
`edges` hold the treatments and comparisons with their study counts,
`multiarm` the sets of treatments compared in multi-arm studies, and
`width` and `height` the natural size in inches. With `contributions`,
the field `contributions` holds the shares.

## Details

Treatments sit on a circle, starting at the top and running clockwise in
the order of the levels of `treatment` when it is a factor, or in order
of first appearance otherwise; `positions` places them by hand instead.
Line width follows the number of studies that make the comparison. When
`n` is given, node area follows the total number of participants on that
treatment. A study with more than two arms adds a line for every pair of
its treatments and, with `multiarm`, a shaded polygon joining them.
Studies that compare the same set of treatments share one polygon, which
has its own hover card and panel.

Row labels come from each column's `label` attribute when it has one, as
set by the 'labelled', 'Hmisc' or 'haven' packages, and otherwise from
its name. Text that contains a URL or a DOI, such as a reference column,
is linked in the panel.

## Examples

``` r
net <- ggnma(psoriasis_nma, study, treatment, n = n, group = class,
             legend_title = "Class")
net
net$edges
#>                 from                 to studies    n
#> 1            Placebo         Etanercept       1  652
#> 2            Placebo Secukinumab 150 mg       4 1386
#> 3            Placebo Secukinumab 300 mg       4 1385
#> 4         Etanercept Secukinumab 150 mg       1  653
#> 5         Etanercept Secukinumab 300 mg       1  653
#> 6        Ustekinumab Secukinumab 300 mg       1  676
#> 7 Secukinumab 150 mg Secukinumab 300 mg       4 1383

# \donttest{
if (requireNamespace("netmeta", quietly = TRUE) &&
    requireNamespace("meta", quietly = TRUE)) {
  pw <- meta::pairwise(treat = treatment, event = pasi75_r, n = pasi75_n,
                       studlab = study, data = psoriasis_nma, sm = "OR")
  fit <- netmeta::netmeta(pw, common = FALSE)
  ggnma(psoriasis_nma, study, treatment, n = n, contributions = fit)
}
# }
```
