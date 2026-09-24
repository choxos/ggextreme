# Interactive network plots

A network meta-analysis compares several treatments at once by combining
trials that each compare a few of them. The network plot shows which
comparisons the evidence covers: every treatment is a node, and every
pair of treatments compared directly in at least one trial is joined by
a line. Judging whether the trials are similar enough to combine means
going back to the arm level data, the baseline characteristics and
outcomes of each arm.
[`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md) puts
that data behind the plot. Hovering over a node or a line shows its arms
side by side, in the manner of a trial’s baseline table, and clicking it
opens the full table with every column.

``` r

library(ggextreme)
```

## A first network

The package includes arm level data from five randomized trials in
moderate to severe plaque psoriasis, compiled by Phillippo et al. for
their multilevel network meta-regression.

``` r

net <- ggnma(
  psoriasis_nma,
  study = study,
  treatment = treatment,
  n = n,
  group = class,
  legend_title = "Class",
  title = "Treatments for plaque psoriasis"
)
net
```

Hover over the line between placebo and secukinumab 150 mg to compare
the four trials that make that comparison, arm by arm, and click it for
every column. Hover over or click a node to see every arm on that
treatment. The references at the bottom of each panel are linked.

## The data

[`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md) takes
long data with one row per study arm, the shape `netmeta::pairwise()`
and `multinma::set_agd_arm()` start from:

``` r

head(psoriasis_nma[c("study", "treatment", "n", "pasi75_r", "age")])
#>     study          treatment   n pasi75_r  age
#> 1   CLEAR        Ustekinumab 339      265 44.6
#> 2   CLEAR Secukinumab 300 mg 337      304 45.2
#> 3 ERASURE            Placebo 248       11 45.4
#> 4 ERASURE Secukinumab 150 mg 245      174 44.9
#> 5 ERASURE Secukinumab 300 mg 245      200 44.9
#> 6 FEATURE            Placebo  59        0 46.5
```

Three columns place each arm: `study`, `treatment` and, optionally,
`group` for the drug class. Every other column becomes a row of the
panel tables, in the order given, so choosing what to show is a matter
of selecting columns first. `n` is optional; when given, node area
follows the total number of participants on each treatment, and the
hover cards count participants as well as studies.

Rows are named from each column’s `label` attribute when it has one, as
set by the ‘labelled’, ‘Hmisc’ or ‘haven’ packages, and otherwise from
the column name:

``` r

attr(psoriasis_nma$age, "label")
#> [1] "Mean age (years)"
```

Text that is the same for every arm of a study, such as the reference
here, is listed once per study under the table rather than repeated in
every column. Text that contains a URL or a DOI is linked.

## The hover card

The hover card holds a compact version of the table. By default it shows
the first four columns; `hover` picks them by name, so the card can
carry the sample size, the outcome and the baseline characteristic that
matters most for the question at hand:

``` r

ggnma(
  psoriasis_nma, study, treatment, n = n, group = class,
  hover = c("n", "pasi75_r", "pasi_w0", "prior_systemic"),
  legend = FALSE
)
```

`hover = character(0)` gives a card that only lists the studies. The
click panel always shows every column.

## What the plot encodes

- **Nodes** are treatments, placed on a circle starting at the top and
  running clockwise in the order of the factor levels of `treatment`, or
  in order of first appearance. `positions` places them by hand.
- **Node area** follows the number of participants on each treatment
  when `n` is given.
- **Line width** follows the number of studies that make the comparison.
- **A multi-arm study** adds a line for every pair of its treatments,
  and a shaded polygon joining them. The counts are available on the
  object:

``` r

net$edges
#>                 from                 to studies    n
#> 1            Placebo         Etanercept       1  652
#> 2            Placebo Secukinumab 150 mg       4 1386
#> 3            Placebo Secukinumab 300 mg       4 1385
#> 4         Etanercept Secukinumab 150 mg       1  653
#> 5         Etanercept Secukinumab 300 mg       1  653
#> 6        Ustekinumab Secukinumab 300 mg       1  676
#> 7 Secukinumab 150 mg Secukinumab 300 mg       4 1383
```

## Multi-arm studies

Lines alone cannot show that three treatments were compared within one
trial rather than in three separate ones. A shaded polygon joins the
treatments of every study with more than two arms, and studies that
compare the same set share one polygon. Here ERASURE, FEATURE and
JUNCTURE each compare placebo with both doses of secukinumab, and
FIXTURE adds etanercept:

``` r

net$multiarm
#>                                                    treatments arms
#> 1             Placebo, Secukinumab 150 mg, Secukinumab 300 mg    3
#> 2 Placebo, Etanercept, Secukinumab 150 mg, Secukinumab 300 mg    4
#>                      studies
#> 1 ERASURE, FEATURE, JUNCTURE
#> 2                    FIXTURE
```

Each polygon has its own hover card and panel, with the arms of every
study it stands for. `multiarm = FALSE` leaves the shading out.

## Placing nodes by hand

`positions` takes one row per treatment with `x` and `y` coordinates,
`y` pointing up. Any units will do: the layout is scaled to fit the
plot, keeping its shape, and labels point away from its middle. This one
puts placebo at the top, the active comparators at the sides and the two
secukinumab doses along the bottom:

``` r

positions <- data.frame(
  treatment = c("Placebo", "Etanercept", "Ustekinumab",
                "Secukinumab 150 mg", "Secukinumab 300 mg"),
  x = c(1, 0, 2, 0.5, 1.5),
  y = c(1, 0, 0, -1, -1)
)
ggnma(psoriasis_nma, study, treatment, n = n, group = class,
      positions = positions, legend = FALSE)
```

## Colors

Without `group`, every node takes the first color of
[`race_palette()`](https://choxos.github.io/ggextreme/reference/race_palette.md),
or the single color in `palette`. With `group`, nodes are colored by
class and a legend is drawn. A named `palette` sets colors by class, and
classes left out keep their default:

``` r

ggnma(
  psoriasis_nma, study, treatment, n = n, group = class,
  palette = c(Placebo = "#9A9A9A"),
  legend_title = "Class"
)
```

## Other forms

As with causal diagrams, the plot prints as a widget in the RStudio
viewer, in R Markdown and Quarto documents, and on ‘pkgdown’ sites.
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
returns the htmlwidget for ‘shiny’,
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
the underlying ggplot for a manuscript, and
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
a single `.html` file or a static `.png`:

``` r

graph_save(net, "network.html")
graph_save(net, "network.png", res = 300)
```

## Source of the example data

The arm level data are those compiled by Phillippo (2019) and
distributed with the ‘multinma’ package, from the published reports of
CLEAR, ERASURE, FEATURE, FIXTURE and JUNCTURE; see
[`?psoriasis_nma`](https://choxos.github.io/ggextreme/reference/psoriasis_nma.md).
They were analyzed in [Phillippo et
al. (2020)](https://doi.org/10.1111/rssa.12579).
