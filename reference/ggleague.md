# Draw an interactive league table for a network meta-analysis

Draws every pairwise estimate of a network meta-analysis as a grid, with
the treatments on the diagonal and a ranking beside it. Following
[`netmeta::netleague()`](https://rdrr.io/pkg/netmeta/man/netleague.html),
each cell compares the treatment that comes first in the table with the
one that comes second: network estimates sit below the diagonal and
direct estimates, from the trials that compare the pair head to head,
above it.

## Usage

``` r
ggleague(
  x,
  data = NULL,
  study = NULL,
  treatment = NULL,
  pooled = NULL,
  small_values = NULL,
  order = NULL,
  ranking = TRUE,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- x:

  A network meta-analysis from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html).

- data:

  Optional arm level data, one row per study arm, for the click panels,
  such as the data given to
  [`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md).
  Every column other than `study` and `treatment` becomes a row of the
  arm table.

- study, treatment:

  Bare column names in `data` identifying the study and treatment of
  each arm. Treatment names must match those in `x`.

- pooled:

  Which model to show, `"random"` or `"common"`. Defaults to the random
  effects model when `x` has one.

- small_values:

  Whether small values of the effect are `"desirable"`, as for
  mortality, or `"undesirable"`, as for a response. Sets which treatment
  a cell favors and the direction of the ranking. Defaults to the
  setting stored in `x`, which is worth checking.

- order:

  Order of the treatments along the diagonal. Defaults to the ranking,
  best first.

- ranking:

  Draw the P-score ranking beside the table.

- title, caption:

  Title above the table and note below it. The default caption explains
  which estimates sit on each side of the diagonal.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggleague`, which prints as an interactive widget.
Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The fields `treatments` and
`pscore` give the order used and the P-scores.

## Details

Cells are shaded by the size of the effect, in one color when it favors
the first treatment and another when it favors the second, and faded
when the confidence interval includes the null. Hovering over a cell
shows its network, direct and indirect estimates and the share of the
network estimate that comes from direct trials. Clicking a cell opens
the direct trials under the table: their arm level data when `data` is
given, and otherwise each trial's own estimate. Hovering over a
treatment on the diagonal or in the ranking lights its row and column.

## Examples

``` r
if (requireNamespace("netmeta", quietly = TRUE) &&
    requireNamespace("meta", quietly = TRUE)) {
  pw <- meta::pairwise(treat = treatment, event = pasi75_r,
                       n = pasi75_n, studlab = study,
                       data = psoriasis_nma, sm = "OR")
  nma <- netmeta::netmeta(pw, common = FALSE)
  ggleague(nma, psoriasis_nma, study, treatment,
           small_values = "undesirable")
}
```
