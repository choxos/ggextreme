# League tables

A league table puts every pairwise estimate of a network meta-analysis
in one grid. Each number in it blends direct evidence, from trials that
compare the pair head to head, with indirect evidence carried through
the rest of the network, and how much of each goes into a cell matters
for how far it can be trusted.
[`ggleague()`](https://choxos.github.io/ggextreme/reference/ggleague.md)
shows that split for every cell, next to a ranking of the treatments,
and opens the direct trials on click.

``` r

library(ggextreme)
```

## A first league table

The network is the one drawn by
[`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md): five
randomized trials of treatments for plaque psoriasis, analyzed for PASI
75 response with netmeta.

``` r

pw <- meta::pairwise(
  treat = treatment, event = pasi75_r, n = pasi75_n,
  studlab = study, data = psoriasis_nma, sm = "OR"
)
nma <- netmeta::netmeta(pw, common = FALSE)

ggleague(
  nma, psoriasis_nma, study, treatment,
  small_values = "undesirable",
  title = "PASI 75 response"
)
```

Hover over a cell for its network, direct and indirect estimates and the
share of the network estimate that comes from direct trials. Click it to
compare the direct trials arm by arm. Hover over a treatment, on the
diagonal or in the ranking, to light its row and column.

## Reading the table

The layout follows
[`netmeta::netleague()`](https://rdrr.io/pkg/netmeta/man/netleague.html).
Each cell compares the treatment that comes first in the table with the
one that comes second:

- below the diagonal, the network estimate;
- above the diagonal, the direct estimate, or “no direct trials” when no
  trial compares the pair.

Treatments are ordered by P-score, best first, so most cells favor the
first treatment. Cells are shaded by the size of the effect, in one
color when it favors the first treatment and another when it favors the
second, and faded when the confidence interval includes no difference.

## Which way is better

`small_values` says whether small values of the effect are desirable, as
for mortality, or undesirable, as for a response. It sets which
treatment a cell favors and the direction of the ranking, so it has to
be right for the outcome. It defaults to the setting stored in the
netmeta object, which netmeta sets to “desirable” unless told otherwise;
for a response such as PASI 75 that is the wrong way round, and the
table above passes `small_values = "undesirable"`.

## Click panels

With `data`, `study` and `treatment`, the arm level data, a click on a
cell opens the arms of every direct trial side by side, in the same
table the network plot uses, with every other column of the data as a
row. Without them, the panel lists each direct trial’s own estimate:

``` r

ggleague(nma, small_values = "undesirable", ranking = FALSE)
```

## Options

| argument  | effect                                                         |
|-----------|----------------------------------------------------------------|
| `pooled`  | `"random"` or `"common"`; defaults to the random effects model |
| `order`   | the order of the treatments; defaults to the ranking           |
| `ranking` | draw the P-score ranking beside the table                      |
| `caption` | replaces the default note on which estimate sits where         |

Like the other graphs, the table prints as a widget, and
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
and
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
give it as a ggplot, an htmlwidget or a file.
