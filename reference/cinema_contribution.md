# Draw where each network estimate's evidence comes from, study by study

Draws the contribution of every study to every estimate of a network
meta-analysis, from `netmeta::netcontrib(x, study = TRUE)`: one bar per
comparison, split into studies as wide as their share of the estimate
and colored by each study's risk of bias or indirectness, with the
studies grouped low, moderate and high as CINeMA draws them; a
comparison by study matrix of the same numbers, whose squares mark with
an outline the studies that compare that pair head to head; and a small
network.

## Usage

``` r
cinema_contribution(
  x,
  ...,
  color = NULL,
  positions = NULL,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- x:

  Judgments from
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md)
  made with study judgments, or a network meta-analysis from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html),
  judged with the arguments in `...`.

- ...:

  When `x` is a netmeta fit, arguments for
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md):
  the study judgments `rob` and `indirectness`, at least one of them,
  and optionally `contributions`, `small_values`, `order` and `pooled`.

- color:

  Which judgment colors the studies at first, `"rob"` or
  `"indirectness"`. Defaults to risk of bias when it was given.

- positions:

  Optional data frame placing the treatments of the small network by
  hand, with columns `treatment`, `x` and `y` and `y` pointing up, as in
  [`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md).
  Defaults to a circle.

- title, caption:

  Title above the plot and note below it. A caption you give is added
  above the default notes.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `cinema_contribution`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `contributions`
holds the share of each estimate from each study.

## Details

In the widget, a switch colors the studies by risk of bias or by
indirectness. Selecting a comparison, from its label, the matrix or the
menu, lights its bar and matrix row and widens each line of the small
network by the share of the estimate that flows through it. Selecting a
study, from the matrix, its button or the panel, dims the others and
marks the comparisons it randomized. Clicking a part of a bar lists the
studies with that judgment in that estimate, with their shares and the
reasons for their judgments. The labels and matrix headers can be
reached with the keyboard, and a sentence under the controls says what
the selection shows.

Contributions say where an estimate's information comes from, not how
trustworthy it is. Dropping a study would change the whole fit, so there
is deliberately no switch that removes one and rescales the bars.

## Sources

Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: an
approach for assessing confidence in the results of a network
meta-analysis. PLoS Medicine 2020;17(4):e1003082.
[doi:10.1371/journal.pmed.1003082](https://doi.org/10.1371/journal.pmed.1003082)

Papakonstantinou T, Nikolakopoulou A, Higgins JPT, Egger M, Salanti G.
CINeMA: software for semiautomated assessment of the confidence in the
results of network meta-analysis. Campbell Systematic Reviews
2020;16:e1080. [doi:10.1002/cl2.1080](https://doi.org/10.1002/cl2.1080)

Papakonstantinou T, Nikolakopoulou A, Rucker G, et al. Estimating the
contribution of studies in network meta-analysis: paths, flows and
streams. F1000Research 2018;7:610.

## Examples

``` r
# \donttest{
if (requireNamespace("netmeta", quietly = TRUE) &&
    requireNamespace("meta", quietly = TRUE)) {
  pw <- meta::pairwise(treat = treatment, event = pasi75_r,
                       n = pasi75_n, studlab = study,
                       data = psoriasis_nma, sm = "OR")
  nma <- netmeta::netmeta(pw, common = FALSE)
  # Illustrative judgments, invented for this example.
  rob <- data.frame(
    study = c("CLEAR", "ERASURE", "FEATURE", "FIXTURE", "JUNCTURE"),
    judgment = c("high", "low", "some concerns", "low", "some concerns")
  )
  cinema_contribution(nma, rob = rob, small_values = "undesirable",
                      caption = "Illustrative judgments, not published assessments.")
}
# }
```
