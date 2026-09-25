# Draw network estimates against a range of little difference

Draws every network estimate of a network meta-analysis with its
confidence interval and prediction interval against a range of little
difference, whose lower and upper limits the reader can move on their
own. Beside each estimate, in words, is what its confidence interval and
its prediction interval are compatible with: an important benefit,
little difference or an important harm of the first treatment against
the second, or more than one of these; and the imprecision and
heterogeneity judgments that CINeMA's rules give at these limits.

## Usage

``` r
cinema_clinical(
  x,
  ...,
  reference = NULL,
  prediction = TRUE,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- x:

  Judgments from
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md)
  that include a `threshold`, or a network meta-analysis from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html),
  judged with the arguments in `...`.

- ...:

  When `x` is a netmeta fit, arguments for
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md):
  `threshold`, the prespecified limits of little difference, which is
  required, and `small_values`, `order` and `pooled`.

- reference:

  Optionally, one treatment: only its comparisons with the others are
  drawn, such as every treatment against placebo.

- prediction:

  Draw the prediction intervals, when the model has them.

- title, caption:

  Title above the plot and note below it. A caption you give is added
  above the default notes.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `cinema_clinical`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `readings` gives,
for each comparison drawn, what its intervals are compatible with and
the imprecision and heterogeneity judgments at the prespecified limits.

## Details

In the widget, two sliders move the lower and the upper limit; the
prespecified limits stay marked with dashed lines and a button returns
to them. Every reading, judgment and the summary under the plot follow.
Under the estimates, a threshold sensitivity strip for each limit shows,
row by row, where each reading changes as that limit moves with the
other held where it is: a change of color is a change of reading.
Clicking or tapping a strip moves that limit there; the sliders do the
same from the keyboard. A switch hides the prediction intervals.

The limits apply to every comparison as written, the first treatment
against the second, in the order set by
[`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md).
A prediction interval shows where the effect in a comparable new setting
is expected to lie, not the effect for an individual patient, and with
few studies the heterogeneity behind it is poorly estimated. The plot
describes what each interval is compatible with; it never calls a
comparison equivalent.

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
  cinema_clinical(nma, threshold = c(0.8, 1.25),
                  small_values = "undesirable")
}
# }
```
