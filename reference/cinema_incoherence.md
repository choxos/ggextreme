# Draw direct, indirect and network estimates side by side

Draws, for every comparison of a network meta-analysis, the direct
estimate from the studies that compare the pair head to head, the
indirect estimate from the rest of the network and the network estimate
that combines them, as separated by
[`netmeta::netsplit()`](https://rdrr.io/pkg/netmeta/man/netsplit.html).
Beside them is the inconsistency factor, the ratio of the direct to the
indirect estimate for a ratio measure or their difference otherwise,
with its confidence interval and p-value, and the incoherence judgment
that CINeMA's rules give.

## Usage

``` r
cinema_incoherence(x, ..., title = NULL, caption = NULL, family = "Lato")
```

## Arguments

- x:

  Judgments from
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md),
  or a network meta-analysis from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html),
  judged with the arguments in `...`.

- ...:

  When `x` is a netmeta fit, arguments for
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md),
  such as `threshold`, which shades the range of little difference and
  lets the rule judge comparisons whose test gives p of 0.10 or less,
  `split`, `small_values`, `order` and `contributions`, which list the
  studies behind each indirect estimate.

- title, caption:

  Title above the plot and note below it. A caption you give is added
  above the default notes.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `cinema_incoherence`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `comparisons` holds
the direct, indirect and network estimates and the inconsistency factors
on the scale of the effect, and `global` the design by treatment test.

## Details

A comparison with only direct or only indirect evidence cannot be
checked this way. It is kept visibly apart, marked as not assessable
locally, because the absence of a test is not agreement; CINeMA then
judges it from the global design by treatment interaction test, given
under the plot. Both tests have low power, above all with few studies,
so a large p-value is weak evidence that direct and indirect evidence
agree, and the interval of the inconsistency factor shows how large a
disagreement the data still allow.

Hovering over a comparison gives its numbers; clicking it, or pressing
Enter on it, opens a panel that says in words how far direct and
indirect evidence could disagree, lists the studies behind each estimate
and gives the reason for the judgment.

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
  cinema_incoherence(nma, threshold = 1.25, small_values = "undesirable")
}
# }
```
