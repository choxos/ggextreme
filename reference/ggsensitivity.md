# Draw an interactive bias and tipping point explorer

Asks how strong unmeasured confounding would have to be to change a
conclusion. The surface covers every pair of strengths an unmeasured
confounder could have, as a risk ratio with the exposure and a risk
ratio with the outcome, and is shaded by what would remain of the result
under it: an effect still clinically important with an interval clear of
the null, an interval clear of the null, an estimate on the same side of
the null, or nothing. Curves mark where each of these is lost, the
E-values for the estimate and for the confidence limit sit on the
diagonal, and measured covariates given as `benchmarks` show how strong
confounding of a known kind was.

## Usage

``` r
ggsensitivity(
  estimate,
  lower,
  upper,
  measure = c("RR", "OR", "HR"),
  rare = FALSE,
  important = NULL,
  benchmarks = NULL,
  max_strength = NULL,
  xlab = "Confounder with the exposure (risk ratio)",
  ylab = "Confounder with the outcome (risk ratio)",
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- estimate, lower, upper:

  The estimate and its confidence interval, on the ratio scale.

- measure:

  `"RR"`, `"OR"` or `"HR"`.

- rare:

  Whether the outcome is rare, below about 15 percent, so that an odds
  ratio or hazard ratio can stand in for a risk ratio.

- important:

  The smallest effect that would matter clinically, on the same scale
  and on the same side of 1 as the estimate, such as 1.25 or 0.8.
  Optional.

- benchmarks:

  Optional data frame of measured covariates to compare with, with
  columns `label`, `exposure` and `outcome`: each covariate's risk ratio
  with the exposure and with the outcome.

- max_strength:

  The largest strength on the axes. Defaults to a little past the
  E-value.

- xlab, ylab:

  Axis labels of the surface.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggsensitivity`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `evalues` holds the
E-values and `benchmarks` the adjusted estimates for each benchmark.

## Details

Beside the surface, the estimate as analyzed is drawn above the estimate
adjusted for a chosen confounder, one for each benchmark, and the ones
at the two E-values. In the widget, clicking the surface, or moving the
two sliders, chooses the confounder, and a sentence says what it would
do.

The adjustment divides the estimate by the bounding factor of Ding and
VanderWeele (2016), which is the most bias a confounder of those
strengths could cause, so the adjusted values are the worst case for
each pair. An odds ratio or a hazard ratio for a common outcome is first
converted to an approximate risk ratio, as VanderWeele and Ding (2017)
propose; set `rare = TRUE` when the outcome is rare, to use it as it is.

## Examples

``` r
ggsensitivity(1.8, 1.4, 2.31, important = 1.25,
              benchmarks = data.frame(label = c("Age", "Smoking"),
                                      exposure = c(1.6, 2.3), outcome = c(1.9, 1.5)))
```
