# Draw an interactive diagnostic threshold explorer

Shows what a cutoff on a continuous test means for the people tested.
The marker's distribution in those with and without the condition sits
beside the ROC curve and the predictive values across prevalence, above
a grid of 1,000 people who are found, missed, falsely alarmed or
correctly cleared, and a table of sensitivity, specificity, predictive
values and likelihood ratios with their confidence intervals.

## Usage

``` r
ggdiagnostic(
  formula,
  data,
  cutoff = NULL,
  prevalence = NULL,
  direction = c("auto", "higher", "lower"),
  labels = NULL,
  marker_label = NULL,
  level = 0.95,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- formula:

  A formula `outcome ~ marker`. The outcome is logical, 0 and 1, or a
  factor or text with two values, whose second level (or `TRUE`, or 1)
  means the condition is present. The marker is numeric.

- data:

  A data frame holding both.

- cutoff:

  The prespecified cutoff. A result at or beyond it, in the direction of
  `direction`, is positive. Defaults to the cutoff that maximizes
  Youden's index in these data.

- prevalence:

  The prevalence of the condition where the test will be used, between 0
  and 1. Defaults to the prevalence in `data`.

- direction:

  Whether `"higher"` or `"lower"` values point to the condition.
  `"auto"` picks the direction with an area under the curve of at least
  one half.

- labels:

  Names for those without and with the condition, in that order.

- marker_label:

  Axis label for the marker, with its unit.

- level:

  Confidence level for the intervals.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggdiagnostic`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `accuracy` holds
the measures at the cutoff, `roc` the curve and `auc` the area under it.

## Details

In the widget, dragging the cutoff on the distributions or moving its
slider updates everything at once, and a second slider sets the
prevalence of the population the test will be used in, which changes the
predictive values and the grid but not sensitivity or specificity. A
sentence under the plot says what the current cutoff does in words.

A cutoff chosen in the same data it is judged on looks better than it
will in new patients, so a prespecified `cutoff` is marked as such, and
the default, the cutoff that maximizes Youden's index, is labeled as
chosen in these data. A test cutoff is not a treatment threshold, and in
a case control sample the prevalence in the data is not the prevalence
in practice; set `prevalence` to the one that applies.

## Intervals

Sensitivity and specificity have Wilson score intervals. Predictive
values at a set prevalence use the logit intervals of Mercaldo, Lau and
Zhou (2007), likelihood ratios the log method, and the area under the
curve the method of DeLong, DeLong and Clarke-Pearson (1988). Intervals
that need a count of zero are not shown.

## Examples

``` r
if (requireNamespace("MASS", quietly = TRUE)) {
  pima <- rbind(MASS::Pima.tr, MASS::Pima.te)
  ggdiagnostic(type ~ glu, pima, cutoff = 126, prevalence = 0.1,
               labels = c("No diabetes", "Diabetes"),
               marker_label = "Plasma glucose (mg/dL)")
}
```
