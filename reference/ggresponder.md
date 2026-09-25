# Draw an interactive responder threshold plot

Shows what a responder definition keeps and what it throws away. For two
arms, the curves give the share of patients in each arm who improved by
at least each amount, so the whole distribution of change stays in view,
with the prespecified threshold marked. Beside them, the difference in
responders is drawn across every possible threshold with its confidence
band, which shows how much the conclusion depends on where the line is
drawn. Under both, a table gives the responders in each arm, their
difference, the number needed to treat, and the mean difference, which
uses every patient.

## Usage

``` r
ggresponder(
  formula,
  data,
  threshold,
  higher_is_better = TRUE,
  reference = NULL,
  xlab = "Improvement from baseline",
  level = 0.95,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- formula:

  A formula `change ~ arm`, where `change` is each patient's change from
  baseline and `arm` has two values.

- data:

  A data frame holding both.

- threshold:

  The prespecified improvement that makes a responder, in the units of
  `change`, such as a minimal important difference.

- higher_is_better:

  Whether a rise in `change` is an improvement. When `FALSE`, as for
  pain, the change is turned around so that improvement is positive.

- reference:

  The control arm. Defaults to the first level of `arm`.

- xlab:

  Label of the improvement axis, with its unit.

- level:

  Confidence level for the intervals.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggresponder`, which prints as an interactive widget.
Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `responders` holds
the measures at the threshold and `curve` the difference at every
threshold.

## Details

In the widget, a slider or a drag on either panel moves the threshold,
and every number and a sentence follow; a button returns to the
prespecified threshold.

Responders have Wilson score intervals and their difference the hybrid
score interval of Newcombe (1998). The number needed to treat is the
reciprocal of the difference; when the interval of the difference
includes zero, its interval runs from benefit through infinity to harm,
as Altman (1998) describes. The mean difference has a Welch interval.

## Examples

``` r
set.seed(3)
pain <- data.frame(arm = rep(c("Placebo", "Active"), each = 120),
                   change = c(rnorm(120, -1.3, 2), rnorm(120, -2.2, 2)))
ggresponder(change ~ arm, pain, threshold = 2, higher_is_better = FALSE,
            xlab = "Improvement in pain (points on a 0 to 10 scale)")
```
