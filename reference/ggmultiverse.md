# Draw an interactive multiverse of analyses

Draws a specification curve: every reasonable analysis of the same
question, one per row of `data`, sorted by its estimate, with its
confidence interval, above a grid that marks the choices each analysis
made. The primary analysis stays marked, the reference value is drawn,
and beside each choice sits the median estimate of the analyses that
made it, so the choices that move the result stand out.

## Usage

``` r
ggmultiverse(
  data,
  estimate,
  lower,
  upper,
  decisions,
  primary = NULL,
  ratio = NULL,
  ylab = "Estimate",
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- data:

  A data frame with one row per analysis.

- estimate, lower, upper:

  Bare columns of `data` with each estimate and its confidence interval.

- decisions:

  Names of the columns of `data` that hold the choices, such as the
  outcome definition, the adjustment set and the model.

- primary:

  Optional logical expression of the columns of `data`, or a row number,
  marking the primary analysis.

- ratio:

  Whether the estimates are ratios, drawn on a log scale around 1.
  Defaults to `TRUE` when every lower limit is positive.

- ylab:

  Label of the estimate axis, such as `"Odds ratio"`.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggmultiverse`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `influence` holds
the median estimate for every choice.

## Details

In the widget, hovering over an analysis shows its estimate and every
choice behind it. Dragging across the curve selects a run of analyses
and says which choices they share. Clicking a choice in the grid keeps
only the analyses that made it, and several choices can be combined; the
primary analysis is never hidden.

The share of analyses with intervals that exclude the reference value is
a description of this set of analyses, not a probability that the effect
is real, and the analyses are not independent.

## Examples

``` r
specs <- expand.grid(outcome = c("Primary", "Broad"),
                     adjustment = c("Minimal", "Standard", "Extended"),
                     model = c("Logistic", "Log-binomial"),
                     stringsAsFactors = FALSE)
set.seed(1)
log_or <- -0.3 + 0.12 * (specs$outcome == "Broad") +
  0.1 * match(specs$adjustment, c("Minimal", "Standard", "Extended")) +
  rnorm(nrow(specs), 0, 0.03)
specs$or <- exp(log_or)
specs$lo <- exp(log_or - 1.96 * 0.09)
specs$hi <- exp(log_or + 1.96 * 0.09)
ggmultiverse(specs, or, lo, hi, decisions = c("outcome", "adjustment", "model"),
             primary = outcome == "Primary" & adjustment == "Standard" & model == "Logistic",
             ylab = "Odds ratio")
```
