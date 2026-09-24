# Predict from a nomogram

Gives the linear predictor, its standard error and every prediction the
nomogram shows, for any set of predictor values, computed the way the
widget computes them. Use it to check a nomogram against the model's own
[`predict()`](https://rdrr.io/r/stats/predict.html) method, or to read
predictions for a list of patients.

## Usage

``` r
nomogram_predict(x, values = list())
```

## Arguments

- x:

  A nomogram from
  [`ggnomogram()`](https://choxos.github.io/ggextreme/reference/ggnomogram.md).

- values:

  A named list of predictor values. Predictors left out take their
  starting values.

## Value

A list with the total `points`, the linear predictor `lp` and its
standard error `se`, and `outputs`, a data frame with one row per
prediction and its confidence interval.
