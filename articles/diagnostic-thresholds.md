# Diagnostic thresholds

A cutoff turns a continuous test into a yes or a no. Where it sits
decides how many people with the condition are missed and how many
without it are sent on for more tests, and what a positive result means
depends on how common the condition is where the test is used.
[`ggdiagnostic()`](https://choxos.github.io/ggextreme/reference/ggdiagnostic.md)
shows all of it at once, for any cutoff and any prevalence.

``` r

library(ggextreme)
```

## A first explorer

The Pima Indians diabetes data in MASS record plasma glucose two hours
after a glucose load. A cutoff of 126 mg/dL is taken as prespecified
here, and the test is imagined in a population where one person in ten
has diabetes:

``` r

pima <- rbind(MASS::Pima.tr, MASS::Pima.te)
dx <- ggdiagnostic(type ~ glu, pima, cutoff = 126, prevalence = 0.1,
                   labels = c("No diabetes", "Diabetes"),
                   marker_label = "Plasma glucose (mg/dL)",
                   title = "Plasma glucose for diabetes")
dx
```

Drag the cutoff across the distributions, or use the slider above the
plot. The shaded tails are the people who test positive, the point on
the ROC curve and its crosshair of intervals move with it, and the grid
of 1,000 people recolors: found, missed, false alarms and correctly
cleared. The second slider sets the prevalence, which changes the
predictive values and the grid but not sensitivity or specificity. The
sentence above the plot says what the current setting does.

``` r

dx$accuracy
#>       measure  estimate     lower     upper
#> 1 sensitivity 0.6666667 0.5943294 0.7319232
#> 2 specificity 0.7690141 0.7224322 0.8098363
#> 3         ppv 0.2428181 0.2052492 0.2847992
#> 4         npv 0.9540513 0.9435977 0.9626441
#> 5 lr_positive 2.8861789 2.3243049 3.5838794
#> 6 lr_negative 0.4334554 0.3492499 0.5379633
dx$auc
#> $auc
#> [1] 0.7939763
#> 
#> $lower
#> [1] 0.753043
#> 
#> $upper
#> [1] 0.8349096
```

## What to keep in mind

- A cutoff chosen by looking at the same data, such as the one that
  maximizes Youden’s index, which is the default when `cutoff` is not
  given, looks better here than it will in new patients. The plot says
  so.
- The prevalence in a case control sample is not the prevalence in
  practice. Set `prevalence` to the one that applies.
- A test cutoff is not a treatment threshold: deciding who to treat also
  weighs the harms of treatment.

## Intervals

Sensitivity and specificity have Wilson score intervals, the predictive
values the logit intervals of Mercaldo and colleagues, the likelihood
ratios the log method and the area under the curve DeLong’s. An interval
that needs a count of zero is not shown.

## Options

| argument | effect |
|----|----|
| `direction` | whether `"higher"` or `"lower"` values point to the condition; `"auto"` by default |
| `labels` | names for those without and with the condition |
| `marker_label` | the marker’s axis label, with its unit |
| `level` | the confidence level |
