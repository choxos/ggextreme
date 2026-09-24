# Nomograms

A nomogram turns a regression model into something a clinician can use
with a pencil: one axis per predictor, scaled in points, a total, and an
axis that reads the prediction off the total.
[`ggnomogram()`](https://choxos.github.io/ggextreme/reference/ggnomogram.md)
draws one from a fitted model and gives every predictor a handle. Drag a
handle to a patient’s value, click a category or use the arrow keys, and
the points, the total and the prediction with its 95% confidence
interval follow, computed in the page from the model’s coefficients and
their covariance.

``` r

library(ggextreme)
```

## A logistic model

Hosmer and Lemeshow’s low birth weight data ship with MASS. This model
has a spline for the mother’s age and an interaction between smoking and
hypertension, which a nomogram has to handle:

``` r

bw <- MASS::birthwt
bw$race <- factor(bw$race, labels = c("White", "Black", "Other"))
bw$smoke <- factor(bw$smoke, labels = c("No", "Yes"))
bw$ht <- factor(bw$ht, labels = c("No", "Yes"))
fit <- glm(low ~ splines::ns(age, 3) + lwt + race + smoke * ht,
           family = binomial, data = bw)

ggnomogram(
  fit,
  outcome = "Risk of low birth weight",
  labels = c(age = "Mother's age (years)", lwt = "Weight at last period (lb)",
             race = "Race", smoke = "Smoked in pregnancy", ht = "Hypertension"),
  title = "Low birth weight"
)
```

The age effect falls, rises and falls again across its range, so its
axis is folded onto three lines, the way rms draws a nonlinear term:
drag the handle along whichever line holds the age you want.
Hypertension’s effect depends on smoking, so its axis is drawn for the
current smoking status and redraws when that changes; the note under its
label says which. The prediction is the same one
[`predict()`](https://rdrr.io/r/stats/predict.html) gives:

``` r

n <- ggnomogram(fit)
nomogram_predict(n, list(age = 25, lwt = 120, race = "Black", smoke = "Yes", ht = "No"))
#> $points
#> [1] 240.5572
#> 
#> $lp
#> [1] 1.140621
#> 
#> $se
#> [1] 0.6097766
#> 
#> $outputs
#>               output  estimate     lower     upper
#> 1 Probability of low 0.7577936 0.4863736 0.9117958
```

[`nomogram_predict()`](https://choxos.github.io/ggextreme/reference/nomogram_predict.md)
computes predictions the way the widget does, so it is a way to check a
nomogram or to read predictions for a list of patients.

## A Cox model

For a Cox model the nomogram predicts survival at the `times` given,
with the same interval
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
gives for the patient. A stratified model, here by sex, gets a choice of
stratum under the plot, since each stratum has its own baseline:

``` r

lung <- survival::lung
lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
cox <- survival::coxph(survival::Surv(time, status) ~ age + ph.ecog + survival::strata(sex),
                       data = lung)
ggnomogram(cox, times = c("6 months" = 182, "1 year" = 365),
           labels = c(age = "Age (years)", ph.ecog = "ECOG performance status"),
           title = "Survival in advanced lung cancer")
```

## Ordinal and mixed models

An ordinal model gets one axis for the probability of each category or
above, and the probability of every category beside the plot:

``` r

bw$visits <- factor(pmin(bw$ftv, 2), labels = c("None", "One", "Two or more"))
ggnomogram(MASS::polr(visits ~ age + race, data = bw, Hess = TRUE),
           labels = c(age = "Mother's age (years)"),
           title = "Physician visits in the first trimester")
```

A mixed model predicts for a typical cluster, with the random effects at
zero. On a nonlinear link, as in a logistic mixed model, that is the
prediction for an average cluster, not the average over clusters, and
the interval reflects the fixed effects only; the section under the plot
says so.

``` r

sleep <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
ggnomogram(sleep, outcome = "Reaction time (ms)",
           labels = c(Days = "Days of sleep deprivation"))
```

## Models it reads

| model | prediction |
|----|----|
| [`lm()`](https://rdrr.io/r/stats/lm.html), [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html), [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html), [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) | the mean, with a prediction interval for [`lm()`](https://rdrr.io/r/stats/lm.html) |
| [`glm()`](https://rdrr.io/r/stats/glm.html), [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html), [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html), [`logistf::logistf()`](https://rdrr.io/pkg/logistf/man/logistf.html), [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html), [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) | the mean on the response scale through the link |
| [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html), [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html), [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | the prediction for a typical cluster |
| [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html) | survival at `times`, by stratum |
| [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html), [`rms::psm()`](https://rdrr.io/pkg/rms/man/psm.html) | the median survival time and survival at `times` |
| [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html), [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html), [`rms::orm()`](https://rdrr.io/pkg/rms/man/orm.html) | the probability of each category or above |
| [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | the odds of each category against the reference, and every probability |

Every class is checked against its package’s own
[`predict()`](https://rdrr.io/r/stats/predict.html) in the package’s
tests. Transformations, polynomials and splines work because the points
come from the model’s design matrix. Interactions between two numeric
predictors are drawn with the second axis for the current value of the
first; three or more interacting numeric predictors cannot be drawn.

## Options

| argument | effect |
|----|----|
| `values` | the patient the widget starts at, and the static copy’s conditions |
| `labels` | axis labels for the predictors |
| `ranges` | ranges of numeric axes; the default runs from the 2.5th to the 97.5th percentile |
| `outcome` | labels of the prediction axes |
| `times` | times for the survival axes of a survival model |
| `level` | the category a static copy of a multinomial nomogram is scaled on |

A static copy from
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
is the classic printed nomogram, without the handles; on a dark page the
widget takes a dark palette of its own.
