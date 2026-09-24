# Draw an interactive nomogram for a regression model

Draws the nomogram of a fitted regression model: one axis per predictor,
scaled in points, a total points axis and one or more axes that turn the
total into a prediction. In the widget every predictor has a handle.
Dragging it, clicking a category or using the arrow keys sets a
patient's values, and the points, the total and the prediction with its
95% confidence interval follow, computed in the page from the model's
coefficients and their covariance.

## Usage

``` r
ggnomogram(
  fit,
  data = NULL,
  values = NULL,
  labels = NULL,
  ranges = NULL,
  outcome = NULL,
  times = NULL,
  level = NULL,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- fit:

  A fitted regression model. See Details for the classes read.

- data:

  The data the model was fitted to. Defaults to the data named in the
  model's call, which is enough when that data is still around.

- values:

  Starting values for the predictors, as a named list. The others start
  at the median, or at the most common category.

- labels:

  Axis labels for the predictors, as a named character vector. The
  others use the column's `label` attribute or its name.

- ranges:

  Ranges for numeric axes, as a named list of pairs.

- outcome:

  Label of the prediction axis. For a model with several, a character
  vector with one label each.

- times:

  For a survival model, the times to predict survival at, named to label
  the axes, such as `c("1 year" = 365, "5 years" = 1826)`. Defaults to
  the median follow-up time.

- level:

  For a multinomial model, the category whose nomogram the static copy
  draws. Defaults to the first after the reference.

- title, caption:

  Title above the nomogram and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggnomogram`, which prints as an interactive widget.
Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file.
[`nomogram_predict()`](https://choxos.github.io/ggextreme/reference/nomogram_predict.md)
gives the prediction for any set of values, as the widget computes it.

## Details

Contributions are computed through the model's design matrix, so
transformed and nonlinear terms, such as `log(x)`,
[`poly()`](https://rdrr.io/r/stats/poly.html), `ns()`, `rcs()` or a
smooth from 'mgcv', are drawn as they were fitted. An axis whose effect
rises and then falls is folded onto more than one line, as rms does.
When predictors interact, the axis of a later predictor in the
interaction is drawn for the current values of the earlier ones and
redraws as they change. Numeric axes run from the 2.5th to the 97.5th
percentile of the data unless `ranges` says otherwise.

The prediction depends on the model:

- Linear models:

  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html),
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) and Gaussian
  models with an identity link: the predicted mean, with a prediction
  interval for [`lm()`](https://rdrr.io/r/stats/lm.html).

- Generalized linear models:

  [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html),
  [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html),
  [`logistf::logistf()`](https://rdrr.io/pkg/logistf/man/logistf.html),
  [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) and
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html): the mean on
  the response scale, such as a probability or a rate, through the
  model's link.

- Mixed models:

  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html),
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html),
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) and
  [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html):
  the prediction for a typical cluster, with the random effects at zero.
  On a nonlinear link this is the prediction for that cluster, not the
  average over the population. The interval reflects the fixed effects
  only. For glmmTMB, zero inflation and dispersion models are left out.

- Cox models:

  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) and
  [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html): survival at each
  of `times`, with the interval
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
  gives for the same patient. A stratified model gets a choice of
  stratum.

- Parametric survival models:

  [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html)
  and [`rms::psm()`](https://rdrr.io/pkg/rms/man/psm.html): the median
  survival time and survival at `times`.

- Ordinal models:

  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html),
  [`rms::orm()`](https://rdrr.io/pkg/rms/man/orm.html) and ordinal
  [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html): the probability
  of each category or above, with the probability of every category
  beside the plot.

- Multinomial models:

  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html): one
  nomogram per category, scaled on its odds against the reference
  category, with the probability of every category beside the plot.

## Examples

``` r
bw <- MASS::birthwt
bw$race <- factor(bw$race, labels = c("White", "Black", "Other"))
bw$smoke <- factor(bw$smoke, labels = c("No", "Yes"))
fit <- glm(low ~ age + lwt + race + smoke, family = binomial, data = bw)
n <- ggnomogram(fit, outcome = "Risk of low birth weight",
                labels = c(age = "Age (years)", lwt = "Weight (lb)"))
n
nomogram_predict(n, list(age = 30, lwt = 110, race = "Black", smoke = "Yes"))
#> $points
#> [1] 247.8336
#> 
#> $lp
#> [1] 0.5663902
#> 
#> $se
#> [1] 0.6063791
#> 
#> $outputs
#>                     output  estimate    lower     upper
#> 1 Risk of low birth weight 0.6379298 0.349306 0.8525662
#> 
```
