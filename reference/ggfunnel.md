# Draw an interactive funnel plot for a meta-analysis

Draws each study's effect against its standard error, with the most
precise studies at the top, to show small-study effects. The shaded
contours mark where a study would be statistically significant against
no effect (Peters et al. 2008), so a gap in the unshaded area, where
studies would not be significant, points to publication bias rather than
heterogeneity alone. The solid line is the pooled estimate and the
dashed lines around it the region where 95% of studies would fall
without heterogeneity or bias.

## Usage

``` r
ggfunnel(
  x,
  data = NULL,
  rob = NULL,
  hover = NULL,
  contours = c(0.1, 0.05, 0.01),
  trim_fill = FALSE,
  tests = TRUE,
  exponentiate = NULL,
  xlim = NULL,
  xlab = NULL,
  title = NULL,
  caption = NULL,
  family = "Lato"
)
```

## Arguments

- x:

  A fitted meta-analysis: an `rma.uni` object from
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
  or a `meta` object from the 'meta' package, such as the result of
  [`meta::metabin()`](https://rdrr.io/pkg/meta/man/metabin.html) or
  [`meta::metagen()`](https://rdrr.io/pkg/meta/man/metagen.html). Models
  with moderators are not supported.

- data:

  Optional data frame with one row per study, in the order of the model,
  holding the columns to show. Defaults to the data stored in the fit,
  which is there when the model was fitted with a `data` argument.

- rob:

  Name of the column of `data` holding each study's overall risk of bias
  judgement, which colors its point. Judgements are matched by their
  wording, as in
  [`ggmeta()`](https://choxos.github.io/ggextreme/reference/ggmeta.md).

- hover:

  Names of columns of `data` shown in each study's hover card. Defaults
  to `columns`.

- contours:

  Significance levels for the shaded contours, against no effect. `NULL`
  draws none.

- trim_fill:

  Add the studies imputed by trim and fill and the adjusted estimate.

- tests:

  Add the tests for small-study effects, in a collapsed section under
  the plot.

- exponentiate:

  Show effects on the ratio scale. Defaults to `TRUE` for ratio measures
  (`RR`, `OR`, `HR`, `IRR`, `ROM` and Peto odds ratios), which are
  modeled on the log scale.

- xlim:

  Optional limits of the effect axis, on the scale shown.

- xlab:

  Axis label. Defaults to the name of the effect measure.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggfunnel`, which prints as an interactive widget.
Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `tests` holds the
tests as a data frame.

## Details

Hovering over a study shows its effect, weight, risk of bias, the
columns named in `hover` and the significance zone it falls in. Clicking
it opens the pooled estimate with that study left out, beside its full
record.

With `tests = TRUE`, a section under the plot, collapsed until the
reader opens it, gives Egger's regression test (Egger et al. 1997),
Begg's rank correlation test (Begg and Mazumdar 1994) and, with
`trim_fill = TRUE`, the trim and fill estimate (Duval and Tweedie 2000),
as computed by 'metafor' or 'meta', with a note on what each asks.
Egger's test is the classical one, `metafor::regtest(model = "lm")`,
which 'meta' also computes; metafor's own default, `regtest()` with
`model = "rma"`, gives a different p value. The two packages' trim and
fill estimators can also impute different numbers of studies from the
same data. The table is also returned as the field `tests`. These tests
have little power with fewer than ten studies, and asymmetry can come
from heterogeneity, chance or the quality of small studies as well as
from publication bias.

With `trim_fill = TRUE`, the studies trim and fill imputes are drawn as
hollow circles and the adjusted estimate as a dashed line, and the
widget gets a switch that hides them.

## Examples

``` r
if (requireNamespace("metafor", quietly = TRUE)) {
  dat <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg,
                         ci = cpos, di = cneg, data = metadat::dat.bcg,
                         slab = paste(author, year))
  fit <- metafor::rma(yi, vi, data = dat)
  f <- ggfunnel(fit, hover = "alloc", trim_fill = TRUE)
  f
  f$tests
}
#>                      test
#> 1 Egger's regression test
#> 2 Begg's rank correlation
#> 3           Trim and fill
#>                                                    statistic         p
#> 1                                           t = -1.40, 11 df 0.1887070
#> 2                                         Kendall's τ = 0.03 0.9523619
#> 3 1 study imputed on the right; Risk ratio 0.52 (0.37, 0.74)        NA
```
