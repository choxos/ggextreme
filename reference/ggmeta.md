# Draw an interactive forest plot for a meta-analysis

Draws the forest plot of a fitted meta-analysis, with the data behind
every study a hover and a click away. Hovering over a study shows its
effect, weight and the columns named in `hover`; clicking it opens its
full record, every column of `data`, under the plot. Hovering over the
pooled diamond shows the heterogeneity statistics and the prediction
interval. Risk of bias judgments, given in `rob`, are drawn as traffic
lights beside each study.

## Usage

``` r
ggmeta(
  x,
  data = NULL,
  columns = NULL,
  rob = NULL,
  hover = NULL,
  cumulative = FALSE,
  exponentiate = NULL,
  xlim = NULL,
  favors = NULL,
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

- columns:

  Names of columns of `data` to print as text columns beside the study
  labels, such as event counts. Name the vector to set the column
  headers.

- rob:

  Names of columns of `data` holding risk of bias judgments, one per
  domain, drawn as traffic lights. Name the vector to set the column
  headers, such as `c(D1 = "rob.R", Overall = "rob.overall")`. Judgments
  are matched by their wording: "low", "some concerns" or "moderate",
  "unclear", "high" or "serious", "critical", and "no information", in
  any case.

- hover:

  Names of columns of `data` shown in each study's hover card. Defaults
  to `columns`.

- cumulative:

  Show the cumulative meta-analysis rather than the individual studies.

- exponentiate:

  Show effects on the ratio scale. Defaults to `TRUE` for ratio measures
  (`RR`, `OR`, `HR`, `IRR`, `ROM` and Peto odds ratios), which are
  modeled on the log scale.

- xlim:

  Optional limits of the effect axis, on the scale shown.

- favors:

  Optional labels for the two sides of the null line, such as
  `c("Favors treatment", "Favors control")`.

- xlab:

  Axis label. Defaults to the name of the effect measure.

- title, caption:

  Title above the plot and note below it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `ggmeta`, which prints as an interactive widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file, and
[`animate_meta()`](https://choxos.github.io/ggextreme/reference/animate_meta.md)
for the cumulative replay.

## Details

Square area follows the study's weight in the model. The diamond is the
pooled estimate with its confidence interval, and the line through it
the prediction interval for a random effects model. Ratio measures such
as risk, odds and hazard ratios are shown on a log axis. A confidence
interval that runs past the axis ends in an arrow.

With `cumulative = TRUE`, each row shows the pooled estimate from the
studies up to and including it, in the order of the data, so sort the
data by year before fitting the model for a cumulative meta-analysis
over time.
[`animate_meta()`](https://choxos.github.io/ggextreme/reference/animate_meta.md)
replays that sequence as an animation.

## Examples

``` r
if (requireNamespace("metafor", quietly = TRUE)) {
  dat <- metafor::escalc(
    measure = "OR", ai = p2y12.mi, n1i = p2y12.total,
    ci = aspirin.mi, n2i = aspirin.total,
    data = metadat::dat.chiarito2020, slab = paste(study, year)
  )
  dat <- dat[!is.na(dat$yi), ]
  dat$p2y12 <- paste0(dat$p2y12.mi, "/", dat$p2y12.total)
  dat$aspirin <- paste0(dat$aspirin.mi, "/", dat$aspirin.total)
  fit <- metafor::rma(yi, vi, data = dat)

  ggmeta(
    fit,
    columns = c("P2Y12 inhibitor" = "p2y12", Aspirin = "aspirin"),
    rob = c(R = "rob.R", D = "rob.D", Mi = "rob.Mi", Me = "rob.Me",
            S = "rob.S", Overall = "rob.overall"),
    favors = c("Favors P2Y12 inhibitor", "Favors aspirin")
  )
}
```
