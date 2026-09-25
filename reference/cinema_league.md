# Draw a league table with a CINeMA confidence profile in every cell

Draws every pairwise estimate of a network meta-analysis as a grid laid
out like
[`ggleague()`](https://choxos.github.io/ggextreme/reference/ggleague.md)
and
[`netmeta::netleague()`](https://rdrr.io/pkg/netmeta/man/netleague.html):
each cell compares the treatment that comes first on the diagonal with
the one that comes later, network estimates sit below the diagonal and
direct estimates above it. Under each network estimate six small marks
give its judgment in each CINeMA domain, left to right within-study
bias, reporting bias, indirectness, imprecision, heterogeneity and
incoherence, colored by no, some or major concerns. The marks are never
added into a score.

## Usage

``` r
cinema_league(x, ..., title = NULL, caption = NULL, family = "Lato")
```

## Arguments

- x:

  Judgments from
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md),
  or a network meta-analysis from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html),
  which is then judged with the arguments in `...`.

- ...:

  When `x` is a netmeta fit, arguments for
  [`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md):
  the study judgments `rob` and `indirectness`, `reporting`,
  `threshold`, `rule`, your own `judgments`, `small_values` and `order`.
  Giving only `judgments`, such as a report exported from the CINeMA web
  application, draws your judgments as they are.

- title, caption:

  Title above the table and note below it. The default caption says
  which estimates sit on each side of the diagonal and, domain by
  domain, which judgments were computed by which rule and which are
  yours. A caption you give is added above it.

- family:

  Font family. The package ships Lato and registers it on load.

## Value

An object of class `cinema_league`, which prints as an interactive
widget. Use
[`graph_widget()`](https://choxos.github.io/ggextreme/reference/graph_widget.md),
[`graph_plot()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
or
[`graph_save()`](https://choxos.github.io/ggextreme/reference/graph_widget.md)
for the widget, a static ggplot or a file. The field `judgments` holds
the result of
[`cinema_judge()`](https://choxos.github.io/ggextreme/reference/cinema_judge.md)
the table was drawn from.

## Details

Hovering over a cell lists its judgments; clicking it opens a panel
under the table with every domain's judgment, the reason for it and
whether it was computed by a rule or given by you, the estimate against
the range of little difference, and a bar of the contribution of each
study to the estimate, colored by its risk of bias. The cells can also
be reached with the keyboard: Tab moves between them and Enter or Space
opens one. Hovering over a treatment on the diagonal lights its row and
column.

A mark drawn hollow is a domain not judged, such as reporting bias when
you gave no judgment for it. A round mark in the incoherence place means
there was no local test for that comparison, only the global test of the
whole network, as CINeMA prescribes.

## Rules

Every rule below is the one CINeMA implements, as published; where the
papers leave a detail open the choice made here is stated.

- **Within-study bias** and **indirectness** combine the study judgments
  with the percentage contribution of each study to each estimate
  (Papakonstantinou et al. 2018), from
  [`netmeta::netcontrib()`](https://rdrr.io/pkg/netmeta/man/netcontrib.html).
  The *majority* rule takes the level with the largest total
  contribution, the more serious level on a tie; the *average* rule
  scores low 1, moderate 2 and high 3, averages the scores weighted by
  contribution and rounds, halves up; the *highest* rule takes the most
  serious level among the studies that contribute more than 0.0001
  percent. Low, moderate and high become no, some and major concerns.

- **Reporting bias** is your judgment; CINeMA suggests suspected or
  undetected, and suspected is shown as some concerns.

- **Imprecision** compares the confidence interval with the range of
  little difference. There are no concerns when the interval lies wholly
  within the range, or wholly on the side of no effect that the point
  estimate is on; some concerns when it crosses no effect but not the
  limit on the other side; and major concerns when it passes that limit,
  so that it holds important effects in both directions.

- **Heterogeneity** judges the prediction interval by the same rule.
  There are no concerns when it reaches the same step as the confidence
  interval, some concerns when it reaches one step further and major
  concerns when it reaches two (Table 4 of Papakonstantinou et al. 2020;
  this reproduces every scenario in Figure 3 of Nikolakopoulou et al.
  2020). A common effect model has no prediction interval, so
  heterogeneity is then not judged.

- **Incoherence**, for a comparison with direct and indirect evidence,
  uses the test of the difference between them from
  [`netmeta::netsplit()`](https://rdrr.io/pkg/netmeta/man/netsplit.html)
  (SIDE). With p above 0.10 there are no concerns. Otherwise the areas
  below, within and above the range of little difference are compared:
  when both confidence intervals reach the same areas there are no
  concerns, when they differ in one area some concerns, and when they
  differ in two or three major concerns. A comparison with only direct
  or only indirect evidence cannot be tested locally, and takes its
  judgment from the global design by treatment interaction test of
  [`netmeta::decomp.design()`](https://rdrr.io/pkg/netmeta/man/decomp.design.html):
  major concerns below 0.05, some from 0.05 to 0.10 and no concerns
  above; when the network has no closed loop, so the test cannot be
  computed, major concerns. Both tests have low power.

CINeMA's authors stress that these rules are a starting point: the
reasons say what each rule saw, so a judgment can be revised by giving
it in `judgments`. CINeMA also leaves any overall rating to the
reviewers; this function gives none, though a rating you supply is kept
and shown.

## Sources

Nikolakopoulou A, Higgins JPT, Papakonstantinou T, et al. CINeMA: an
approach for assessing confidence in the results of a network
meta-analysis. PLoS Medicine 2020;17(4):e1003082.
[doi:10.1371/journal.pmed.1003082](https://doi.org/10.1371/journal.pmed.1003082)

Papakonstantinou T, Nikolakopoulou A, Higgins JPT, Egger M, Salanti G.
CINeMA: software for semiautomated assessment of the confidence in the
results of network meta-analysis. Campbell Systematic Reviews
2020;16:e1080. [doi:10.1002/cl2.1080](https://doi.org/10.1002/cl2.1080)

Papakonstantinou T, Nikolakopoulou A, Rucker G, et al. Estimating the
contribution of studies in network meta-analysis: paths, flows and
streams. F1000Research 2018;7:610.

## Examples

``` r
# \donttest{
if (requireNamespace("netmeta", quietly = TRUE) &&
    requireNamespace("meta", quietly = TRUE)) {
  pw <- meta::pairwise(treat = treatment, event = pasi75_r,
                       n = pasi75_n, studlab = study,
                       data = psoriasis_nma, sm = "OR")
  nma <- netmeta::netmeta(pw, common = FALSE)
  # Illustrative judgments, invented for this example.
  rob <- data.frame(
    study = c("CLEAR", "ERASURE", "FEATURE", "FIXTURE", "JUNCTURE"),
    judgment = c("high", "low", "some concerns", "low", "some concerns")
  )
  cinema_league(nma, rob = rob, threshold = 1.25,
                small_values = "undesirable",
                caption = "Illustrative judgments, not published assessments.")
}
# }
```
