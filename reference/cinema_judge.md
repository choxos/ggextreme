# Judge confidence in the results of a network meta-analysis

Applies the rules of CINeMA, Confidence in Network Meta-Analysis
(Nikolakopoulou et al. 2020; Papakonstantinou et al. 2020), to every
comparison of a network meta-analysis. Each comparison gets a judgment
of no concerns, some concerns or major concerns in each of six domains:
within-study bias, reporting bias, indirectness, imprecision,
heterogeneity and incoherence, each with its reason in words and a note
of whether it was computed by a rule or given by you. The domains are
kept side by side and never added into a score.

## Usage

``` r
cinema_judge(
  x,
  rob = NULL,
  indirectness = NULL,
  reporting = NULL,
  threshold = NULL,
  rule = "average",
  judgments = NULL,
  small_values = NULL,
  order = NULL,
  pooled = NULL,
  contributions = NULL,
  split = NULL
)
```

## Arguments

- x:

  A network meta-analysis from
  [`netmeta::netmeta()`](https://rdrr.io/pkg/netmeta/man/netmeta.html).

- rob, indirectness:

  Study level judgments of risk of bias and of indirectness: data frames
  with a column `study`, naming every study in the network once, a
  column `judgment`, and optionally a column `reason` in words. A
  judgment is low, moderate or high, written as `"low"`,
  `"some concerns"` (or `"moderate"` or `"unclear"`) and `"high"`, as
  `"l"`, `"m"` and `"h"`, or as 1, 2 and 3, the codes the CINeMA web
  application reads.

- reporting:

  Your judgment of reporting bias, which cannot be computed from the
  data: a data frame with a column `judgment`, `"undetected"` or
  `"suspected"` (`"strongly suspected"` is also read), an optional
  column `reason`, and the comparison it applies to, either as `treat1`
  and `treat2` or as `comparison` (such as `"A:B"` or `"A vs B"`). A
  data frame with a single row and no comparison applies to every
  comparison.

- threshold:

  The limits of the range of little difference, on the scale of the
  effect (an odds ratio, say, not its logarithm), for the first
  treatment of each comparison against the second: one number, such as
  `1.25`, for a range symmetric about no effect (0.8 to 1.25 here), or
  two numbers for independent lower and upper limits, which must lie on
  either side of no effect. A threshold of no effect itself, 1 for a
  ratio or 0 for a difference, treats any effect as important.
  Imprecision and heterogeneity, and incoherence when its test gives p
  of 0.10 or less, need it.

- rule:

  How the study judgments are summarized for each comparison:
  `"average"`, `"majority"` or `"highest"`. Give two, such as
  `c("average", "highest")`, for different rules for within-study bias
  and for indirectness. See the section on rules.

- judgments:

  Domain judgments you made yourself, such as those exported from the
  CINeMA web application, which replace the computed ones. Either wide,
  with the comparison (`treat1` and `treat2`, or `comparison`) and one
  column per domain, named like the domains (`"Within-study bias"`,
  `"Reporting bias"`, `"Indirectness"`, `"Imprecision"`,
  `"Heterogeneity"`, `"Incoherence"`), optional columns such as
  `"Imprecision reason"` and optional `"Confidence rating"` and
  `"Reason(s) for downgrading"` columns; or long, with the comparison
  and columns `domain`, `judgment` and optionally `reason`. A missing or
  empty judgment leaves the computed one in place.

- small_values:

  Whether small values of the effect are `"desirable"`, as for
  mortality, or `"undesirable"`, as for a response. It sets the ranking,
  and so the order of the treatments, and which side of the range is a
  benefit. Defaults to the setting stored in `x`, which is worth
  checking.

- order:

  The order of the treatments. Each comparison is written with the
  treatment that comes first in this order first, and the limits in
  `threshold` apply in that direction. Defaults to the P-score ranking,
  best first.

- pooled:

  Which model to use, `"random"` or `"common"`. Defaults to the random
  effects model when `x` has one.

- contributions:

  The contribution of each study to each estimate: an object from
  `netmeta::netcontrib(x, study = TRUE)`, or `TRUE` to compute it. By
  default it is computed when `rob` or `indirectness` is given, which
  takes a few seconds for a network of a few dozen studies.

- split:

  The direct and indirect estimates: an object from
  [`netmeta::netsplit()`](https://rdrr.io/pkg/netmeta/man/netsplit.html),
  or `NULL` to compute them with its default method, back-calculation.
  Give one computed with `method = "SIDDE"` to use that method instead.

## Value

An object of class `cinema`, a list whose main fields are `judgments`,
one row per comparison and domain with the `level` (0, 1 or 2 for no,
some or major concerns, `NA` when not judged), the `judgment` in words,
the `reason` and its `source`; `comparisons`, the network, direct and
indirect estimates, prediction intervals and inconsistency factors on
the scale of the effect; `contributions`, the share of each estimate
from each study; `studies`, the study judgments; `threshold`, `rule`,
`global` (the design by treatment test) and `fit`. It prints as a table
of the judgments.

## Details

The result feeds the plots of the family, so they share one set of
estimates, contributions and judgments:
[`cinema_contribution()`](https://choxos.github.io/ggextreme/reference/cinema_contribution.md),
[`cinema_clinical()`](https://choxos.github.io/ggextreme/reference/cinema_clinical.md),
[`cinema_incoherence()`](https://choxos.github.io/ggextreme/reference/cinema_incoherence.md)
and
[`cinema_league()`](https://choxos.github.io/ggextreme/reference/cinema_league.md).
Each of them also accepts a netmeta fit and the arguments of this
function.

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
  # Illustrative study judgments, invented for this example: they are
  # not published assessments of these trials.
  rob <- data.frame(
    study = c("CLEAR", "ERASURE", "FEATURE", "FIXTURE", "JUNCTURE"),
    judgment = c("high", "low", "some concerns", "low", "some concerns")
  )
  j <- cinema_judge(nma, rob = rob, threshold = 1.25,
                    small_values = "undesirable",
                    reporting = data.frame(judgment = "undetected"))
  j
  head(j$judgments)
}
#>               treat1             treat2            domain level    judgment
#> 1 Secukinumab 300 mg Secukinumab 150 mg Within-study bias     0 No concerns
#> 2 Secukinumab 300 mg Secukinumab 150 mg    Reporting bias     0  Undetected
#> 3 Secukinumab 300 mg Secukinumab 150 mg      Indirectness    NA  Not judged
#> 4 Secukinumab 300 mg Secukinumab 150 mg       Imprecision     0 No concerns
#> 5 Secukinumab 300 mg Secukinumab 150 mg     Heterogeneity     0 No concerns
#> 6 Secukinumab 300 mg Secukinumab 150 mg       Incoherence     0 No concerns
#>                                                                                                                                                                                                                                                                                                                                                                             reason
#> 1                                                                                                            Studies with some concerns about risk of bias supply 16.5% (FEATURE and JUNCTURE) and studies at low risk of bias supply 83.5% (FIXTURE and ERASURE). Average rule: scoring low 1, moderate 2 and high 3, the contribution weighted score is 1.17, which rounds to 1.
#> 2                                                                                                                                                                                                                                                                                                                                              Your judgment; no reason was given.
#> 3                                                                                                                                                                                                                                                                           Not judged. Give study judgments in `indirectness` to compute it, or your own judgment in `judgments`.
#> 4                                                                                                                                                                                                                The 95% CI, 1.33 to 2.18, lies entirely on one side of no effect, so no value in it favors Secukinumab 150 mg; the limits of little difference are 0.80 and 1.25.
#> 5 The 95% prediction interval, 1.25 to 2.31, lies entirely on one side of no effect, so no value in it favors Secukinumab 150 mg; the CI stays on the same side of no effect. Both lead to the same conclusion. The between-study variance, common to the network, is estimated as 0.000 from 5 studies; with few studies it, and so the prediction interval, is poorly estimated.
#> 6                                                                                                                         No local test is possible: no independent indirect evidence exists for this comparison. Judged from the whole network instead: the global design by treatment test gives Q = 1.48 on 2 df, p = 0.48, so above 0.10: no concerns. The test has low power.
#>                                                                      source
#> 1 Computed: average rule over the study judgments, weighted by contribution
#> 2                                                                     Yours
#> 3                                                                Not judged
#> 4                                           Computed: CI against the limits
#> 5                          Computed: prediction interval against the limits
#> 6                                 Computed: global design by treatment test
# }
```
