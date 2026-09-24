# Arm level data from five trials in plaque psoriasis

Baseline characteristics and PASI 75 response for the 15 arms of five
randomized trials in moderate to severe plaque psoriasis: CLEAR,
ERASURE, FEATURE, FIXTURE and JUNCTURE. The trials compare secukinumab
at two doses with placebo, etanercept and ustekinumab, and most have
more than two arms, which makes a small but well connected network for
[`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md).

## Usage

``` r
psoriasis_nma
```

## Format

A data frame with 15 rows, one per arm, and 15 columns:

- study:

  Trial name.

- treatment:

  Treatment, a factor with placebo first.

- class:

  Drug class, a factor with four levels.

- n:

  Participants randomized.

- pasi75_r, pasi75_n:

  Participants with a PASI 75 response, and the number analyzed.

- age:

  Mean age in years.

- male:

  Percentage of men.

- weight:

  Mean weight in kilograms.

- bmi:

  Mean body mass index.

- pasi_w0:

  Mean PASI score at baseline.

- duration:

  Mean duration of psoriasis in years.

- prior_systemic:

  Percentage with previous systemic treatment.

- psa:

  Percentage with psoriatic arthritis.

- reference:

  The trial's main publication, with its DOI.

## Source

Aggregate data compiled by Phillippo (2019) and distributed as
`plaque_psoriasis_agd` in the 'multinma' package. The analysis that uses
them is Phillippo DM, Dias S, Ades AE, et al. (2020). Multilevel network
meta-regression for population-adjusted treatment comparisons. Journal
of the Royal Statistical Society Series A 183(3): 1189-1210.
[doi:10.1111/rssa.12579](https://doi.org/10.1111/rssa.12579) . Trial
references were checked against PubMed.

## Details

Every column except `study`, `treatment` and `class` carries a `label`
attribute, which
[`ggnma()`](https://choxos.github.io/ggextreme/reference/ggnma.md) uses
to name the rows of its arm tables.

## Examples

``` r
psoriasis_nma[c("study", "treatment", "n", "pasi75_r")]
#>       study          treatment   n pasi75_r
#> 1     CLEAR        Ustekinumab 339      265
#> 2     CLEAR Secukinumab 300 mg 337      304
#> 3   ERASURE            Placebo 248       11
#> 4   ERASURE Secukinumab 150 mg 245      174
#> 5   ERASURE Secukinumab 300 mg 245      200
#> 6   FEATURE            Placebo  59        0
#> 7   FEATURE Secukinumab 150 mg  59       41
#> 8   FEATURE Secukinumab 300 mg  59       44
#> 9   FIXTURE            Placebo 326       16
#> 10  FIXTURE         Etanercept 326      142
#> 11  FIXTURE Secukinumab 150 mg 327      219
#> 12  FIXTURE Secukinumab 300 mg 327      249
#> 13 JUNCTURE            Placebo  61        2
#> 14 JUNCTURE Secukinumab 150 mg  61       43
#> 15 JUNCTURE Secukinumab 300 mg  60       52
attr(psoriasis_nma$age, "label")
#> [1] "Mean age (years)"
```
