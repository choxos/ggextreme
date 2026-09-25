# Bias and tipping points

Every observational estimate invites the same question: how strong would
unmeasured confounding have to be to change the conclusion?
[`ggsensitivity()`](https://choxos.github.io/ggextreme/reference/ggsensitivity.md)
answers it for every combination of strengths at once, and marks where
the answer changes.

``` r

library(ggextreme)
```

## A first explorer

Suppose an analysis gives a risk ratio of 1.80 (1.40 to 2.31), that a
risk ratio of 1.25 is the smallest that would matter clinically, and
that the two strongest measured confounders, age and smoking, were
associated with the exposure and the outcome as below:

``` r

bench <- data.frame(label = c("Age", "Smoking"),
                    exposure = c(1.6, 2.3), outcome = c(1.9, 1.5))
s <- ggsensitivity(1.8, 1.4, 2.31, important = 1.25, benchmarks = bench,
                   title = "How strong would confounding have to be?")
s
```

Each point of the surface is an unmeasured confounder with a risk ratio
with the exposure on one axis and with the outcome on the other. The
shading says what would survive it: a clinically important effect with
an interval clear of 1, an interval clear of 1, an estimate on the same
side of 1, or nothing. Click the surface, or use the sliders, to choose
a confounder; the row “Under the chosen confounder” and the sentence
under the plot follow.

``` r

s$evalues
#>             target   evalue
#> 1         estimate 3.000000
#> 2 confidence limit 2.148331
s$benchmarks
#>     label exposure outcome     bias estimate    lower    upper
#> 1     Age      1.6     1.9 1.216000 1.480263 1.151316 1.899671
#> 2 Smoking      2.3     1.5 1.232143 1.460870 1.136232 1.874783
```

The E-value is the strength, on both axes at once, that could just
explain the estimate away; the second E-value does the same for the
confidence limit. A confounder as strong as the benchmarks would leave
the estimate clear of 1, which is some reassurance, not proof: an
unmeasured confounder need not resemble the measured ones.

## The method

The adjustment divides the estimate by the bounding factor of Ding and
VanderWeele, which is the most bias a confounder of given strengths
could produce, so every adjusted value is a worst case. A protective
estimate is handled by symmetry. An odds ratio or hazard ratio for a
common outcome is first converted to an approximate risk ratio; set
`rare = TRUE` when the outcome is rare.

``` r

ggsensitivity(0.62, 0.48, 0.80, measure = "HR", important = 0.8)
```
