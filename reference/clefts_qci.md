# Quality of Care Index for orofacial clefts, 1990 to 2019

Yearly Quality of Care Index (QCI) scores for orofacial clefts in
fifteen countries. The QCI is a composite of four secondary indices
derived from Global Burden of Disease estimates and summarized by
principal component analysis, rescaled to run from 0 to 100, where
higher is better care.

## Usage

``` r
clefts_qci
```

## Format

A data frame with 450 rows and 5 columns:

- country:

  Country name.

- iso:

  ISO 3166-1 alpha-2 country code, lower case.

- region:

  World region, a factor with five levels.

- year:

  Year, 1990 to 2019.

- qci:

  Quality of Care Index, 0 to 100.

## Source

Sofi-Mahmudi A, Shamsoddin E, Khademioore S, Khazaei Y, Vahdati A,
Tovani-Palone MR (2025). Global, regional, and national survey on burden
and Quality of Care Index (QCI) of orofacial clefts: Global burden of
disease systematic analysis 1990-2019. PLOS ONE 20(1): e0317267.
[doi:10.1371/journal.pone.0317267](https://doi.org/10.1371/journal.pone.0317267)

## Details

The published analysis covers every country; the fifteen here were
chosen to span continents, to cover a wide range of scores and to
include several changes of rank over the period, which makes the set a
useful example for
[`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md).
Country names are shortened for plotting, and `iso` matches the codes
[`race_flags()`](https://choxos.github.io/ggextreme/reference/race_flags.md)
uses.

## Examples

``` r
head(clefts_qci)
#>   country iso        region year       qci
#> 1  Brazil  br Latin America 1990  0.000000
#> 2  Brazil  br Latin America 1991  2.922546
#> 3  Brazil  br Latin America 1992  6.361471
#> 4  Brazil  br Latin America 1993 12.062058
#> 5  Brazil  br Latin America 1994 19.362466
#> 6  Brazil  br Latin America 1995 29.275937
subset(clefts_qci, year == 2019)[order(-subset(clefts_qci, year == 2019)$qci), ]
#>           country iso        region year      qci
#> 150       Germany  de        Europe 2019 99.34695
#> 60          Chile  cl Latin America 2019 99.28937
#> 390   South Korea  kr          Asia 2019 98.92488
#> 450 United States  us North America 2019 98.50286
#> 120         Egypt  eg        Africa 2019 96.51796
#> 240          Iran  ir          Asia 2019 95.89016
#> 180         India  in          Asia 2019 92.31184
#> 420        Turkey  tr          Asia 2019 92.04578
#> 90          China  cn          Asia 2019 91.51940
#> 270         Kenya  ke        Africa 2019 89.84286
#> 300        Mexico  mx Latin America 2019 89.66448
#> 360  South Africa  za        Africa 2019 87.23564
#> 30         Brazil  br Latin America 2019 87.10227
#> 210     Indonesia  id          Asia 2019 85.21461
#> 330       Nigeria  ng        Africa 2019 82.95918
```
