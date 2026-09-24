# Quality of Care Index for orofacial clefts in every country, 1990 to 2019

Yearly Quality of Care Index (QCI) scores for orofacial clefts in 195
countries and territories, the full country panel of the analysis that
[clefts_qci](https://choxos.github.io/ggextreme/reference/clefts_qci.md)
samples. The QCI is a composite of four secondary indices derived from
Global Burden of Disease estimates and summarized by principal component
analysis, rescaled to run from 0 to 100, where higher is better care.
World regions, WHO regions, World Bank income groups and SDI groups in
the published panel are left out.

## Usage

``` r
clefts_qci_world
```

## Format

A data frame with 5,850 rows and 4 columns:

- country:

  Country or territory, as named by the Global Burden of Disease study.

- iso3:

  ISO 3166-1 alpha-3 code.

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

Countries keep the names the Global Burden of Disease study gives them,
such as "Iran (Islamic Republic of)", and carry the ISO 3166-1 alpha-3
code of the map
[`ggchoropleth()`](https://choxos.github.io/ggextreme/reference/ggchoropleth.md)
draws, so either column can name the region.

## Examples

``` r
head(clefts_qci_world)
#>       country iso3 year      qci
#> 1 Afghanistan  AFG 1990 58.81456
#> 2 Afghanistan  AFG 1991 60.07209
#> 3 Afghanistan  AFG 1992 60.60599
#> 4 Afghanistan  AFG 1993 60.81299
#> 5 Afghanistan  AFG 1994 60.86453
#> 6 Afghanistan  AFG 1995 60.90540
ggchoropleth(clefts_qci_world, iso3, year, values = c(QCI = "qci"))
```
