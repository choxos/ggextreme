# Countries the bundled flags cover

Countries the bundled flags cover

## Usage

``` r
race_flag_codes()
```

## Value

A data frame with the two letter code, the three letter code and the
country name of every bundled flag.

## Examples

``` r
head(race_flag_codes())
#>   code code3              country
#> 1   ad   and              Andorra
#> 2   ae   are United Arab Emirates
#> 3   af   afg          Afghanistan
#> 4   ag   atg  Antigua and Barbuda
#> 5   ai   aia             Anguilla
#> 6   al   alb              Albania
subset(race_flag_codes(), grepl("Korea", country))
#>     code code3                   country
#> 117   kp   prk Korea, Dem. People's Rep.
#> 118   kr   kor               Korea, Rep.
```
