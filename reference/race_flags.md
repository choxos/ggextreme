# Bundled circular country flags

Returns file paths to circular flag images, ready to hand to the
`images` argument of
[`ggrace()`](https://choxos.github.io/ggextreme/reference/ggrace.md).
The package ships one flag per ISO 3166-1 country, plus Kurdistan,
mostly drawn from the same artwork
[`gt::fmt_flag()`](https://gt.rstudio.com/reference/fmt_flag.html) uses.
See `inst/extdata/flags/SOURCE.txt` for the provenance.

## Usage

``` r
race_flags(country)
```

## Arguments

- country:

  Country names or ISO codes, in any mix.

## Value

A character vector of file paths, named by the input.

## Details

Lookup tries, in order: the two letter code, the three letter code, the
full country name, and finally a unique partial match on the name. Names
follow the World Bank style used by 'gt', so a few common spellings do
not match: pass the code for those, and use
[`race_flag_codes()`](https://choxos.github.io/ggextreme/reference/race_flag_codes.md)
to find it.

## Examples

``` r
race_flags(c("br", "Iran", "Kenya"))
#>                                                               br 
#> "/home/runner/work/_temp/Library/ggextreme/extdata/flags/br.svg" 
#>                                                             Iran 
#> "/home/runner/work/_temp/Library/ggextreme/extdata/flags/ir.svg" 
#>                                                            Kenya 
#> "/home/runner/work/_temp/Library/ggextreme/extdata/flags/ke.svg" 

# "Turkey" and "South Korea" are spelled differently in the table, so pass
# their codes instead.
race_flags(c("tr", "kr"))
#>                                                               tr 
#> "/home/runner/work/_temp/Library/ggextreme/extdata/flags/tr.svg" 
#>                                                               kr 
#> "/home/runner/work/_temp/Library/ggextreme/extdata/flags/kr.svg" 
```
