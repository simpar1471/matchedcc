
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matchedcc

<!-- badges: start -->

[![R-CMD-check](https://github.com/simpar1471/matchedcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simpar1471/matchedcc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The aim of matchedcc is to provide epidemiologists using R with
Stata-like analysis of matched case-control data. This package has two
functions, `mcc()` and `mcci()`, which are direct analogues of Stata’s
own `mcc` and `mcci` commands.

## Installation

You can install the development version of matchedcc from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("simpar1471/matchedcc")
```

## Example

``` r
library(matchedcc)
matchedcc::mccxmpl
#>    case control
#> 1     1       1
#> 2     1       0
#> 3     0       1
#> 4     0       0
#> 5     1       1
#> 6     1       1
#> 7     1       1
#> 8     1       1
#> 9     1       1
#> 10    1       1
#> 11    1       1
#> 12    1       0
#> 13    1       0
#> 14    1       0
#> 15    1       0
#> 16    1       0
#> 17    1       0
#> 18    1       0
#> 19    0       1
#> 20    0       1
#> 21    0       0
#> 22    0       0
#> 23    0       0
#> 24    0       0
#> 25    0       0
#> 26    0       0
#> 27    0       0
```

## Citation

Parker S (2024). *matchedcc: Stata-like matched case-control analysis*.
<https://github.com/lshtm-gigs/gigs/>.
