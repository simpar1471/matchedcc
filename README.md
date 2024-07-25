
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matchedcc

<!-- badges: start -->

[![R-CMD-check](https://github.com/simpar1471/matchedcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simpar1471/matchedcc/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/matchedcc)](https://CRAN.R-project.org/package=matchedcc)
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

## Examples

The functions in matchedcc are easy to use. To demonstrate their use, we
will use the `mccxmpl` dataset, which is included in matchedcc. This
dataset has two columns - `cases` and `controls`. In this dataset, cases
had experienced a heart attack, and cases and controls were matched
accordingly. Each column has only `1` or `0` values, which describe
whether a case or control encountered our exposure - in this case,
drinking \>6 cups of coffee per day.

``` r
library(matchedcc)
head(matchedcc::mccxmpl)
#>   case control
#> 1    1       1
#> 2    1       0
#> 3    0       1
#> 4    0       0
#> 5    1       1
#> 6    1       1
```

### Vector input

The `mcc()` function will take vectors of `1`s and `0`s and use these to
run a matched case control analysis:

``` r
mcc(cases = matchedcc::mccxmpl$case, controls = matchedcc::mccxmpl$control)
#> $data
#>            Controls
#> Cases       Unexposed Exposed Total
#>   Unexposed         8       8    16
#>   Exposed           3       8    11
#>   Total            11      16    27
#> 
#> $mcnemar_chi2
#> 
#>  McNemar's Chi-squared test
#> 
#> data:  mcc_table
#> McNemar's chi-squared = 2.2727, df = 1, p-value = 0.1317
#> 
#> 
#> $mcnemar_exact_p
#> Exact McNemar significance probability 
#>                              0.2265625 
#> 
#> $proportions
#>   Proportion with factor
#>        Cases  Controls
#>    0.5925926 0.4074074
#> 
#> $statistics
#>             estimate [95% CI]
#> statistic     estimate       lower      upper
#>   difference 0.1851852 -0.08225420  0.4526246
#>   ratio      1.4545455  0.89110096  2.3742568
#>   rel. diff. 0.3125000 -0.02436881  0.6493688
#>   odds ratio 2.6666667  0.64003641 15.6064036
```

### 2x2 table input

The `mcc()` function can also accept a 2x2 table with matched
case-control data, provided it is in the following format:

| Cases     | Controls |           |
|-----------|----------|-----------|
|           | Exposed  | Unexposed |
| Exposed   | a        | b         |
| Unexposed | c        | d         |

``` r
mcc_table <- table(matchedcc::mccxmpl$control,
                   matchedcc::mccxmpl$case)
mcc(table = mcc_table)
#> $data
#>            Controls
#> Cases       Unexposed Exposed Total
#>   Unexposed         8       8    16
#>   Exposed           3       8    11
#>   Total            11      16    27
#> 
#> $mcnemar_chi2
#> 
#>  McNemar's Chi-squared test
#> 
#> data:  mcc_table
#> McNemar's chi-squared = 2.2727, df = 1, p-value = 0.1317
#> 
#> 
#> $mcnemar_exact_p
#> Exact McNemar significance probability 
#>                              0.2265625 
#> 
#> $proportions
#>   Proportion with factor
#>        Cases  Controls
#>    0.5925926 0.4074074
#> 
#> $statistics
#>             estimate [95% CI]
#> statistic     estimate       lower      upper
#>   difference 0.1851852 -0.08225420  0.4526246
#>   ratio      1.4545455  0.89110096  2.3742568
#>   rel. diff. 0.3125000 -0.02436881  0.6493688
#>   odds ratio 2.6666667  0.64003641 15.6064036
```

### ‘Immediate’ input

Last but not least, if you have cell counts from a 2x2 table, you can
provide individual cell counts to `mcci()`:

``` r
mcci(a = 8, b = 8, c = 3, d = 8)
#> $data
#>            Controls
#> Cases       Unexposed Exposed Total
#>   Unexposed         8       8    16
#>   Exposed           3       8    11
#>   Total            11      16    27
#> 
#> $mcnemar_chi2
#> 
#>  McNemar's Chi-squared test
#> 
#> data:  mcc_table
#> McNemar's chi-squared = 2.2727, df = 1, p-value = 0.1317
#> 
#> 
#> $mcnemar_exact_p
#> Exact McNemar significance probability 
#>                              0.2265625 
#> 
#> $proportions
#>   Proportion with factor
#>        Cases  Controls
#>    0.5925926 0.4074074
#> 
#> $statistics
#>             estimate [95% CI]
#> statistic     estimate       lower      upper
#>   difference 0.1851852 -0.08225420  0.4526246
#>   ratio      1.4545455  0.89110096  2.3742568
#>   rel. diff. 0.3125000 -0.02436881  0.6493688
#>   odds ratio 2.6666667  0.64003641 15.6064036
```

## Validation against Stata

The package is validated against Stata’s own outputs, using 1000
randomly generated `mcci` runs from Stata. The code to generate these
can be seen in `/tests/testdata/run-stata-mcc.R`.

## Citation

Parker S (2024). *matchedcc: Stata-like matched case-control analysis*.
<https://github.com/lshtm-gigs/gigs/>.
