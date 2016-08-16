
<!-- README.md is generated from README.Rmd. Please edit that file -->
dp
==

The goal of the `dp` package is to provide an extensible deparsing function for R. Like `base::deparse()` and others, the output is intended to produce results that are (almost) identical to the input when evaluated. Unlike the `base` equivalents, `dp` aims at making the output as human-friendly as possible.

The `dp()` function is rougly equivalent to `deparse()`, but with nicer defaults. The `dpc()` function returns an object which can be passed to `eval()` but also retains the desired formatting.

Examples
--------

``` r
dp(1)
#> [1] "1"
dpc(1L)
#> 1L
dpc(Sys.time())
#> as.POSIXct("2016-08-16 17:09:46 CEST")
dpc(print)
#> base::print

eval(dpc(print))
#> function (x, ...) 
#> UseMethod("print")
#> <bytecode: 0x392fed8>
#> <environment: namespace:base>
```

Compare this to the `dput()` output:

``` r
deparse(1)
#> [1] "1"
dput(1L)
#> 1L
dput(Sys.time())
#> structure(1471360187.05273, class = c("POSIXct", "POSIXt"))
dput(print)
#> function (x, ...) 
#> UseMethod("print")

eval(parse(text = deparse(print)))
#> function (x, ...) 
#> UseMethod("print")
```

Installation
------------

Install via

``` r
devtools::install_github("krlmlr/dp")
```
