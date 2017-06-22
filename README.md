
<!-- README.md is generated from README.Rmd. Please edit that file -->
deparse
==

The goal of the `deparse` package is to provide an extensible deparsing function for R. Like `base::deparse()` and others, the output is intended to produce results that are (almost) identical to the input when evaluated. Unlike the `base` equivalents, `deparse` aims at making the output as human-friendly as possible.

The `deparse()` function is rougly equivalent to `deparse()`, but with nicer defaults. The `deparsec()` function returns an object which can be passed to `eval()` but also retains the desired formatting---a better `dput()`.

Examples
--------

``` r
deparse(1)
#> [1] "1"
deparsec(1L)
#> 1L
deparsec(Sys.time())
#> as.POSIXct("2016-08-16 17:11:32 CEST")
deparsec(print)
#> base::print

eval(deparsec(print))
#> function (x, ...) 
#> UseMethod("print")
#> <bytecode: 0x27c1ed8>
#> <environment: namespace:base>
```

Compare this to the `dput()` output:

``` r
deparse(1)
#> [1] "1"
dput(1L)
#> 1L
dput(Sys.time())
#> structure(1471360292.17246, class = c("POSIXct", "POSIXt"))
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
devtools::install_github("krlmlr/deparse")
```
