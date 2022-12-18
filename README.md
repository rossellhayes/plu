
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plu <img src="man/figures/logo.png?raw=TRUE" align="right" height="138" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/plu?color=brightgreen)](https://cran.r-project.org/package=plu)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blueviolet.svg)](https://cran.r-project.org/web/licenses/MIT)
[![R build
status](https://github.com/rossellhayes/plu/workflows/R-CMD-check/badge.svg)](https://github.com/rossellhayes/plu/actions)
[![](https://codecov.io/gh/rossellhayes/plu/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rossellhayes/plu)
[![Dependencies](https://tinyverse.netlify.com/badge/plu)](https://cran.r-project.org/package=plu)
[![CodeFactor](https://www.codefactor.io/repository/github/rossellhayes/plu/badge)](https://www.codefactor.io/repository/github/rossellhayes/plu)
<!-- badges: end -->

Pluralize phrases in R

## Overview

**plu** provides a simplified way to dynamically generate plain-language
messages in R when we can’t know beforehand whether a message will be
singular or plural.

Pluralizes English phrases based on the length of an associated vector.
Contains helper functions to create natural language lists from vectors
and to include the length of a vector in natural language.

## Installation

You can install the stable release of **plu** from
[CRAN](https://cran.r-project.org/package=plu) with:

``` r
install.packages("plu")
```

You can install the development version of **plu** from
[GitHub](https://github.com/rossellhayes/plu) with:

``` r
# install.packages("remotes")
remotes::install_github("rossellhayes/plu")
```

## Usage

**plu** can be particularly useful when constructing error messages. For
example, you may want to create a message that is gramatically correct
regardless of whether the user’s code had one problem or multiple
problems.

With one problem, **plu** constructs a message in the singular:

``` r
arguments <- c(1, 2, 3, 3.5)

paste(
  "All arguments must be integers.",
  plu::ral("Argument", arguments[arguments %% 1 != 0]), 
  and::and(encodeString(arguments[arguments %% 1 != 0], quote = "`")),
  plu::ral("isn't an integer.", arguments[arguments %% 1 != 0])
)
#> [1] "All arguments must be integers. Argument `3.5` isn't an integer."
```

But with two problems, the same code will construct a message in the
plural:

``` r
arguments <- c(1, 2, 3, 3.5, 3.75)

paste(
  "All arguments must be integers.",
  plu::ral("Argument", arguments[arguments %% 1 != 0]), 
  and::and(encodeString(arguments[arguments %% 1 != 0], quote = "`")),
  plu::ral("isn't an integer.", arguments[arguments %% 1 != 0])
)
#> [1] "All arguments must be integers. Arguments `3.5` and `3.75` aren't integers."
```

If you expect a lot of problems, you can use `plu::more()` to limit the
number of displayed issues:

``` r
ints <- as.integer(runif(20, -10, 10))
paste(
  "All inputs must be non-negative.",
  and::and(plu::more(encodeString(ints[ints < 0], quote = "`"), type = "integer")),
  plu::ral("is", ints[ints < 0]), "negative."
)
#> [1] "All inputs must be non-negative. `-3`, `-2`, `-3`, `-5`, `-7`, and 6 more integers are negative."
```

## Credits

Hex sticker font is
[Bodoni\*](https://github.com/indestructible-type/Bodoni) by
[indestructible type\*](https://indestructibletype.com/Home.html).

Image adapted from icon made by [Freepik](https://www.freepik.com) from
[flaticon.com](https://www.flaticon.com/free-icon/umbrella_2357382).

------------------------------------------------------------------------

Please note that the **plu** project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
