
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plu <img src="man/figures/logo.png?raw=TRUE" align="right" height="138" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/plu?color=brightgreen)](https://cran.r-project.org/package=plu)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/badge/license-MIT-blueviolet.svg)](https://opensource.org/licenses/MIT)
[![R build
status](https://github.com/rossellhayes/plu/workflows/R-CMD-check/badge.svg)](https://github.com/rossellhayes/plu/actions)
[![](https://codecov.io/gh/rossellhayes/plu/branch/master/graph/badge.svg)](https://codecov.io/gh/rossellhayes/plu)
[![Dependencies](https://tinyverse.netlify.com/badge/plu)](https://cran.r-project.org/package=plu)
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

``` r
formulas1 <- c(x %in% 1:3 ~ "low", x %in% 4:6 ~ "medium", "x %in% 7:9")
formulas2 <- c(x %in% 1:3 ~ "low", x %in% 4:6 ~ "medium", "x %in% 7:9", "high")
problems1 <- Filter(function(x) !rlang::is_formula(x), formulas1)
problems2 <- Filter(function(x) !rlang::is_formula(x), formulas2)

paste(
  "All arguments must be formulas.",
  plu::ral("Argument", problems1), 
  plu::stick(problems1),
  plu::ral("isn't a formula.", problems1)
)
#> [1] "All arguments must be formulas. Argument x %in% 7:9 isn't a formula."

paste(
  "All arguments must be formulas.",
  plu::ral("Argument", problems2), 
  plu::stick(problems2, usethis::ui_code),
  plu::ral("isn't a formula.", problems2)
)
#> [1] "All arguments must be formulas. Arguments `x %in% 7:9` and `high` aren't formulas."
```

## Credits

Hex sticker font is
[Bodoni\*](https://github.com/indestructible-type/Bodoni) by
[indestructible type\*](https://indestructibletype.com/Home.html).

Image adapted from icon made by [Freepik](https://freepik.com) from
[flaticon.com](https://www.flaticon.com/free-icon/umbrella_2357382).

-----

Please note that the **plu** project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
