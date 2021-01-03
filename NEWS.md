# plu (development version)

## New features
* `plu::more()` limits a vector to the first `n` elements with a message for remaining elements.
  * This can be useful for providing messages to users without an overwhelming amount of detail.
  * `plu::more(letters, max = 2)` yields `c("a", "b", "24 more characters")`.
  
* The convenience function `get_fun()` finds a function using a character string or unquoted function name, with or without colons.

## Patches
* `plu::stick()`'s `oxford` now defaults to `FALSE` rather than depending on the user's environment.
  
## Deprecations
* `plu::stick()`'s `syndeton` is now deprecated in favor of explicitly setting `sep` and `conj`.
* `plu::stick()`'s `fn` and `...` are now deprecated in favor of applying a function to `x` before passing it into `plu::stick()`.

## Miscellaneous
* Added a `pkgdown` site.
* Added an ORCID to `DESCRIPTION`.

# plu 0.1.1

* Allow braces to be used within words.
  * "cact{us|i}" -> "cactus", "cacti"
  * "antenna{|e}" -> "antenna", "antennae"
* Remove cross-reference to **glue** to avoid CRAN NOTE.
* Add AGID copyright information to package documentation (`?plu`).
* Minor speed improvements to regular pluralization rules.
* Bug fixes when calling `plu::ral()` from inside other functions.

# plu 0.1.0

* Initial release.
