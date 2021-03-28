# plu (development version)

# plu 0.2.0

## New features
* `plu::more()` limits a vector to the first `n` elements with a message for remaining elements.
  * This can be useful for providing messages to users without an overwhelming amount of detail.
  * `plu::more(letters, max = 2)` yields `c("a", "b", "24 more characters")`.
  
* The convenience function `get_fun()` finds a function using a character string or unquoted function name, with or without colons.

* `plu::ral()` now ensures that the capitalization of a phrase stays the same after pluralizing.
  * `plu::ral("A sentence.")` now results in "Sentences." rather than "sentences."

* Convenience functions around capitalization have been added:
  * `is_capital()` tests if a character (or string of characters) is capital or lowercase.
  * `is_capitalized()` tests if the first alphabetic character in a string is capital.
  * `capitalize()` converts the first alphabetic character in a string to capital.
    * Unlike `tools::toTitleCase()` or `stringr::str_to_sentence()`, this does not change the capitalization of any subsequent characters.

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
