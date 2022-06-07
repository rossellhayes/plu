# plu 0.2.2

* Modest speed improvements to `plu_ralize()`.
  * This propagates to `plu_ral()` and `plu_more()`, which use `plu_ralize()` internally.
* Deprecated `plu_stick()` in favor of `knitr::combine_words()` or `glue::glue_collapse()`.

# plu 0.2.1

## New features
* `plu_ral()` now supports arbitrary delimteters for special sequences.
  * `plu_ral("attorney [general]", open = "[", close = "]")` produces "attorneys general".
* `plu_ral()` now supports {braced|pipe} sequences of indefinite length.
  * `plu_ral("{one|two|three|more}", n = 3)` produces "three".
  * `plu_ral("{one|two|three|more}", n = 50)` produces "more".
  * The delimters of these sequences can be changed with `open` and `close`.

## Patches
* Modest speed improvements to `plu_ralize()`.
  * This propagates to `plu_ral()` and `plu_more()`, which use `plu_ralize()` internally.
* `plu_ral()` now correctly handles {braced} strings that contain sentence breaks.
  * Previously, sentence breaks would break the {braced} string and apply normal pluralization to its contents.
* `plu_ral()` now correctly handles {braced|pipe} sequences when `pl` is set to `FALSE`.
  * `plu_ral("{singular|plural}", pl = FALSE)` now returns `"singular"` instead of incorrectly returning `"plural"`.
* `plu_ralize()` now correctly handles ALL-CAPS words where their lowercase equivalent has an irregular plural.
  * `plu_ralize("CHILD")` now returns `"CHILDs"` instead of incorrectly returning `"Children"`.
* `is_capital(strict = TRUE)` now correctly handles non-character inputs.
  * `is_capital(1, strict = TRUE)` no returns `FALSE` instead of incorrectly returning `NA`.
  
## Miscellaneous
* Error messages now use `crayon` if it is installed.
* Deprecated arguments to `plu::stick()` now produce errors.
* Removed dependencies on `rlang` and `stringi`.
* Gained dependency on `backports`.

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
