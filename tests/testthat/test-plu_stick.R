options(plu.oxford_comma = FALSE)
ingredients <- c("sugar", "spice", "everything nice")

test_that("simple plu_stick works", {
  expect_equal(plu::stick(ingredients), "sugar, spice and everything nice")
})

test_that("plu_stick fn works", {
  expect_equal(
    plu::stick(ingredients, toupper), "SUGAR, SPICE and EVERYTHING NICE"
  )
  expect_equal(
    plu::stick(ingredients, sQuote, FALSE),
    "'sugar', 'spice' and 'everything nice'"
  )
})

test_that("plu_stick conj works", {
  expect_equal(
    plu::stick(ingredients, conj = " or "), "sugar, spice or everything nice"
  )
})

test_that("plu_stick sep works", {
  expect_equal(
    plu::stick(ingredients, sep = "/"), "sugar/spice and everything nice"
  )
})

test_that("plu_stick conj_all works", {
  expect_equal(
    plu::stick(ingredients, syndeton = "all"),
    "sugar and spice and everything nice"
  )
  expect_equal(
    plu::stick(ingredients, syndeton = "none"), "sugar, spice, everything nice"
  )
})

test_that("plu_stick oxford works", {
  expect_equal(
    plu::stick(ingredients, oxford = TRUE), "sugar, spice, and everything nice"
  )
  expect_equal(
    plu::stick(ingredients, oxford = FALSE), "sugar, spice and everything nice"
  )
})

test_that("options(plu.oxford_comma) works", {
  options(plu.oxford_comma = TRUE)
  expect_equal(plu::stick(ingredients), "sugar, spice, and everything nice")
  options(plu.oxford_comma = FALSE)
  expect_equal(plu::stick(ingredients), "sugar, spice and everything nice")
})

test_that("locale-based default oxford works", {
  options(plu.oxford_comma = NULL)
  with_mock(
    Sys.getlocale = function(...) "United States",
    expect_equal(plu::stick(ingredients), "sugar, spice, and everything nice")
  )
  with_mock(
    Sys.getlocale = function(...) "United Kingdom",
    expect_equal(plu::stick(ingredients), "sugar, spice and everything nice")
  )
})

test_that("early return", {
  expect_equal(plu_stick(character(0)), character(0))
})

test_that("errors", {
  expect_error(plu_stick("word", fn = "format"))
  expect_error(plu_stick("word", fn = this_is_not_a_real_function))
  expect_error(plu_stick("word", sep = numeric(1)))
  expect_error(plu_stick("word", sep = character(2)))
  expect_error(plu_stick("word", conj = numeric(1)))
  expect_error(plu_stick("word", conj = character(2)))
  expect_error(plu_stick("word", oxford = NA))
  expect_error(plu_stick("word", oxford = numeric(1)))
  expect_error(plu_stick("word", oxford = logical(2)))
})
