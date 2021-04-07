ingredients <- c("sugar", "spice", "everything nice")

test_that("simple plu_stick works", {
  expect_equal(plu_stick(ingredients), "sugar, spice and everything nice")
})

test_that("plu_stick conj works", {
  expect_equal(
    plu_stick(ingredients, conj = " or "), "sugar, spice or everything nice"
  )
})

test_that("plu_stick sep works", {
  expect_equal(
    plu_stick(ingredients, sep = "/"), "sugar/spice and everything nice"
  )

  expect_equal(
    plu_stick(ingredients, sep = ""), "sugarspice and everything nice"
  )

  expect_equal(
    plu_stick(ingredients, sep = NULL), "sugarspice and everything nice"
  )
})

test_that("plu_stick oxford works", {
  expect_equal(
    plu_stick(ingredients, oxford = TRUE), "sugar, spice, and everything nice"
  )
  expect_equal(
    plu_stick(ingredients, oxford = FALSE), "sugar, spice and everything nice"
  )
})

test_that("options(plu.oxford_comma) works", {
  withr::local_options(plu.oxford_comma = TRUE)
  expect_equal(plu_stick(ingredients), "sugar, spice, and everything nice")

  withr::local_options(plu.oxford_comma = FALSE)
  expect_equal(plu_stick(ingredients), "sugar, spice and everything nice")
})

test_that("early return", {
  expect_equal(plu_stick(character(0)), character(0))
})

test_that("errors", {
  expect_error(plu_stick(letters, sep    = numeric(1)),   "`sep`")
  expect_error(plu_stick(letters, sep    = character(2)), "`sep`")
  expect_error(plu_stick(letters, conj   = numeric(1)),   "`conj`")
  expect_error(plu_stick(letters, conj   = character(2)), "`conj`")
  expect_error(plu_stick(letters, oxford = NA))
  expect_error(plu_stick(letters, oxford = numeric(1)))
  expect_error(plu_stick(letters, oxford = logical(2)))
  lifecycle::expect_defunct(plu_stick(ingredients, fn = toupper))
  lifecycle::expect_defunct(plu_stick(ingredients, syndeton = "all"))
  lifecycle::expect_defunct(plu_stick(ingredients, syndeton = "none"))
})
