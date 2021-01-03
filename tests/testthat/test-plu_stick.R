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
  expect_error(plu_stick(letters, sep    = numeric(1)),   "sep")
  expect_error(plu_stick(letters, sep    = character(2)), "sep")
  expect_error(plu_stick(letters, conj   = numeric(1)),   "conj")
  expect_error(plu_stick(letters, conj   = character(2)), "conj")
  expect_error(plu_stick(letters, oxford = NA))
  expect_error(plu_stick(letters, oxford = numeric(1)))
  expect_error(plu_stick(letters, oxford = logical(2)))
})

test_that("plu_stick fn works", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    plu_stick(ingredients, fn = toupper), "SUGAR, SPICE and EVERYTHING NICE"
  )
  expect_equal(
    plu_stick(ingredients, fn = "toupper"), "SUGAR, SPICE and EVERYTHING NICE"
  )

  lifecycle::expect_deprecated(plu_stick(ingredients, fn = toupper))

  expect_error(plu_stick(letters, fn = this_is_not_a_real_function))
})

test_that("plu_stick syndeton works", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    plu_stick(ingredients, syndeton = "all"),
    "sugar and spice and everything nice"
  )
  expect_equal(
    plu_stick(ingredients, syndeton = "none"), "sugar, spice, everything nice"
  )

  lifecycle::expect_deprecated(plu_stick(ingredients, syndeton = "all"))
  lifecycle::expect_deprecated(plu_stick(ingredients, syndeton = "none"))

  expect_error(plu_stick(ingredients, syndeton = "error"))
})
