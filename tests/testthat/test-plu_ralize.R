test_that("simple pluralization", {
  expect_equal(plu_ralize("word"), "words")
})

test_that("character vector pluralization", {
  expect_equal(plu_ralize(c("word", "group")), c("words", "groups"))

  expect_equal(
    plu_ralize(c("word", "worry", "fox", "soliloquy", "batch", "TTY", "day")),
    c("words", "worries", "foxes", "soliloquies", "batches", "TTYs", "days")
  )
})

irregs              <- c("formula", "person", "child")
irregs_none         <- c("formulas", "persons", "childs")
irregs_liberal      <- c("formulas", "persons", "children")
irregs_moderate     <- c("formulas", "people", "children")
irregs_conservative <- c("formulae", "people", "children")

test_that("default irregular pluralization", {
  withr::local_options(plu.irregulars = NULL)
  expect_equal(plu_ralize(irregs), irregs_moderate)
})

test_that("capitalized irregular pluralization", {
  withr::local_options(plu.irregulars = NULL)

  expect_equal(
    plu_ralize(c("Formula", "Person", "Child")),
    c("Formulas", "People", "Children")
  )

  expect_equal(
    plu_ralize(c("FORMULA", "PERSON", "CHILD")),
    c("FORMULAs", "PERSONs", "CHILDs")
  )
})

test_that("irregular pluralization options", {
  expect_equal(plu_ralize(irregs, "none"), irregs_none)
  expect_equal(plu_ralize(irregs, "liberal"), irregs_liberal)
  expect_equal(plu_ralize(irregs, "moderate"), irregs_moderate)
  expect_equal(plu_ralize(irregs, "conservative"), irregs_conservative)
})

test_that("options(plu.irregulars)", {
  withr::local_options(plu.irregulars = "none")
  expect_equal(plu_ralize(irregs), irregs_none)
  expect_equal(plu_ralize(irregs, "liberal"), irregs_liberal)

  withr::local_options(plu.irregulars = "liberal")
  expect_equal(plu_ralize(irregs), irregs_liberal)
  expect_equal(plu_ralize(irregs, "moderate"), irregs_moderate)

  withr::local_options(plu.irregulars = "moderate")
  expect_equal(plu_ralize(irregs), irregs_moderate)
  expect_equal(plu_ralize(irregs, "conservative"), irregs_conservative)

  withr::local_options(plu.irregulars = "conservative")
  expect_equal(plu_ralize(irregs), irregs_conservative)
  expect_equal(plu_ralize(irregs, "none"), irregs_none)
})

test_that("early return", {
  expect_equal(plu_ralize(character(0)), character(0))
})

test_that("errors", {
  expect_error(plu_ralize(integer(1)))
})
