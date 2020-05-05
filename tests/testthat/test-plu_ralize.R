test_that("simple pluralization", {
  expect_equal(plu::ralize("word"), "words")
})

test_that("character vector pluralization", {
  expect_equal(plu::ralize(c("word", "group")), c("words", "groups"))
})

irregs              <- c("formula", "person", "child")
irregs_none         <- c("formulas", "persons", "childs")
irregs_liberal      <- c("formulas", "persons", "children")
irregs_moderate     <- c("formulas", "people", "children")
irregs_conservative <- c("formulae", "people", "children")

test_that("default irregular pluralization", {
  options(plu.irregulars = NULL)
  expect_equal(plu::ralize(irregs), irregs_moderate)
})

test_that("capitalized irregular pluralization", {
  options(plu.irregulars = NULL)
  expect_equal(
    plu::ralize(c("Formula", "Person", "Child")),
    c("Formulas", "People", "Children")
  )
})

test_that("irregular pluralization options", {
  expect_equal(plu::ralize(irregs, "none"), irregs_none)
  expect_equal(plu::ralize(irregs, "liberal"), irregs_liberal)
  expect_equal(plu::ralize(irregs, "moderate"), irregs_moderate)
  expect_equal(plu::ralize(irregs, "conservative"), irregs_conservative)
})

test_that("options(plu.irregulars)", {
  options(plu.irregulars = "none")
  expect_equal(plu::ralize(irregs), irregs_none)
  options(plu.irregulars = "liberal")
  expect_equal(plu::ralize(irregs), irregs_liberal)
  options(plu.irregulars = "moderate")
  expect_equal(plu::ralize(irregs), irregs_moderate)
  options(plu.irregulars = "conservative")
  expect_equal(plu::ralize(irregs), irregs_conservative)
})

