one      <- integer(1)
two      <- integer(2)
fifty    <- integer(50)
thousand <- integer(1000)

options(plu.max_english = NULL)
options(plu.big.mark = NULL)
options(plu.irregulars = NULL)

test_that("pl", {
expect_equal(plu::ral("word", pl = FALSE), "word")
expect_equal(plu::ral("word", pl = TRUE), "words")
})

test_that("n", {
  expect_equal(plu::ral("word", n = 1), "word")
  expect_equal(plu::ral("word", n = -1), "word")
  expect_equal(plu::ral("word", n = 2), "words")
  expect_equal(plu::ral("word", n = 0), "words")
  expect_equal(plu::ral("word", n = 0.5), "words")
})

test_that("vector", {
  expect_equal(plu::ral("word", one), "word")
  expect_equal(plu::ral("word", two), "words")
})

test_that("invariant", {
  expect_equal(plu::ral("{invariant} variant", one), "invariant variant")
  expect_equal(plu::ral("{invariant} variant", two), "invariant variants")
})

test_that("irregulars", {
  expect_equal(plu::ral("formula", two), "formulas")
  expect_equal(plu::ral("formula", two, irregulars = "conservative"), "formulae")
  expect_equal(plu::ral("person", two), "people")
  expect_equal(plu::ral("person", two, irregulars = "liberal"), "persons")
})

test_that("pipe works", {
  expect_equal(plu::ral("{single|plural} number", one), "single number")
  expect_equal(plu::ral("{single|plural} number", two), "plural numbers")
  expect_equal(plu::ral("{single|dual|plural} number", one), "single number")
  expect_equal(plu::ral("{single|dual|plural} number", two), "dual numbers")
  expect_equal(plu::ral("{single|dual|plural} number", fifty), "plural numbers")
})

test_that("number", {
  expect_equal(plu::ral("[1] number", one), "1 number")
  expect_equal(plu::ral("[1] number", fifty), "50 numbers")
  expect_equal(plu::ral("[one] number", one), "one number")
  expect_equal(plu::ral("[one] number", fifty), "fifty numbers")
  expect_equal(plu::ral("[n] number", one), "one number")
  expect_equal(plu::ral("[n] number", fifty), "50 numbers")
})

test_that("max_english", {
  expect_equal(plu::ral("[n] number", fifty, max_english = 100), "fifty numbers")
  options(plu.max_english = 100)
  expect_equal(plu::ral("[n] number", fifty), "fifty numbers")
})

test_that("big.mark", {
  expect_equal(plu::ral("[n] number", thousand), "1,000 numbers")
  expect_equal(plu::ral("[n] number", thousand, big.mark = " "), "1 000 numbers")
  options(plu.big.mark = " ")
  expect_equal(plu::ral("[n] number", thousand), "1 000 numbers")
})
