test_that("simple plu_more()", {
  expect_equal(plu_more(letters), "a, b, c, d, e and 21 more characters")
})

test_that("max", {
  expect_equal(plu_more(letters, max = 2), "a, b and 24 more characters")
})

test_that("max", {
  expect_equal(
    plu_more(letters, max = 2, det = "other"), "a, b and 24 other characters"
  )
})

test_that("sep", {
  expect_equal(
    plu_more(letters, max = 2, sep = "; "), "a; b and 24 more characters"
  )
})

test_that("conj", {
  expect_equal(
    plu_more(letters, max = 2, conj = " or "), "a, b or 24 more characters"
  )
})

test_that("oxford", {
  expect_equal(
    plu_more(letters, max = 2, oxford = TRUE), "a, b, and 24 more characters"
  )
})

test_that("null max", {
  expect_equal(plu_more(letters, max = NULL),  plu_stick(letters))
  expect_equal(plu_more(letters, max = FALSE), plu_stick(letters))
  expect_equal(plu_more(letters, max = NA),    plu_stick(letters))
})

test_that("too high max", {
  expect_equal(plu_more(letters, max = 27),  plu_stick(letters))
  expect_equal(plu_more(letters, max = Inf), plu_stick(letters))
})

test_that("zero max", {
  expect_equal(plu_more(letters, max = 0),    "26 characters")
  expect_equal(plu_more(letters, max = 0.5),  "26 characters")
  expect_equal(plu_more(letters, max = -1),   "26 characters")
  expect_equal(plu_more(letters, max = -Inf), "26 characters")

  expect_equal(plu_more(letters, max = 0, type = FALSE), "26")
})

x <- letters[1:5]

test_that("simple type", {
  expect_equal(
    plu_more(x, max = 3, type = "letter"), "a, b, c and 2 more letters"
  )

  expect_equal(
    plu_more(x, max = 4, type = "letter"), "a, b, c, d and 1 more letter"
  )
})

test_that("automatic type", {
  expect_equal(plu_more(1:3, max = 2), "1, 2 and 1 more integer")
  expect_equal(plu_more(1:3, max = 1), "1 and 2 more integers")

  expect_equal(plu_more(as.numeric(1:3), max = 2), "1, 2 and 1 more numeric")
  expect_equal(plu_more(as.numeric(1:3), max = 1), "1 and 2 more numerics")

  expect_equal(
    plu_more(as.character(1:3), max = 2), "1, 2 and 1 more character"
  )
  expect_equal(
    plu_more(as.character(1:3), max = 1), "1 and 2 more characters"
  )

  expect_equal(plu_more(as.list(1:3), max = 2), "1, 2 and 1 more element")
  expect_equal(plu_more(as.list(1:3), max = 1), "1 and 2 more elements")
})

test_that("pluralization", {
  expect_equal(
    plu_more(x, max = 3, type = "fish"), "a, b, c and 2 more fish"
  )
  expect_equal(
    plu_more(x, max = 4, type = "fish"), "a, b, c, d and 1 more fish"
  )

  expect_equal(
    plu_more(x, max = 3, type = "tooth"), "a, b, c and 2 more teeth"
  )
  expect_equal(
    plu_more(x, max = 4, type = "tooth"), "a, b, c, d and 1 more tooth"
  )

  expect_equal(
    plu_more(x, max = 3, type = "cactus"), "a, b, c and 2 more cacti"
  )
  expect_equal(
    plu_more(x, max = 4, type = "cactus"), "a, b, c, d and 1 more cactus"
  )
})

test_that("cardinal", {
  expect_equal(
    plu_more(letters, fn = nombre::cardinal),
    "a, b, c, d, e and twenty-one more characters"
  )

  expect_equal(
    plu_more(letters, fn = nombre::cardinal, max_n = 100),
    "a, b, c, d, e and twenty-one more characters"
  )

  expect_equal(
    plu_more(letters, fn = nombre::cardinal, max_n = 10),
    "a, b, c, d, e and 21 more characters"
  )
})

test_that("errors", {
  expect_error(plu_more("word", max = c(1, 2)))
  expect_error(plu_more("word", max = "1"))

  expect_error(plu_more("word", type = c("type1", "type2")))

  expect_error(plu_more("word", fn = "format"))
  expect_error(plu_more("word", fn = this_is_not_a_real_function))
})
