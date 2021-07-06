test_that("simple plu_more()", {
  expect_equal(
    plu_more(letters), c("a", "b", "c", "d", "e", "21 more characters")
  )
})

test_that("max", {
  expect_equal(plu_more(letters, max = 2),   c("a", "b", "24 more characters"))
  expect_equal(plu_more(letters, max = 2.5), c("a", "b", "24 more characters"))
})

test_that("det", {
  expect_equal(
    plu_more(letters, max = 2, det = "other"),
    c("a", "b", "24 other characters")
  )
})

test_that("null max", {
  expect_equal(plu_more(letters, max = NULL),  letters)
  expect_equal(plu_more(letters, max = FALSE), letters)
  expect_equal(plu_more(letters, max = NA),    letters)
})

test_that("too high max", {
  expect_equal(plu_more(letters, max = 27),  letters)
  expect_equal(plu_more(letters, max = Inf), letters)
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
    plu_more(x, max = 3, type = "letter"), c("a", "b", "c", "2 more letters")
  )

  expect_equal(
    plu_more(x, max = 4, type = "letter"),
    c("a", "b", "c", "d", "1 more letter")
  )
})

test_that("automatic type", {
  expect_equal(plu_more(1:3, max = 2), c("1", "2", "1 more integer"))
  expect_equal(plu_more(1:3, max = 1), c("1", "2 more integers"))

  expect_equal(
    plu_more(as.numeric(1:3), max = 2), c("1", "2", "1 more numeric")
  )
  expect_equal(plu_more(as.numeric(1:3), max = 1), c("1", "2 more numerics"))

  expect_equal(
    plu_more(as.character(1:3), max = 2), c("1", "2", "1 more character")
  )
  expect_equal(
    plu_more(as.character(1:3), max = 1), c("1", "2 more characters")
  )

  expect_equal(
    plu_more(as.list(1:3), max = 2), list(1, 2, "1 more integer")
  )
  expect_equal(plu_more(as.list(1:3), max = 1), list(1, "2 more integers"))

  expect_equal(
    plu_more(list("a", 2, TRUE), max = 2), list("a", 2, "1 more element")
  )
  expect_equal(
    plu_more(list("a", 2, TRUE), max = 1), list("a", "2 more elements")
  )
})

test_that("pluralization", {
  expect_equal(
    plu_more(x, max = 3, type = "fish"), c("a", "b", "c", "2 more fish")
  )
  expect_equal(
    plu_more(x, max = 4, type = "fish"), c("a", "b", "c", "d", "1 more fish")
  )

  expect_equal(
    plu_more(x, max = 3, type = "tooth"), c("a", "b", "c", "2 more teeth")
  )
  expect_equal(
    plu_more(x, max = 4, type = "tooth"), c("a", "b", "c", "d", "1 more tooth")
  )

  expect_equal(
    plu_more(x, max = 3, type = "cactus"), c("a", "b", "c", "2 more cacti")
  )
  expect_equal(
    plu_more(x, max = 4, type = "cactus"),
    c("a", "b", "c", "d", "1 more cactus")
  )
})

test_that("cardinal", {
  expect_equal(
    plu_more(letters, fn = nombre::cardinal),
    c("a", "b", "c", "d", "e", "twenty-one more characters")
  )

  expect_equal(
    plu_more(letters, fn = nombre::cardinal, max_n = 100),
    c("a", "b", "c", "d", "e", "twenty-one more characters")
  )

  expect_equal(
    plu_more(letters, fn = nombre::cardinal, max_n = 10),
    c("a", "b", "c", "d", "e", "21 more characters")
  )
})

test_that("errors", {
  expect_error(plu_more(letters, max = c(1, 2)))
  expect_error(plu_more(letters, max = "1"))

  expect_error(plu_more(letters, type = c("type1", "type2")))
  expect_error(plu_more(letters, type = 1))

  expect_error(plu_more(letters, fn = "this_is_not_a_real_function"))
  expect_error(plu_more(letters, fn = this_is_not_a_real_function))
  expect_error(plu_more(letters, fn = "plu::this_is_not_a_real_function"))
  expect_error(plu_more(letters, fn = plu::this_is_not_a_real_function))
})
