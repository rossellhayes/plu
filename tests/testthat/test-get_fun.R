test_that("get_fun", {
  expect_equal(get_fun(plu_ral),    plu_ral)
  expect_equal(get_fun("plu_ral"),  plu_ral)
  expect_equal(get_fun(plu::ral),   plu::ral)
  expect_equal(get_fun("plu::ral"), plu::ral)
})

test_that("get_fun with anonymous function", {
  expect_equal(get_fun(function(x) {x + 1}), function(x) {x + 1})
})

test_that("get_fun with NULL", {
  expect_equal(get_fun(NULL), identity)

  expect_equal(get_fun(NULL, plu_ral),    plu_ral)
  expect_equal(get_fun(NULL, "plu_ral"),  plu_ral)
  expect_equal(get_fun(NULL, plu::ral),   plu::ral)
  expect_equal(get_fun(NULL, "plu::ral"), plu::ral)
})

test_that("errors", {
  expect_error(get_fun(NULL, FALSE))

  expect_error(get_fun(plu_not_real_fun),    "plu_not_real_fun")
  expect_error(get_fun("plu_not_real_fun"),  "plu_not_real_fun")
  expect_error(get_fun(plu::not_real_fun),   "plu::not_real_fun")
  expect_error(get_fun("plu::not_real_fun"), "plu::not_real_fun")

  expect_error(get_fun(NULL, plu_not_real_fun),    "plu_not_real_fun")
  expect_error(get_fun(NULL, "plu_not_real_fun"),  "plu_not_real_fun")
  expect_error(get_fun(NULL, plu::not_real_fun),   "plu::not_real_fun")
  expect_error(get_fun(NULL, "plu::not_real_fun"), "plu::not_real_fun")
})
