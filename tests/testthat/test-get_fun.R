test_that("get_fun", {
  expect_equal(get_fun(plu_ral),    plu_ral)
  expect_equal(get_fun("plu_ral"),  plu_ral)
  expect_equal(get_fun(plu::ral),   plu::ral)
  expect_equal(get_fun("plu::ral"), plu::ral)
})

test_that("get_fun with NULL", {
  expect_equal(get_fun(NULL), identity)

  expect_equal(get_fun(NULL, plu_ral),    plu_ral)
  expect_equal(get_fun(NULL, "plu_ral"),  plu_ral)
  expect_equal(get_fun(NULL, plu::ral),   plu::ral)
  expect_equal(get_fun(NULL, "plu::ral"), plu::ral)
})

test_that("errors", {
  expect_error(get_fun(NULL, NULL))
  expect_error(get_fun(NULL, FALSE))

  expect_error(get_fun(plu_not_a_real_function))
  expect_error(get_fun("plu_not_a_real_function"))
  expect_error(get_fun(plu::not_a_real_function))
  expect_error(get_fun("plu::not_a_real_function"))

  expect_error(get_fun(NULL, plu_not_a_real_function))
  expect_error(get_fun(NULL, "plu_not_a_real_function"))
  expect_error(get_fun(NULL, plu::not_a_real_function))
  expect_error(get_fun(NULL, "plu::not_a_real_function"))
})
