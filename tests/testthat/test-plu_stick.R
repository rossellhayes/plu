test_that("plu_stick defunct", {
  lifecycle::expect_defunct(plu_stick(c("sugar", "spice", "everything nice")))
})
