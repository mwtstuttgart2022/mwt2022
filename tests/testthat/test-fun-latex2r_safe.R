test_that("latex2r_safe works", {
  #existence

  testthat::expect_true(exists("latex2r_safe", mode = "function"))

  #output

  testthat::expect_null(latex2r_safe())
})
