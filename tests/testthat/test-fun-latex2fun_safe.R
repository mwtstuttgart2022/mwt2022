#' @import greekLetters

test_that("latex2fun_safe works", {
  #existence

  testthat::expect_true(exists("latex2fun_safe", mode = "function"))

  #catch error

  ##empty input

  testthat::expect_error(latex2fun_safe())

  ##contains assignment

  testthat::expect_error(latex2fun_safe("x = 7"))

  ##contains illegal variables

  ###create illegal latex expressions

  library(greekLetters)
  test_string_vec <-
    c(
      letters[!letters %in% c("x", "e")],
      names(greek) %>% .[.!= "pi"] %>% paste0("\\", .)
    ) %>%
    sapply(paste0, "\\cdot x") %>%
    unname()

  ###initialize output

  actual_out_vec <- c()

  for (latex_string in test_string_vec) {


    actual_out <- tryCatch(latex2fun_safe(latex_string), error = function(cond) "err")

    if (actual_out %>% is.function()) {
      actual_out_vec <-
        c(
          actual_out_vec,
          TRUE
        )
    }
      else
        actual_out_vec <-
          c(
            actual_out_vec,
            FALSE
          )

    }


  testthat::expect_equal(actual_out_vec, rep(FALSE, length(test_string_vec)))



  #output

  actual_function <- latex2fun_safe("e^{x\\cdot\\pi}")

  expected_function <- function(x) {
    exp(x * pi)
  }

  testthat::expect_equal(
    sapply(1:10, expected_function),
    sapply(1:10, expected_function)
    )







})
