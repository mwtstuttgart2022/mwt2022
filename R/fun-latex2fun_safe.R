#' @title latex2fun_safe
#'
#' @param code : character string in Latex language
#'
#' @return Latex expression translated into R code
#' @include fun-latex2r_safe.R
#' @import dplyr
latex2fun_safe = function (latex_string)
{
  fun_body = latex2r_safe(latex_string)
  variables <- all.vars(
    as.formula(paste("~", fun_body))
  )
  if (grepl("=", fun_body)) {
    stop("Expression contains assignment.")
  }
  if (variables[!variables %in% c("x", "pi")] %>% length() != 0) {
    stop("Expression contains other variable than x.")
  }
  pryr::make_function(alist(x = ), parse(text = fun_body)[[1]])
}
