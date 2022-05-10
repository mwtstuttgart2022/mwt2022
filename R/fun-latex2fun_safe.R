#' @title latex2fun_safe
#' @description Works similar to latex2r::latex2fun with additional input constraints
#' @param latex_string : character string in Latex language
#' @return fun_out : function object built from Latex string input
#' @include fun-latex2r_safe.R
#' @import dplyr
#' @export
latex2fun_safe = function (latex_string) {

  #translate latex string

  fun_body = latex2r_safe(latex_string)

  #error catching

  ##contains assignment

  stopifnot("Expression contains assignment." = !grepl("=", fun_body))

  ##contains illegal variables

  ###extract variables from function body

  variables <- paste("~", fun_body) %>% stats::as.formula() %>% all.vars()

  stopifnot(
    "Expression contains other variable than x." =
      variables[!variables %in% c("x", "pi")] %>% length() == 0
    )

  #write new function

  fun_out <- pryr::make_function(alist(x = ), parse(text = fun_body)[[1]])

  return(fun_out)
}
