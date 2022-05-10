#' @title latex2r_safe
#'
#' @param code : character string in Latex language
#'
#' @return Latex expression translated into R code
#' @import latex2r
#' @export
latex2r_safe = function(code = NULL) {
  if (is.null(code)) {
    return(NULL)
  }
  tryCatch({
    latex2r(code)
  },
  latex2r.error = function(cnd) {
    showNotification(
      paste("Error when translating to R code -", cnd$message),
      type = "error"
    )
  },
  error = function(cnd) {
    showNotification("Unexpected error", type = "error")
  })
}
