#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_treppenfunktion_approx_server("treppenfunktion_approx_1")
  mod_cantor_function_server("cantor_function_1")
}
