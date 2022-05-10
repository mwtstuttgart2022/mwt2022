#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("MWT-App"),
      tabsetPanel(
        tabPanel(
          "Cantor-Funktion",
          mod_cantor_function_ui("cantor_function_1")
                 ),
        tabPanel(
          "Approximation durch Treppenfunktionen",
          mod_treppenfunktion_approx_ui("treppenfunktion_approx_1")
          )
        )
      )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mwt2022"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
