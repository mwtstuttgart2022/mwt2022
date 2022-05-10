#' treppenfunktion_approx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinymath
mod_treppenfunktion_approx_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinymath::mathInput(ns("math"), "Equation"),
    actionButton(ns("go"), "Go!"),
    h4("Raw text"),
    verbatimTextOutput(ns("text_raw"), placeholder = TRUE),
    h4("Translation to R code"),
    verbatimTextOutput(ns("text_r"), placeholder = TRUE),
    plotOutput(ns("plot"))
  )
}

#' treppenfunktion_approx Server Functions
#'
#' @noRd
#' @include fun-latex2r_safe.R
#' @include fun-latex2fun_safe.R
mod_treppenfunktion_approx_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    math = eventReactive(input$go, input$math)
    output$text_raw = renderText(math())
    output$text_r = renderText(latex2r_safe(math()))

    output$plot <- renderPlot({
      req(!is.null(math()))
      x = seq(-2 * pi, 2 * pi, length.out = 500)
      f = latex2fun_safe(math())
      f_x = sapply(x, f)
      plot(x, f_x, type = "l")
    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
