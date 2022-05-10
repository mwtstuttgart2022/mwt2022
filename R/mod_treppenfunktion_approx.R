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
    wellPanel(
      sliderInput(ns("n"), "n", min = 0, max = 10, value = 3),
      plotOutput(ns("plot"))
    ),
    inputPanel(
      sliderInput(ns("xmin"), "xmin", min = -5, max = 0, value = -3),
      sliderInput(ns("xmax"), "xmax", min = 0, max = 5, value = 3)
    )
  )
}

#' treppenfunktion_approx Server Functions
#'
#' @noRd
#' @include fun-latex2r_safe.R
#' @include fun-latex2fun_safe.R
#' @import ggplot2
#' @import magrittr
mod_treppenfunktion_approx_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    math = eventReactive(input$go, input$math)
    output$text_raw = renderText(math())
    output$text_r = renderText(latex2r_safe(math()))


    n <- reactive(input$n)
    x_min <- reactive(input$xmin)
    x_max <- reactive(input$xmax)


    output$plot <- renderPlot({

      req(!is.null(math()))

      input_fun = function (x) max(latex2fun_safe(math())(x), 0)


      approx_fun <- function(x, n) {
        min(2^(-n) * floor(2^n * input_fun(x)), n)
      }

      x <- seq(from = x_min(), to = x_max(), by = 0.01)

      function_data <- data.frame(
        x = x,
        f_x = sapply(x, input_fun)
      )

      approx_data <- data.frame(
        x = x,
        f_x = sapply(x, approx_fun, n())
      )

      base <- ggplot() + xlim(x_min(), x_max())


      base +
        geom_area(data = function_data, aes(x = x, y = f_x), color = "red", fill = "blue", alpha = 0.2) +
        geom_area(data = approx_data, aes(x = x, y = f_x), color = "grey60", fill = "cornsilk", alpha = 1)




    })

  })
}

## To be copied in the UI
#

## To be copied in the server
#
