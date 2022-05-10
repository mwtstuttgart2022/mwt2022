#' cantor_function UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cantor_function_ui <- function(id){
  ns <- NS(id)
  tagList(
    sliderInput(ns("n"), "n", min = 0, max = 10, value = 0),
    plotOutput(ns("plot"))
  )
}

#' cantor_function Server Functions
#'
#' @noRd
mod_cantor_function_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    cantor_function <- function(x, n) {
      if (n == 0) {
        return(x)
      }
      I = (( 0 <= x) & (x <= 1/3))
      J = ((1/3 < x) & (x < 2/3))
      K = ((2/3 <= x) & (x <= 1))
      a = 0.5 * cantor_function(3*x[I], n-1)
      b = rep(0.5, times=sum(J))
      c = 0.5 + 0.5*cantor_function(3*x[K] - 2, n-1)
      return(c(a,b,c))
    }

    n <- reactive(input$n)

    output$plot <- renderPlot({
      x = seq(from = 0, to = 1, by = .0001)
      y = cantor_function(x, n())
      plot(x, y, type = "l", ylab = substitute("f"[n()]*"(x)", list(n=n())))
    })



  })
}

## To be copied in the UI
#

## To be copied in the server
#
