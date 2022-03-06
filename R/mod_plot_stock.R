#' plot_stock UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_stock_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}
    
#' plot_stock Server Functions
#'
#' @noRd 
mod_plot_stock_server <- function(id, stock, source = "yahoo"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      # stock() is reactive term specified here.
      plot_momentum(stock(), source)
    })
 
  })
}
    
## To be copied in the UI
# mod_plot_stock_ui("plot_stock_1")
    
## To be copied in the server
# mod_plot_stock_server("plot_stock_1")
