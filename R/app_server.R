#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  stock <- mod_input_stock_server("input_stock_1")
  mod_plot_stock_server("plot_stock_1", stock)
}
