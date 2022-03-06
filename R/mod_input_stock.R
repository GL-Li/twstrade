#' input_stock UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_stock_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      align = "left",
      class = "buttons",
      radioButtons(ns("stock"), 
                   NULL, 
                   choices = c("VLO", "MRO", "BTU", "CNQ", "CENX", "AA", "GNK", 
                               "SBLK", "LVS", "WYNN", "SHAK", "CMG", "AEO", 
                               "LULU", "FOSL", "SIG", "CSIQ", "RUN", "AMD", 
                               "MU", "PLUG", "FCEL", "RIOT", "COIN", "SQ", 
                               "UPST", "FTCH", "OSTK", "DOCU", "ZM", "MRNA", 
                               "BNTX", "DKNG", "IONQ", "BYND", "TSLA", "LAC"),
                   selected = "VLO")
    )
  )
}

#' input_stock Server Functions
#'
#' @noRd 
mod_input_stock_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # print(input$stock)
    return(reactive({
      req(input$stock)
      print(input$stock)
      input$stock
    }))
  })
}

## To be copied in the UI
# mod_input_stock_ui("input_stock_1")

## To be copied in the server
# mod_input_stock_server("input_stock_1")
