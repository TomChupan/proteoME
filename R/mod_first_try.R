#' first_try UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_first_try_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' first_try Server Functions
#'
#' @noRd 
mod_first_try_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_first_try_ui("first_try_1")
    
## To be copied in the server
# mod_first_try_server("first_try_1")
