#' sidebar UI Function
#'
#' @title mod_sidebar_ui and mod_sidebar_server
#' @description A shiny Module containing sidebar content
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_sidebar
#'
#' @export
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' sidebar Server Functions
#'
#' @rdname mod_sidebar
#' @export
mod_sidebar_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
