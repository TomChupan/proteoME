#' body_tab1 UI Function
#'
#' @title mod_body_tab1_ui and mod_body_tab1_server
#' @description A shiny Module with the main body content connected with data import.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title (there is one of the 3
#' possible tables in that box).
#' @param data_type Number from 1 to 3 defining the type of the data
#' as following:
#' \itemize{
#'  \item 1: "Abundances"
#'  \item 2: "Annotations (run)"
#'  \item 3: "Annotations (sample)"
#' }
#'
#'
#' @rdname mod_body_tab1
#'
#' @export
#'
#' @importFrom shiny NS tagList fluidRow tableOutput validate need
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput datatable renderDT
mod_body_tab1_ui <- function(id,box_title="Your title.",data_type=c(1,2,3)){
  ns <- NS(id)
  tagList(

        box(
          width=switch(data_type,12,4,8),height = '30%',title = box_title,status="primary", #just for CSS
          DTOutput(ns("tab"))
        ) #box close


  ) #tagList close
} #mod_body_tab1_ui close

#' body_tab1 Server Functions
#'
#' @param validate_message Message to be displayed when there are no data to build a table.
#' @param r A "storage" for the variables used throughout the app
#' @param data_type Number from 1 to 3 defining the type of the data
#' as following:
#' \itemize{
#'  \item 1: "Abundances"
#'  \item 2: "Annotations (run)"
#'  \item 3: "Annotations (sample)"
#' }
#'
#' @rdname mod_body_tab1
#' @export
mod_body_tab1_server <- function(id,
                                 validate_message="Please upload missing file!",
                                 data_type=c(1,2,3),
                                 r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dTOtab=reactive({
      switch(data_type,
             r$d1,
             r$d2,
             r$d3)
    })

    output$tab=renderDT({
      validate(need(!is.null(dTOtab()), validate_message))
      datatable(dTOtab(), rownames=F,selection="none",
                options = list(searching = F,
                               pageLength = 6,
                               lengthChange=F,
                               scrollX = T,
                               autoWidth = F
                              )
                ) #datatable close
    }) #renderDataTable close

  }) #moduleServer close
}

## To be copied in the UI
#

## To be copied in the server
#
