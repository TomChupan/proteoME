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

    conditionalPanel(condition = "input.tabs=='im'",
    mod_upload_ui(ns("upload_1"),
                  menuItem_label="Abundances",
                  fileInput_label="Upload your dataset with abundances (.csv/.txt)"),
    mod_upload_ui(ns("upload_2"),
                  menuItem_label="Annotations (run)",
                  fileInput_label="Upload your run annotation dataset (.csv/.txt)"),
    mod_upload_ui(ns("upload_3"),
                  menuItem_label="Annotations (sample)",
                  fileInput_label="Upload your sample annotation dataset (.csv/.txt)")

    ) #conditionalPanel close

  ) #tagList close
}

#' sidebar Server Functions
#'
#' @param r A "storage" for the variables used throughout the app
#'
#' @rdname mod_sidebar
#' @export
mod_sidebar_server <- function(id,r){
  moduleServer(
    id,
    function(input, output, session){
    ns <- session$ns

    mod_upload_server("upload_1",data_type=1,r=r)
    mod_upload_server("upload_2",data_type=2,r=r)
    mod_upload_server("upload_3",data_type=3,r=r)

  })
}

## To be copied in the UI
#

## To be copied in the server
#
