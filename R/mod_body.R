#' body UI Function
#'
#' @title mod_body_ui and mod_body_server
#' @description A shiny Module containing tha main body content. This module is
#' included in a dashboardBody() function within app_ui.R.
#' The body of the app is divided into __ tabs:
#' \itemize{
#' \item{Data import}{Tab for uploading 3 types of data (protein abundances,
#' run annotations, sample annotations) and their basic specification}
#' }
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_body
#'
#' @export
#' @importFrom shiny NS tagList tabsetPanel tabPanel fluidPage
mod_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id="tabs", #globally available in all modules (because of conditional panels)
                tabPanel("Data import",value = "im",
                         fluidPage(
                           fluidRow(
                             mod_body_tab1_ui(ns("body_tab1_1"),
                                              box_title="Abundances",
                                              data_type=1)
                           ), #fluidRow close
                           fluidRow(
                             mod_body_tab1_ui(ns("body_tab1_2"),
                                              box_title="Annotations (run)",
                                              data_type=2),
                             mod_body_tab1_ui(ns("body_tab1_3"),
                                              box_title = "Annotations (sample)",
                                              data_type=3)

                           ) #fluidRow close
                         ) #fluidPage close

                         ), #tabPanel Data import close

                tabPanel("Missing values handling",value = "na"
                         ) #tabPanel Missing values handling close
    ) #tabsetPanel close


  ) #tagList close
}

#' body Server Functions
#'
#' @param r A "storage" for the variables used throughout the app
#'
#' @rdname mod_body
#' @export
mod_body_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_body_tab1_server("body_tab1_1",
                         validate_message="Please upload a file with abundances.",
                         data_type=1,
                         r=r)
    mod_body_tab1_server("body_tab1_2",
                         validate_message="Please upload a file with run annotations.",
                         data_type=2,
                         r=r)
    mod_body_tab1_server("body_tab1_3",
                         validate_message="Please upload a file with sample annotations.",
                         data_type=3,
                         r=r)


  })
}

## To be copied in the UI
#

## To be copied in the server
#
