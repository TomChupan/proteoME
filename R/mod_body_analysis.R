# body_analysis UI Function

#' @title mod_body_analysis_ui and mod_body_analysis_server
#' @description A shiny module for showing result table and volcano plot in the app body.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title.
#'
#' @rdname mod_body_analysis
#' @export
#'
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom dplyr %>%
#' @importFrom shinyjs hide show
mod_body_analysis_ui <- function(id,box_title="Your title."){
  ns <- NS(id)
  tagList(
    box(width = 12,title=box_title,collapsible = T,status="primary",
        DT::DTOutput(ns("results")),br()
    )
  )
}

# body_analysis Server Functions

#' @param validate_message Message to be displayed when there are no data to build a table.
#' @param r A 'reactiveValues()' list containing ... (will be specified)
#'
#'
#' @rdname mod_body_analysis
#' @export
mod_body_analysis_server <- function(id,validate_message,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ###Datatable ----
    ##Current dataset
    output$results=DT::renderDT({
      validate(need(not_null(r$results),validate_message))
      DT::datatable(r$results,rownames=F,selection="none",
                    extensions = "Buttons",filter='top',
                    options = list(searching = T,
                                   pageLength = 6,
                                   buttons =
                                     list("copy", list(
                                       extend = "collection",
                                       buttons = list(
                                         list(extend="csv",filename="results",title=NULL),
                                         list(extend="excel",filename="results",title=NULL),
                                         list(extend="pdf",filename="results",title=NULL)
                                         ),
                                       text = "Download"
                                     )),
                                   dom = 'Blfrtip',
                                   search = list(
                                     smart = TRUE,
                                     regex = TRUE,
                                     caseInsensitive = TRUE
                                   ),
                                   scrollX = T,
                                   autoWidth = F,
                                   lengthMenu = list(c(6, 12, -1), # declare values
                                                     c(6, 12, "All")) # declare titles
                                   )
        )
    }) #renderDT close



  })
}

## To be copied in the UI
#

## To be copied in the server
#
