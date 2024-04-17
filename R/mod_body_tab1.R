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
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow tableOutput validate need
#' @importFrom shinydashboard box valueBox valueBoxOutput renderValueBox
#' @importFrom DT DTOutput datatable renderDT formatRound
mod_body_tab1_ui <- function(id,box_title="Your title.",data_type=c(1,2,3,4)){
  ns <- NS(id)
  tagList(

        box(
          width=switch(data_type,8,6,6,8),height = '30%',title = box_title,
          collapsible = ifelse(data_type %in% c(1,4),F,T),status="primary", #just for CSS
           DTOutput(ns("tab")),br()
        ), #box close
        if(data_type %in% c(1,4)){
          column(4,
          valueBoxOutput(ns("n_proteins"),width=12),
          if(data_type==1){
          valueBoxOutput(ns("n_runs"),width = 12)
            },
          valueBoxOutput(ns("n_samples"),width = 12),
          if(data_type==4){
            valueBoxOutput(ns("empty_rows"),width = 12)
          }
          ) #column close
          } #if close


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
#' @noRd
mod_body_tab1_server <- function(id,
                                 validate_message="Please upload missing file!",
                                 data_type=c(1,2,3,4),
                                 r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dTOtab=reactive({
      switch(data_type,
             r$d1,
             r$d2,
             r$d3,
             r$d4
             )
    })

    output$tab=renderDT({
      validate(need(not_null(dTOtab()), validate_message))
      datatable(dTOtab(), rownames=F,selection="none",
                options = list(searching = F,
                               pageLength = 6,
                               lengthChange=F,
                               scrollX = T,
                               autoWidth = F
                              )
                ) %>%
        DT::formatRound(columns = which(sapply(r[[paste0("d",data_type)]],
                                               is.numeric)),
                        digits = ifelse(data_type %in% c(1,4),2,0))

    }) #renderDataTable close

    output$n_proteins=renderValueBox({
      if(data_type==1){ #not aggregated
      if(is.null(r$d1)){
        valueBox("No file loaded","Total number of proteins (rows)",icon=icon("list"))
      }else{
        valueBox(nrow(r$d1),"Total number of proteins (rows)",icon=icon("list"),color="green")
      }
      }else{ #aggregated
        if(is.null(r$d4)){
          valueBox("Not aggregated","Total number of proteins (rows)",icon=icon("list"))
        }else{
          valueBox(nrow(r$d4),"Total number of proteins (rows)",icon=icon("list"),color="green")
        }
      }

    }) #renderValueBox close

    output$n_runs=renderValueBox({
      if(is.null(r$d1)){
        valueBox("No file loaded","Number of runs (columns)",icon=icon("people-arrows"))
      }else{
        valueBox(ncol(r$d1)-1,"Number of runs (columns)",icon=icon("people-arrows"),color="green")
      }

    }) #renderValueBox close

    output$n_samples=renderValueBox({
      if(data_type==1){ #not aggregated
      if(is.null(r$d2)){
        valueBox("No file loaded","Number of samples",icon=icon("person"))
      }else{
        valueBox(nrow(as.data.frame(unique(r$d2[,2]))),"Number of samples",icon=icon("person"),color="green")
      }
      }else{ #aggregated
        if(is.null(r$d4)){
          valueBox("Not aggregated","Number of samples",icon=icon("person"))
        }else{
          valueBox(ncol(r$d4)-1,"Number of samples",icon=icon("person"),color="green")
        }
      }

    }) #renderValueBox close

    output$empty_rows=renderValueBox({
      if(is.null(r$d4)){
        valueBox("Not aggregated","Number of empty rows (not detected proteins)",icon=icon("xmark"))
      }else{
        valueBox(sum(apply(r$d4[,-1],1,function(x) sum(is.na(x)))==(ncol(r$d4)-1)),"Number of empty rows (not detected proteins)",icon=icon("xmark"),color="red")
      }

    }) #renderValueBox close


  }) #moduleServer close
}

## To be copied in the UI
#

## To be copied in the server
#
