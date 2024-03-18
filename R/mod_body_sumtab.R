# body_sumtab UI Function

#' @title mod_body_sumtab_ui and mod_body_sumtab_server
#' @description A shiny for creating summary tables in the app body.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_body_sumtab
#' @export
#'
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT DTOutput formatRound renderDT datatable
#' @importFrom dplyr %>%
#' @importFrom data.table setDT
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs hide hideElement
mod_body_sumtab_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(id=ns("box1"),width = 12,title=textOutput(ns("aftertrans_title")),
        collapsible = F,status="primary",
        materialSwitch(ns("by_treatment"),h4("By treatment group"),inline=T,
                       status="primary"),
        h4("Current version of the dataset:"),
        DT::DTOutput(ns("sumtab_c")), #"static" table for the current version of dataset
        br(),
        h4("After selected transformation method:"),
        DT::DTOutput(ns("sumtab_t")), #reactive table depending on the selectInput with methods
        br()
        )
  )
}

# body_sumtab Server Functions

#' @param validate_message Message to be displayed when there are no data to build a table.
#' @param r A 'reactiveValues()' list containing (among other objects) all three
#' datasets and also a long-format version (combination of these datasets).
#' This is used in the summary tables. We also use here the selected transformation
#' method from the mod_transform module, the active tab and the information whether
#' the dataset was already transformed (r$transformedTF).
#' @param box_title Character to be used as a box title.
#'
#'
#' @rdname mod_body_sumtab
#' @export
mod_body_sumtab_server <- function(id,box_title="Your title.",
                                   validate_message,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####Data preparation ----
    dTOsumtab_c=reactive({
      validate(need(not_null(r$d1) & not_null(r$d2) & not_null(r$d3),
               validate_message))
      req(r$transformedTF==FALSE)
      r$d_pivotlonger
    })

    dTOsumtab_t=reactive({
      validate(need(not_null(r$d1) & not_null(r$d2) & not_null(r$d3),
               validate_message))
      req(r$transformedTF==FALSE)
      if(all(r$d1[,-1]>0,na.rm = T) | r$trans_method!="log2(x)"){
      d=r$d_pivotlonger
      d$abundances=switch(r$trans_method,
                          "log2(x)"=log2(d$abundances),
                          "log2(x+1)"=log2(d$abundances+1),
                          "sqrt(x)"=sqrt(d$abundances)
      )
      d
      }else{NULL}
    }) #dTOsumtab_t close
    #Info:
    observe({
    if(!(all(r$d1[,-1]>0,na.rm = T) | r$trans_method!="log2(x)")&r$tabset_value=="trans"){
       shinyalert::shinyalert(title = "There are zeros in the dataset!",
                   text = "To display the second table, choose a different method (not log2(x)) to transform the data - e.g. log2(x+1).",
                   showConfirmButton = TRUE, type = "info")
    }
    })


    ####Datatables ----
    ###Current dataset
    output$sumtab_c=DT::renderDT({
      req(r$transformedTF==FALSE)
      if(input$by_treatment){
        setDT(dTOsumtab_c())
        DT::datatable(dTOsumtab_c()[,as.list(Tsummary(abundances)),
                                  by = treatment],rownames=F,selection="none",
                      options = list(searching = F,
                                     pageLength = 6,
                                     lengthChange=F,
                                     scrollX = T,
                                     autoWidth = F,
                                     digits=2
                      )
        ) %>%
          DT::formatRound(columns=2:9, #change when Tsummary changes
                      digits=2)
      }else{
        DT::datatable(Tsummary(dTOsumtab_c()$abundances),rownames=F,selection="none",
                      options = list(searching = F,
                                     pageLength = 6,
                                     lengthChange=F,
                                     scrollX = T,
                                     autoWidth = F,
                                     digits=2
                      )
        ) %>%
          DT::formatRound(columns=1:8, #change when Tsummary changes
                      digits=2)
      }
    }) #renderDT close

    ####Transformed dataset
    output$sumtab_t=DT::renderDT({
      req(r$transformedTF==FALSE)
      if(input$by_treatment){
        setDT(dTOsumtab_t())
        DT::datatable(dTOsumtab_t()[,as.list(Tsummary(abundances)),
                                    by = treatment],rownames=F,selection="none",
                      options = list(searching = F,
                                     pageLength = 6,
                                     lengthChange=F,
                                     scrollX = T,
                                     autoWidth = F,
                                     digits=2
                      )
        ) %>%
          DT::formatRound(columns=2:9, #change when Tsummary changes
                          digits=2)
      }else{
        DT::datatable(Tsummary(dTOsumtab_t()$abundances),rownames=F,selection="none",
                      options = list(searching = F,
                                     pageLength = 6,
                                     lengthChange=F,
                                     scrollX = T,
                                     autoWidth = F,
                                     digits=2
                      )
        ) %>%
          DT::formatRound(columns=1:8, #change when Tsummary changes
                          digits=2)
      }
    }) #renderDT close

    ####After transformation ----
    observe({
      if(r$transformedTF==TRUE){
        shinyjs::hide("box1")
      }
    })

    output$aftertrans_title=renderText({
      if(r$transformedTF==FALSE){
        box_title
      }else{
        "Your dataset has already been transformed. If you want to change a transformation method or return to an untransformed dataset, please reset the data and import it again."
      }
    })


  })
}

## To be copied in the UI
#

## To be copied in the server
#
