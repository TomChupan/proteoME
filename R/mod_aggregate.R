# aggregate UI Function

#' #' @title mod_aggregate_ui and mod_aggregate_server
#' @description A shiny Module containing sidebar content for data aggregation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_aggregate
#'
#' @export
#'
#' @importFrom shinyWidgets ask_confirmation confirmSweetAlert
#' @importFrom shinyjs hide hidden show
mod_aggregate_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}")
    ),
    sidebarMenu(
      menuItem(text = "Aggregate dataset",
               startExpanded = T,
               selectInput(ns("method"),"Aggregate by:",
                           choices = c("mean of runs","median of runs"),multiple = F),
               numericInput(ns("atleast"),"Include into sample if protein was quantified in at least __ % of replicates:",
                            value = 50,min = 1,max=100,step=1),
               actionButton(ns("aggregate"),"Aggregate"),
               shinyjs::hidden(
                 actionButton(ns("aggcheck"),"Aggregated",class="button",
                              icon=icon("check")),
                 actionButton(ns("reset"),"Reset (I need to re-aggregate it)"),
                 downloadButton(ns("download"), "Download (.csv)",
                                style = "background-color: #337ab7; color: #fff")
               )
      ) #menuItemm close
    ) #sidebarMenu close


  ) #tagList close
}

# aggregate Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is aggregated here with one of the available methods and
#' parameters to the sample level (and it can be downloaded).
#' A logical indicator (aggregated or not?) is toggled here.
#' Aggregation method and parameters are saved to r to be used in other modules.
#' @rdname mod_aggregate
#'
#' @export
#'
mod_aggregate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      r$agg_method=input$method
    })

    ####Aggregation process ----

    observeEvent(input$aggregate,{
      req(not_null(r$d1) & not_null(r$d2) & not_null(r$d3))

        ask_confirmation(inputId = "confirm",title = "Are you sure?",
                         text = "Have you checked the post-aggregation form of the dataset in all available visualization tools? The previous form of the dataset (by runs) will be still available.",
                         type = "info",cancelOnDismiss = T,
                         btn_labels = c("No, I'll think about it.","Yes, aggregate it!")
        )

    })

    observeEvent(input$confirm,{
      if(isTRUE(input$confirm)){
        #aggregation procedure and r$d1_ag

        r$aggregatedTF=TRUE
        shinyalert(title = "Your dataset has been successfully aggregated!",
                   text="You can check the dataset via available visualization tools on this tab or proceed to the following analysis steps in a few seconds after closing this window (we need to recalculate something :-).",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("aggregate")
        shinyjs::show("aggcheck")
        shinyjs::show("reset")
        shinyjs::show("download")
      }
    }) #observeEvent confirm close

    ####After aggregation ----
    observeEvent(input$aggcheck,{
      shinyalert(title = "Your dataset has already been aggregated!",
                 text="If you want to change an aggregation method or parameters please click on the reset button below.",
                 showConfirmButton = TRUE, type = "info")
    })

    ####Resets
    observeEvent(input$reset,{
      ask_confirmation(inputId = "reconfirm",title = "Are you sure?",
                       text = "You may lose any filtering, imputation or analysis steps. Make sure you have downloaded the current version of the aggregated dataset before re-aggregating.",
                       type = "info",cancelOnDismiss = T,
                       btn_labels = c("No, I'll think about it.","Yes, I want to re-aggregate it!")
      )
    })

    observeEvent(input$reconfirm,{
      r$aggregatedTF=FALSE
      #r$d1_ag=NULL
      shinyjs::show("aggregate")
      shinyjs::hide("aggcheck")
      shinyjs::hide("reset")
      shinyjs::hide("download")
    })

    ####Download ----
    output$download = downloadHandler(
      filename = function() {
        paste0("d1_aggregated.csv")
      },
      content = function(file) {
        write.csv(r$d1_ag, file)
      }
    )

  })
}

## To be copied in the UI
#

## To be copied in the server
#
