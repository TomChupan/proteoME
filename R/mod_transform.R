# transform UI Function

#' @title mod_transform_ui and mod_transform_server
#' @description A shiny Module containing sidebar content for data transformation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shinyWidgets ask_confirmation
#' @importFrom shinyjs hide hidden show
mod_transform_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}")
      ),
    sidebarMenu(
      menuItem(text = "Transform dataset",
               startExpanded = T,
               selectInput(ns("method"),"Select the method:",
                           choices = c("log2(x)","log2(x+1)","sqrt(x)"),multiple = F),
               actionButton(ns("transform"),"Transform"),
               shinyjs::hidden(
                 actionButton(ns("transcheck"),"Transformed",class="button",
                              icon=icon("check"))
                 )
      ) #menuItemm close
    ) #sidebarMenu close

  ) #tagList close
}

# transform Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is transformed here with one of the available methods. The original
#' data set is overwritten. A logical indicator (transformed or not?) is
#' toggled here. Transformation method is saved to r to be used in
#' other modules.
#'
#' @noRd
mod_transform_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      r$trans_method=input$method
    })

    ####Transformation process ----

    observeEvent(input$transform,{
      req(not_null(r$d1) & not_null(r$d2) & not_null(r$d3))
      if(all(r$d1[,-1]>0,na.rm = T) | input$method!="log2(x)"){
      ask_confirmation(inputId = "confirm",title = "Are you sure?",
                       text = "The previous form of the dataset will be irretrievably lost! Make sure you have downloaded it or no longer need it.
                       You will also lost any aggregation, filtering or imputation steps as well
                       as analysis results.",
                       type = "info",cancelOnDismiss = T,
                       btn_labels = c("No, I'll think about it.","Yes, transform it!")
                       )
      }else{
        shinyalert(title = "There are zeros in the dataset!",
                   text = "Cannot logarithm the data, please choose another transformation method - e.g. log2(x+1).",
                   showConfirmButton = TRUE, type = "error")
      }
    })

    observeEvent(input$confirm,{
      if(isTRUE(input$confirm)){
        r$d1[,-1]=switch(input$method,
                         "log2(x)"=log2(r$d1[,-1]),
                         "log2(x+1)"=log2(r$d1[,-1]+1),
                         "sqrt(x)"=sqrt(r$d1[,-1])
                         )
        r$d_pivotlonger[,"abundances"]=c(t(as.matrix(r$d1[,-1])))
        r$eda_box_1=NULL
        r$eda_hist_1=NULL
        #Indicator:
        r$transformedTF=TRUE
        r$aggregatedTF=FALSE
        r$filteredTF=FALSE
        r$imputedTF=FALSE
        #Data:
        r$d4=NULL
        r$dAG_pivotlonger=NULL
        #Plots:
        r$ag_box_1=NULL
        r$ag_hist_1=NULL
        #Analysis results:
        r$results=NULL

        shinyalert(title = "Your dataset has been successfully transformed!",
                   text="You can check the dataset on the previous tabs or proceed to the following analysis steps in a few seconds after closing this window (we need to recalculate something :-).",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("transform")
        shinyjs::show("transcheck")
      }
    }) #observeEvent confirm close

    ####After transformation ----
    observeEvent(input$transcheck,{
      shinyalert(title = "Your dataset has already been transformed!",
                 text="This action can be done only once. If you want to change a transformation method or return to an untransformed dataset, please reset the data and import it again.",
                 showConfirmButton = TRUE, type = "info")
    })

    ####Resets
    observe({
      if(r$transformedTF==FALSE){
        shinyjs::show("method")
        shinyjs::show("transform")
        shinyjs::hide("transcheck")
      }else{
        shinyjs::hide("method")
        shinyjs::hide("transform")
        shinyjs::show("transcheck")
      }
    })


  })
}

## To be copied in the UI
#

## To be copied in the server
#
