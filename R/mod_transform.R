# transform UI Function

#' @title mod_transform_ui and mod_transform_server
#' @description A shiny Module containing sidebar content for data transformation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_transform
#'
#' @export
#' @importFrom shinyWidgets ask_confirmation confirmSweetAlert
#' @importFrom shinyjs hide hidden show
mod_transform_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}")
      ),
    sidebarMenu(
      menuItem(text = "Transform data set",
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

  )
}

# transform Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is transformed here with one of the available methods. The original
#' data set is overwritten.
#'
#' @rdname mod_transform
#' @export
mod_transform_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      r$trans_method=input$method
    })

    ####Transformation process ----

    observeEvent(input$transform,{
      if(all(r$d1[,-1]>0,na.rm = T) | input$method!="log2(x)"){
      ask_confirmation(inputId = "confirm",title = "Are you sure?",
                       text = "The previous form of the dataset will be irretrievably lost! Make sure you have downloaded it or no longer need it.",
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
        r$eda_box_1=NULL
        r$eda_hist_1=NULL
        r$transformedTF=TRUE
        shinyalert(title = "Your dataset has been successfully transformed!",
                   text="You can check the dataset on the previous tabs or proceed to the following analysis steps in a few seconds after closing this window (we need to recalculate something :-).",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("transform")
        shinyjs::show("transcheck")
      }
    }) #observeEvent confirm close

    ####After transformation ----
    observeEvent(input$transcheck,{
      shinyalert(title = "Your dataset has already been  transformed",
                 text="This action can be done only once. If you want to change a transformation method or return to an untransformed dataset, please reset the data and import it again.",
                 showConfirmButton = TRUE, type = "info")
    })


  })
}

## To be copied in the UI
#

## To be copied in the server
#
