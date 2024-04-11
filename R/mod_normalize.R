# normalize UI Function

#' @title mod_normalize_ui and mod_normalize_server
#' @description A shiny Module containing sidebar content for data normalization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_normalize
#'
#' @export
#' @importFrom shinyWidgets ask_confirmation
#' @importFrom shinyjs hide hidden show
#' @importFrom preprocessCore normalize.quantiles
#' @importFrom MBQN mbqn
mod_normalize_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}")
    ),
    sidebarMenu(
      menuItem(text = "Normalize dataset",
               startExpanded = T,
               selectInput(ns("method"),"Select the method:",
                           choices = c("median centering"="median",
                                       "quantile normalization"="quantile",
                                       "MBQN (median)"="mbqn_med",
                                       "MBQN (mean)"="mbqn_mean"
                                       ),multiple = F),
               actionButton(ns("normalize"),"Normalize"),
               shinyjs::hidden(
                 actionButton(ns("normcheck"),"Normalized",class="button",
                              icon=icon("check"))
               )
      ) #menuItemm close
    ) #sidebarMenu close

  )
}

# normalize Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is normalized here with one of the available methods. The original
#' data set is overwritten. A logical indicator (normalized or not?) is
#' toggled here. Normalization method is saved to r to be used in
#' other modules.
#'
#' @rdname mod_normalize
#' @export
mod_normalize_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      r$norm_method=input$method
    })

    ####Normalization process ----

    observeEvent(input$normalize,{
      req(not_null(r$d1) & not_null(r$d2) & not_null(r$d3))
        ask_confirmation(inputId = "confirm",title = "Are you sure?",
                         text = "The previous form of the dataset will be irretrievably lost! Make sure you have downloaded it or no longer need it.",
                         type = "info",cancelOnDismiss = T,
                         btn_labels = c("No, I'll think about it.","Yes, normalize it!")
        )
    })

    observeEvent(input$confirm,{
      if(isTRUE(input$confirm)){
        r$d1[,-1]=switch(input$method,
                         "median"=sweep(r$d1[,-1], 2L, #data, by rows/cols
                                        apply(r$d1[,-1], 2L, median, na.rm = TRUE), #vector of column medians
                                        FUN = "-", check.margin = FALSE), #subtract it and don't check dimension
                         "quantile"=preprocessCore::normalize.quantiles(as.matrix(r$d1[,-1])),
                         "mbqn_med"=MBQN::mbqn(as.matrix(r$d1[,-1]),FUN = "median",
                                           method="preprocessCore"),
                         "mbqn_mean"=MBQN::mbqn(as.matrix(r$d1[,-1]),FUN = "mean",
                                               method="preprocessCore")
        )
        r$d_pivotlonger[,"abundances"]=c(t(as.matrix(r$d1[,-1])))
        r$eda_box_1=NULL
        r$eda_hist_1=NULL
        r$normalizedTF=TRUE
        r$turnoff_data_char=TRUE
        shinyalert(title = "Your dataset has been successfully normalized!",
                   text="You can check the dataset on the previous tabs or proceed to the following analysis steps in a few seconds after closing this window (we need to recalculate something :-).",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("normalize")
        shinyjs::show("normcheck")
      }
    }) #observeEvent confirm close

    ####After normalization ----
    observeEvent(input$normcheck,{
      shinyalert(title = "Your dataset has already been normalized!",
                 text="This action can be done only once. If you want to change a normalization method or return to an unnormalized dataset, please reset the data and import it again.",
                 showConfirmButton = TRUE, type = "info")
    })

    ####Resets----
    observe({
      if(r$normalizedTF==FALSE){
        shinyjs::show("normalize")
        shinyjs::hide("normcheck")
      }
    })

    ####Normalized from the start ----
    observe({
      if("normalized"%in%r$data_char){
        shinyjs::hide("normalize")
        shinyjs::show("normcheck")
      }
    })


  })
}

## To be copied in the UI
#

## To be copied in the server
#
