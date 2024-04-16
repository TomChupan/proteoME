# impute UI Function

#' #' @title mod_impute_ui and mod_impute_server
#' @description A shiny Module containing sidebar content for data imputation
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_impute
#'
#' @export
#'
#' @importFrom shinyWidgets ask_confirmation
#' @importFrom shinyjs hide hidden show
#' @importFrom dplyr %>% mutate_if
#' @importFrom impute impute.knn
#' @importFrom missForest missForest
mod_impute_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}
                  .custom-h4 {margin-left: 10px;}")
    ),
    sidebarMenu(
      menuItem(text = "Impute dataset",
               startExpanded = T,
               selectInput(ns("method"),"Select the imputation method:",
                           choices = c("SampMin"="sampmin",
                                       "kNN"="knn",
                                       "Random forest"="rf"),
                           multiple = F),
               uiOutput(ns("inputspec")),
               shinyjs::hidden(
                 numericInput(ns("setseed"),"Set seed:",min=1,step=1,value=123)
               ),
               actionButton(ns("impute"),"Impute"),
               shinyjs::hidden(
                 actionButton(ns("imcheck"),"Imputed",class="button",
                              icon=icon("check")),
                 downloadButton(ns("download"), "Download (.csv)",
                                style = "background-color: #337ab7; color: #fff")
               )
      ) #menuItemm close
    ) #sidebarMenu close


  ) #tagList close
}

# impute Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is imputed here with one of the available methods and
#' parameters (and it can be downloaded).
#' A logical indicator (imputed or not?) is toggled here.
#' Imputation method and parameters are saved to r to be used in other modules.
#' @rdname mod_impute
#'
#' @export
#'
mod_impute_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      r$im_method=input$method
      r$setseed=input$setseed
      r$ntree=input$ntree
      r$k=input$k
    })

    #####Reactive inputs ----
    output$inputspec=renderUI({
      req(not_null(r$d4))
      if(input$method=="knn"){
        numericInput(ns("k"),"Select 'k' for kNN:",min=1,max=ncol(r$d4)-2,step=1,
                     value=10)
      }else if(input$method=="rf"){
        numericInput(ns("ntree"),"Select a number of trees to grow in each forest:",
                     min=1,max=1000,step=1,value=100)
      }
    })

    observe({
      req(r$imputedTF==FALSE)
      if(input$method=="rf"){
        shinyjs::show("setseed")
      }else{
        shinyjs::hide("setseed")
      }
    })


    ####Imputing process ----

    observeEvent(input$impute,{
      if(is.null(r$d4)){
        shinyalert(title = "Please aggregate your dataset first!",
                   text="Imputation is performed on aggregated data. For aggregation, use the 'Aggregate' tab.",
                   showConfirmButton = TRUE, type = "info")
      }else{
        ask_confirmation(inputId = "confirm",title = "Are you sure?",
                         text = "The previous form of the dataset (non-imputed aggregated/filtered
                         data) will be irretrievably lost and replaced by the imputed dataset!
                         Make sure you have downloaded it or no longer need it.
                         Any analysis results (table, volcano plot) will be deleted.
                         The imported form of the dataset (by runs) will still be available.",
                         type = "info",cancelOnDismiss = T,
                         btn_labels = c("No, I'll think about it.","Yes, impute it!")
        )
      }
    })

    observeEvent(input$confirm,{
      if(isTRUE(input$confirm)){
        if(input$method=="sampmin"){
          r$d4=r$d4 %>%
            mutate_if(is.numeric, function(x) ifelse(is.na(x), min(x, na.rm = T), x))
        }else if(input$method=="knn"){
          knn_imputed=impute::impute.knn(as.matrix(r$d4[,-1]),k = input$k,
                                         rowmax = 1, colmax = 1,maxp=nrow(r$d1))
          r$d4[,-1]=knn_imputed$data
        }else if(input$method=="rf"){
          if(min(apply(r$d4[,-1],1,function(x){sum(!is.na(x))}))<=5){
            shinyalert(title = "You do not have enough data to use this method.",
                       text="Please choose another method or be stricter when filtering your data.",
                       showConfirmButton = TRUE, type = "warning")
          }else if(not_null(r$d4_rf_imp)){
            r$d4[,-1]=r$d4_rf_imp[,-1]
          }else{
          set.seed(input$setseed)
          rf_imputed=missForest::missForest(t(r$d4[,-1]), ntree = input$ntree)
          r$d4[,-1]=t(rf_imputed$ximp)
          }
        }

        if(sum(is.na(r$d4))==0){
        r$imputedTF=TRUE
        r$turnoff_data_char=TRUE
        #Analysis results:
        r$results=NULL
        #Plots:
        r$an_volcano_1=NULL
        #long format:
        r$dAG_pivotlonger[,"abundances"]=c(t(as.matrix(r$d4[,-1])))

        shinyalert(title = "Your dataset has been successfully imputed!",
                   text="You can check the dataset via heatmap on this tab or proceed to the following analysis step.",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("method")
        shinyjs::hide("inputspec")
        shinyjs::hide("setseed")
        shinyjs::hide("impute")
        shinyjs::show("imcheck")
        shinyjs::show("download")
        }

      }
    }) #observeEvent confirm close

    ####After imputation ----
    observeEvent(input$imcheck,{
      shinyalert(title = "Your dataset has already been imputed!",
                 text="If you want to change the imputation method or parameters, you need to
                 re-aggregate the data on the 'Aggregate' tab.",
                 showConfirmButton = TRUE, type = "info")
    })

    #Reset on the ag tab -----
    observe({
      if(r$imputedTF==FALSE){
        shinyjs::show("method")
        shinyjs::show("inputspec")
        shinyjs::show("impute")
        shinyjs::hide("imcheck")
        shinyjs::hide("download")
      }
    })

    #To be ready for re-aggregation:
    observe({
      if(r$imputedTF==TRUE){
        updateSelectInput(session,inputId="method",selected = "sampmin")
      }
    })


    #Imputed from the start ----
    observe({
      if("imputed"%in%r$data_char){
        shinyjs::hide("method")
        shinyjs::hide("inputspec")
        shinyjs::hide("impute")
        shinyjs::show("imcheck")
      }
    })


    ####Download ----
    output$download = downloadHandler(
      filename = function() {
        paste0("d1_imputed.csv")
      },
      content = function(file) {
        write.csv(r$d4, file,row.names = F)
      }
    )

    ####Helpers ----
    #observeEvent(input$help,{
    #  showModal(
    #    modalDialog(
    #      includeMarkdown(app_sys("app/www/helper_im_1.Rmd")),
    #      footer = modalButton("Close"),
    #      size="l",
    #      easyClose = T
    #    )
    #  )
    #})


  })
}

## To be copied in the UI
#

## To be copied in the server
#
