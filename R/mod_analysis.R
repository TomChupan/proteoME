# analysis UI Function

#' #' @title mod_analysis_ui and mod_analysis_server
#' @description A shiny Module containing sidebar content for data analysis
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_analysis
#'
#' @export
#'
#' @importFrom shinyWidgets ask_confirmation confirmSweetAlert
#' @importFrom shinyjs hide hidden show
#' @importFrom dplyr %>%
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback

mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}
                  .custom-h4 {margin-left: 10px;}")
    ),
    sidebarMenu(
      menuItem(text = "Analyse dataset",
               startExpanded = T,
               uiOutput(ns("input_ngroups")),
               uiOutput(ns("input_groups")),
               uiOutput(ns("input_method"))


               #actionButton(ns("analysis"),"analysis"),
               #shinyjs::hidden(
               #  actionButton(ns("imcheck"),"analysisd",class="button",
               #               icon=icon("check")),
               #  downloadButton(ns("download"), "Download (.csv)",
               #                 style = "background-color: #337ab7; color: #fff")
               #)
      ) #menuItemm close
    ) #sidebarMenu close


  ) #tagList close
}

# analysis Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is analysed here with one of the available methods.
#' A logical indicator (analysed or not?) is toggled here.
#' Analysis method _______ are saved to r to be used in other modules.
#' @rdname mod_analysis
#'
#' @export
#'
mod_analysis_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #observe({
    #  req(input$ngroups)
    #  r$an_method=input$method
    #})

    #####Reactive inputs ----
    output$input_ngroups=renderUI({
      req(not_null(r$d3))
      numericInput(ns("ngroups"),"Number of groups to compare:",
                   min=2,max=length(levels(as.factor(r$d3$treatment))),
                   value=length(levels(as.factor(r$d3$treatment))),step=1)
    })

    output$input_groups=renderUI({
      req(input$ngroups)
      if(input$ngroups!=length(levels(as.factor(r$d3$treatment))))
      selectInput(ns("groups"),"Groups to compare:",
                  choices = levels(as.factor((r$d3$treatment))),multiple = T)
    })
    #shinyFeedback for this input (we need it to be equal to input$ngroups):
    observeEvent(input$groups,{
      if (length(input$groups)!=input$ngroups){
        showFeedbackWarning(
          inputId = "groups",
          text = "You should pick a number of groups selected in the previous input."
        )
      }else{
        hideFeedback("groups")
      }
    })

    output$input_method=renderUI({
      req(input$ngroups)
      selectInput(ns("method"),"Select a method for comparing groups:",
                  choices = {if(input$ngroups==2){
                    c("t-test"="t.test",
                      "Wilcoxon signed-rank test"="wilcoxon")
                  }else{
                    c("ANOVA"="anova","Kruskal–Wallis test"="kw.test")
                  }
                  },multiple = F)
    })

    #####Imputing process ----
#
    #observeEvent(input$analysis,{
    #  if(is.null(r$d4)){
    #    shinyalert(title = "Please aggregate your dataset first!",
    #               text="Imputation is performed on aggregated data. For aggregation, use the 'Aggregate' tab.",
    #               showConfirmButton = TRUE, type = "info")
    #  }else{
    #    ask_confirmation(inputId = "confirm",title = "Are you sure?",
    #                     text = "The previous form of the dataset (non-analysisd aggregated/filtered
    #                     data) will be irretrievably lost and replaced by the analysisd dataset!
    #                     Make sure you have downloaded it or no longer need it.
    #                     The imported form of the dataset (by runs) will still be available.",
    #                     type = "info",cancelOnDismiss = T,
    #                     btn_labels = c("No, I'll think about it.","Yes, analysis it!")
    #    )
    #  }
    #})
#
    #observeEvent(input$confirm,{
    #  if(isTRUE(input$confirm)){
    #    if(input$method=="sampmin"){
    #      r$d4=r$d4 %>%
    #        mutate_if(is.numeric, function(x) ifelse(is.na(x), min(x, na.rm = T), x))
    #    }else if(input$method=="knn"){
    #      knn_analysisd=analysis::analysis.knn(as.matrix(r$d4[,-1]),k = input$k,
    #                                     rowmax = 1, colmax = 1,maxp=nrow(r$d1))
    #      r$d4[,-1]=knn_analysisd$data
    #    }else if(input$method=="rf"){
    #      if(min(apply(r$d4[,-1],1,function(x){sum(!is.na(x))}))<=5){
    #        shinyalert(title = "You do not have enough data to use this method.",
    #                   text="Please choose another method or be stricter when filtering your data.",
    #                   showConfirmButton = TRUE, type = "warning")
    #      }else if(not_null(r$d4_rf_imp)){
    #        r$d4[,-1]=r$d4_rf_imp[,-1]
    #      }else{
    #        set.seed(input$setseed)
    #        rf_analysisd=missForest::missForest(t(r$d4[,-1]), ntree = input$ntree)
    #        r$d4[,-1]=t(rf_analysisd$ximp)
    #      }
    #    }
#
    #    if(sum(is.na(r$d4))==0){
    #      r$analysisdTF=TRUE
    #      r$turnoff_data_char=TRUE
    #      #long format:
    #      r$dAG_pivotlonger[,"abundances"]=c(t(as.matrix(r$d4[,-1])))
#
    #      shinyalert(title = "Your dataset has been successfully analysisd!",
    #                 text="You can check the dataset via _______ on this tab or proceed to the following analysis step.",
    #                 showConfirmButton = TRUE, type = "success")
    #      shinyjs::hide("method")
    #      shinyjs::hide("inputspec")
    #      shinyjs::hide("setseed")
    #      shinyjs::hide("analysis")
    #      shinyjs::show("imcheck")
    #      shinyjs::show("download")
    #    }
#
    #  }
    #}) #observeEvent confirm close
#
    #####After imputation ----
    #observeEvent(input$imcheck,{
    #  shinyalert(title = "Your dataset has already been analysisd!",
    #             text="If you want to change the imputation method or parameters, you need to
    #             re-aggregate the data on the 'Aggregate' tab.",
    #             showConfirmButton = TRUE, type = "info")
    #})
#
    ##Reset on the ag tab -----
    #observe({
    #  if(r$analysisdTF==FALSE){
    #    shinyjs::show("method")
    #    shinyjs::show("inputspec")
    #    shinyjs::show("analysis")
    #    shinyjs::hide("imcheck")
    #    shinyjs::hide("download")
    #  }
    #})
#
    ##To be ready for re-aggregation:
    #observe({
    #  if(r$analysisdTF==TRUE){
    #    updateSelectInput(session,inputId="method",selected = "sampmin")
    #  }
    #})
#
#
    ##analysisd from the start ----
    #observe({
    #  if("analysisd"%in%r$data_char){
    #    shinyjs::hide("method")
    #    shinyjs::hide("inputspec")
    #    shinyjs::hide("analysis")
    #    shinyjs::show("imcheck")
    #  }
    #})
#
#
    #####Download ----
    #output$download = downloadHandler(
    #  filename = function() {
    #    paste0("d1_analysisd.csv")
    #  },
    #  content = function(file) {
    #    write.csv(r$d4, file,row.names = F)
    #  }
    #)

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
