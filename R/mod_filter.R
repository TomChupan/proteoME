# filter UI Function

#' #' @title mod_filter_ui and mod_filter_server
#' @description A shiny Module containing sidebar content for data filtering.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_filter
#'
#' @export
#'
#' @importFrom shinyWidgets ask_confirmation confirmSweetAlert
#' @importFrom shinyjs hide hidden show
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
mod_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}
                  .custom-h4 {margin-left: 10px;}")
    ),
    sidebarMenu(
      menuItem(text = "Filter dataset",
               startExpanded = T,
               selectInput(ns("method"),"Keep protein when it is quantified in at least: ",
                           choices = c("n % of all samples"="all",
                                       "n % of samples within at least one treatment group"="onegroup",
                                       "n % of samples within each treatment group"="eachgroup"),
                           multiple = F),
               sliderInput(ns("atleast"),"n:",
                           value = 15,min = 1,max=100,step=1),
               actionButton(ns("filter"),"Filter"),
               shinyjs::hidden(
                 actionButton(ns("filcheck"),"Filtered",class="button",
                              icon=icon("check")),
                 downloadButton(ns("download"), "Download (.csv)",
                                style = "background-color: #337ab7; color: #fff")
               )
      ) #menuItemm close
    ) #sidebarMenu close


  ) #tagList close
}

# filter Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is filtered here with one of the available methods and
#' parameters (and it can be downloaded).
#' A logical indicator (filtered or not?) is toggled here.
#' Filtering method and parameters are saved to r to be used in other modules.
#' @rdname mod_filter
#'
#' @export
#'
mod_filter_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      r$fil_method=input$method
      r$fil_n=input$atleast
    })

    ####Filtering process ----

    observeEvent(input$filter,{
      if(is.null(r$d4)){
        shinyalert(title = "Please aggregate your dataset first!",
                   text="Filtering is performed on aggregated data. For aggregation, use the previous tab.",
                   showConfirmButton = TRUE, type = "info")
      }else{
        ask_confirmation(inputId = "confirm",title = "Are you sure?",
                         text = "The previous form of the dataset (non-filtered aggregated
                         data) will be irretrievably lost and replaced by the filtered dataset!
                         Make sure you have downloaded it or no longer need it.
                         The imported form of the dataset (by runs) will still be available.",
                         type = "info",cancelOnDismiss = T,
                         btn_labels = c("No, I'll think about it.","Yes, filter it!")
        )
      }
    })

    observeEvent(input$confirm,{
      if(isTRUE(input$confirm)){
        r$d4=proteoFI(r$d4,r$d3,input$atleast,input$method)
        r$filteredTF=TRUE
        r$turnoff_data_char=TRUE
        #long format:
        d=r$dAG_pivotlonger
        d=d[which(d$Accession%in%unlist(r$d4[,"Accession"])),]
        r$dAG_pivotlonger=d

        shinyalert(title = "Your dataset has been successfully filtered!",
                   text="You can check the dataset via missingness plots on this tab or proceed to the following analysis steps.",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("method")
        shinyjs::hide("atleast")
        shinyjs::hide("filter")
        shinyjs::show("filcheck")
        shinyjs::show("download")
      }
    }) #observeEvent confirm close

    ####After filtering ----
    observeEvent(input$filcheck,{
      shinyalert(title = "Your dataset has already been filtered!",
                 text="If you want to change the filtering method or parameters, you need to
                 re-aggregate the data on the previous tab.",
                 showConfirmButton = TRUE, type = "info")
    })

    #Reset on the ag tab -----
    observe({
      if(r$filteredTF==FALSE){
        shinyjs::show("method")
        shinyjs::show("atleast")
        shinyjs::show("filter")
        shinyjs::hide("filcheck")
        shinyjs::hide("download")
      }
    })

    #Imputed from the start ----
    observe({
      if("imputed"%in%r$data_char){
        shinyjs::hide("method")
        shinyjs::hide("atleast")
        shinyjs::hide("filter")
        shinyjs::show("filcheck")
      }
    })


    ####Download ----
    output$download = downloadHandler(
      filename = function() {
        paste0("d1_filtered.csv")
      },
      content = function(file) {
        write.csv(r$d4, file,row.names = F)
      }
    )

    ####Helpers ----
    #observeEvent(input$help,{
    #  showModal(
    #    modalDialog(
    #      includeMarkdown(app_sys("app/www/helper_fi__1.Rmd")),
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
