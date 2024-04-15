# upload UI Function

#' @title mod_upload_ui and mod_upload_server
#' @description A shiny Module for data upload.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param menuItem_label Character to name the menu item. For upload menu,
#' it can take one of the following values:
#' \itemize{
#'  \item "Abundances"
#'  \item "Annotations (run)"
#'  \item "Annotations (sample)"
#' }
#' @param fileInput_label Character to be used as a fileInput label.
#'
#' @rdname mod_upload
#'
#' @export
#'
#' @importFrom shiny NS tagList conditionalPanel fileInput actionButton checkboxGroupInput updateCheckboxGroupInput observeEvent observe actionLink showModal modalDialog includeMarkdown
#' @importFrom shinydashboard sidebarMenu menuItem
#' @importFrom shinyalert shinyalert
#' @importFrom tools file_ext
#' @importFrom vroom vroom
#' @importFrom shinyjs hidden hide show reset
#' @importFrom tidyr pivot_longer
#' @importFrom shinyWidgets ask_confirmation confirmSweetAlert
mod_upload_ui <- function(id,menuItem_label=c("Abundances","Annotations (run)",
                          "Annotations (sample)"),fileInput_label="Your label."){
  ns <- NS(id)
  menuItem_label=match.arg(menuItem_label)
  tagList(
    tags$head(
      tags$style(
        HTML(paste0("#",ns("help"),"{display: inline;
                     margin: 0px;
                     }"
              ))
                 )
             ),
    sidebarMenu(
      menuItem(menuItem_label,startExpanded = T,

               fileInput(inputId=ns("import"),
               label=HTML(fileInput_label,
                          as.character(
                            actionLink(inputId = ns("help"),
                                                  label = "",
                                                  icon = icon("circle-question")))
                          ),
               placeholder = paste0("Max File Size: 100 MB"),
               accept = c(".csv",".txt"),multiple = F),

               actionButton(inputId=ns("use_example_data"),label="Use example data"),
               if(menuItem_label=="Abundances"){
                 shinyjs::hidden(
                   checkboxGroupInput(inputId=ns("data_char"),label="Uploaded data are:",
                                      choices = c("normalized","imputed")),
                   actionButton(inputId = ns("zerosTOna"),label="Replace zeros with NA's")
                   )
               },
               if(menuItem_label=="Annotations (sample)"){
                 shinyjs::hidden(
                   actionButton(inputId=ns("edit_factors"),label="Set factors/levels")
                 )
               },
               shinyjs::hidden(downloadButton(ns("download"), "Download (.csv)",
                               style = "background-color: #337ab7; color: #fff")
               ),
               shinyjs::hidden(actionButton(inputId = ns("reset"),label = "Reset data"))

             ) #menuItem close


              ) #sidebarMenu close

  ) #tagList close
} #mod_upload_ui close

# upload Server Functions

#' @param r A "storage" for the variables used throughout the app
#' @param data_type Number from 1 to 3 defining the type of the data
#' as following:
#' \itemize{
#'  \item 1: "Abundances"
#'  \item 2: "Annotations (run)"
#'  \item 3: "Annotations (sample)"
#' }
#'
#' @rdname mod_upload
#' @export
#'
mod_upload_server <- function(id,data_type=c(1,2,3),r){
  moduleServer(
    id,
    function(input, output, session){
    ns <- session$ns

    ###Helper----

    observeEvent(input$help,{
      showModal(
        modalDialog(
          switch(data_type,
                 includeMarkdown(app_sys("app/www/helper_abundances.Rmd")),
                 includeMarkdown(app_sys("app/www/helper_annotations_run.Rmd")),
                 includeMarkdown(app_sys("app/www/helper_annotations_sample.Rmd"))
          ),
          footer = modalButton("Close"),
          size="m",
          easyClose = T
        )
      )
    })


    ###Data upload----
    format=reactive({
      file_ext(input$import$name)
    })

    observeEvent(input$import,{
      infile = input$import
      if(is.null(infile)) {
        return(NULL)
      }

      if(!(format() %in% c("csv","txt"))){
        shinyalert(title = "Invalid file format",
                   text = "Please upload a .csv or .txt file.",
                   type = "error")
        return(NULL)
      }

      r[[paste0("d",data_type)]]=switch(format(),
                                        csv = vroom(infile$datapath,show_col_types = F,
                                                    guess_max = Inf),
                                        txt = vroom(infile$datapath,show_col_types = F,
                                                    guess_max = Inf)


      )
      #to data.frame:
      r[[paste0("d",data_type)]]=as.data.frame(r[[paste0("d",data_type)]])

    }) #observeEvent close


    observeEvent(input$use_example_data,{

        r[[paste0("d",data_type)]]=switch(data_type,
                                          data_example,
                                          ann_run_example,
                                          ann_sample_example)

    }) #observeEvent close

    #"Pivot longer" format
    observeEvent(c(input$use_example_data,input$import),{
      req(not_null(r$d1)&not_null(r$d2)&not_null(r$d3))
      d=r$d1 %>%
        tidyr::pivot_longer(!Accession,names_to = "runID",values_to = "abundances")
      d$index=1:nrow(d)
      d=merge(d,r$d2[,c(1:2)],by="runID")
      d=merge(d,r$d3[,c(1:2)],by="sampleID")
      d=d[order(d$index),]
      d$runID=factor(d$runID,levels=names(r$d1)[-1])
      d$sampleID=factor(d$sampleID,levels=r$d3[,"sampleID"])
      d$Accession=factor(d$Accession,levels=r$d1[,"Accession"])
      r$d_pivotlonger=d
    })

    ####Resets ----

    ##Reseting things from r
    observeEvent(input$reset,{
      #Data
      r[[paste0("d",data_type)]]=NULL
      r$d_pivotlonger=NULL
      r$d4=NULL
      r$d_detected=NULL
      r$dAG_pivotlonger=NULL
      #Plots
      r$eda_box_1=NULL
      r$eda_hist_1=NULL
      r$ag_heatmap_1=NULL
      r$ag_box_1=NULL
      r$ag_hist_1=NULL
      r$ag_bar_1=NULL
      #T/F indicators
      if(data_type==1){
        r$transformedTF=FALSE
        r$normalizedTF=FALSE
      }
      r$aggregatedTF=FALSE
      r$filteredTF=FALSE
      r$imputedTF=FALSE

      #Analysis results
      r$results=NULL
    })

    #Show/hide things on the sidebar when uploading/reseting
    observeEvent(c(input$use_example_data,input$import),{
      shinyjs::show("download")
      shinyjs::show("reset")
      switch(data_type,
             {shinyjs::show("data_char")
               req(not_null(r$d1))
               if(sum(r$d1[,-1]==0,na.rm=T)>0){
              shinyjs::show("zerosTOna")
               }
              },
             NULL,
             shinyjs::show("edit_factors")
      )
      shinyjs::hide("import")
      shinyjs::hide("use_example_data")
    },ignoreInit = T)

    #hide 'replace zeros by NA's' after data transformation, normalization etc.:
    observe({
      if(r$transformedTF==T | r$normalizedTF==T | r$aggregatedTF==T|
         r$filteredTF==T | r$imputedTF==T){
        shinyjs::hide("zerosTOna")
      }
    })


    observeEvent(input$reset,{
      shinyjs::hide("download")
      shinyjs::hide("reset")
      switch(data_type,
             {updateCheckboxGroupInput(session,inputId="data_char",selected = character(0))
             shinyjs::hide("data_char")
             if(sum(r$d1[,-1]==0,na.rm=T)>0){
             shinyjs::hide("zerosTOna")
             }
             r$data_char=NULL
             },
             NULL,
             shinyjs::hide("edit_factors")
      )
      shinyjs::reset("import") #just to reset the fileInput progress bar and previous file name
      shinyjs::show("import")
      shinyjs::show("use_example_data")
    })

    ###Data characteristics - checkbox -----
    observe({
      r$data_char=input$data_char
      if("normalized"%in%input$data_char){
        r$normalizedTF=TRUE
      }else{
        r$normalizedTF=FALSE
      }
      if("imputed"%in%input$data_char){
        r$filteredTF=TRUE
        r$imputedTF=TRUE
      }else{
        r$filteredTF=FALSE
        r$imputedTF=FALSE
      }
    })

    observe({
      if(r$turnoff_data_char==TRUE){
        hide("data_char")
      }
    })

    #### Zeros to NA's -----
    observeEvent(input$zerosTOna,{
      ask_confirmation(inputId = "confirm",title = "Are you sure?",
                       text = "All zeros will be replaced by 'NA'. The previous form of
                       the dataset will be irretrievably lost! Make sure you have downloaded it
                       or no longer need it.",
                       type = "info",cancelOnDismiss = T,
                       btn_labels = c("No, keep zeros in the data.","Yes, replace them all!")
      )
    })

    observeEvent(input$confirm,{
      if(isTRUE(input$confirm)){
        r$d1[r$d1==0]<-NA
        if(not_null(r$d_pivotlonger)){
        zeroindL=which(r$d_pivotlonger$abundances==0)
        r$d_pivotlonger[zeroindL,"abundances"]=NA
        }

        #reset:
        #Plots
        r$eda_box_1=NULL
        r$eda_hist_1=NULL
        r$ag_heatmap_1=NULL
        r$ag_bar_1=NULL

        shinyalert(title = "You have replaced all zeros with 'NA' value.",
                   showConfirmButton = TRUE, type = "success")
        shinyjs::hide("zerosTOna")
      }
    }) #observeEvent confirm close


    ###Edit factor vars/levels/colors----
    observeEvent(input$edit_factors,{
      r$edit_factors_button=ifelse(is.null(r$edit_factors_button),1,r$edit_factors_button+1)
    })


      mod_factors_server("factors_1",r=r)

    ###Download button----
      output$download = downloadHandler(
        filename = function() {
          paste0("d",data_type,".csv")
        },
        content = function(file) {
          write.csv(r[[paste0("d",data_type)]], file,row.names = F)
        }
      )




  }) #moduleServer close
} #server close

## To be copied in the UI
#

## To be copied in the server
#
