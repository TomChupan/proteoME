#' upload UI Function
#'
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
#' @importFrom shinyjs useShinyjs hidden hide show reset
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
      useShinyjs(),
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
                                      choices = c("imputed","normalized"))
                   )
               },
               if(menuItem_label=="Annotations (sample)"){
                 shinyjs::hidden(
                   actionButton(inputId=ns("edit_factors"),label="Set factors/levels")
                 )
               },
               shinyjs::hidden(actionButton(inputId = ns("reset"),label = "Reset data"))

             ) #menuItem close


              ) #sidebarMenu close

  ) #tagList close
} #mod_upload_ui close

#' upload Server Functions
#'
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
                 includeMarkdown("inst/app/www/helper_abundances.Rmd"),
                 includeMarkdown("inst/app/www/helper_annotations_run.Rmd"),
                 includeMarkdown("inst/app/www/helper_annotations_sample.Rmd")
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
                                        csv = vroom(infile$datapath,show_col_types = F),
                                        txt = vroom(infile$datapath,show_col_types = F)


      )

    }) #observeEvent close


    observeEvent(input$use_example_data,{

        r[[paste0("d",data_type)]]=switch(data_type,
                                          data_example,
                                          ann_run_example,
                                          ann_sample_example)

    }) #observeEvent close

    observeEvent(input$reset,{
      r[[paste0("d",data_type)]]=NULL
    })

    observeEvent(c(input$use_example_data,input$import),{
      req(!is.null(r[[paste0("d",data_type)]]))
      shinyjs::show("reset")
      switch(data_type,
             shinyjs::show("data_char"),
             NULL,
             shinyjs::show("edit_factors")
      )
      shinyjs::hide("import")
      shinyjs::hide("use_example_data")
    })

    observeEvent(input$reset,{
      shinyjs::hide("reset")
      switch(data_type,
             {updateCheckboxGroupInput(session,inputId="data_char",selected = character(0))
             shinyjs::hide("data_char")
             },
             NULL,
             shinyjs::hide("edit_factors")
      )
      shinyjs::reset("import") #just to reset the fileInput progress bar and previous file name
      shinyjs::show("import")
      shinyjs::show("use_example_data")
    })


    ###Edit factor vars/levels/colors----
    observeEvent(input$edit_factors,{
      r$edit_factors_button=ifelse(is.null(r$edit_factors_button),1,r$edit_factors_button+1)
    })


      mod_factors_server("factors_1",r=r)



  }) #moduleServer close
} #server close

## To be copied in the UI
#

## To be copied in the server
#
