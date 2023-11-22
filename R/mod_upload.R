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
#' @importFrom shiny NS tagList conditionalPanel fileInput actionButton checkboxGroupInput observeEvent
#' @importFrom shinydashboard sidebarMenu menuItem
#' @importFrom shinyalert shinyalert
#' @importFrom tools file_ext
mod_upload_ui <- function(id,menuItem_label=c("Abundances","Annotations (run)",
                          "Annotations (sample)"),fileInput_label){
  ns <- NS(id)
  menuItem_label=match.arg(menuItem_label)
  tagList(

    conditionalPanel(condition = "input.tabs=='im'",
                     tags$head(
                       tags$style(
                         HTML("#help {display: inline;
                                       margin: 0px;
                                      }"
                         )
                       )
                     ),
                     sidebarMenu(
                       menuItem(menuItem_label,startExpanded = T,
                                fileInput(ns("import"),
                                          label=HTML(fileInput_label,
                                                     as.character(actionLink(inputId = ns("help",
                                                                             label = "",
                                                                             icon = icon("circle-question")))
                                          ),
                                          placeholder = paste0("Max File Size: ",
                                                               n," MB"),
                                          accept = c(".csv",".txt"),multiple = F),
                                actionButton(ns("use_example_data"),"Use example data"),
                                if(menuItem_label=="Abundances"){
                                checkboxGroupInput(ns("data_char"),"Uploaded data are:",
                                                   choices = c("imputed","normalized"))
                                },
                                if(menuItem_label=="Annotations (sample)"){
                                  actionButton(ns("new_levels"), "Adjust factor levels")
                                }

                              ) #fileInput close

                      ) #menuItem close
                    ) #sidebarMenu close

                ) #conditionalPanel close
  ) #tagList close
} #mod_upload_ui close

#' upload Server Functions
#'
#' @rdname mod_upload
#' @export
#'
mod_upload_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
    ns <- session$ns

    ###Helper----

    observeEvent(input$help, {
      shinyalert(text = "This type of data is supposed to look like this: picture? (html) table?",
                 showConfirmButton = TRUE, type = "info")
    })


    ###Data upload----
    format=reactive({
      file_ext(input$import$name)
    })

    d = reactive({

      if(input$use_example_data){
        d_example
      }else{
        infile = input$import
        if (is.null(infile)) {
          return(NULL)
        }

        data=switch(format(),
                    csv = vroom(input$import$datapath),
                    txt = vroom(input$import$datapath),
                    validate("Invalid file; Please upload a .csv or .txt file")
        )
        data
      }


    })


  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
