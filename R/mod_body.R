# body UI Function

#' @title mod_body_ui and mod_body_server
#' @description A shiny Module containing tha main body content. This module is
#' included in a dashboardBody() function within app_ui.R.
#' The body of the app is divided into __ tabs:
#' \itemize{
#' \item{Data import}{Tab for uploading 3 types of data (protein abundances,
#' run annotations, sample annotations) and their basic specification}
#' }
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny NS tagList tabsetPanel tabPanel fluidPage
#' @importFrom utils str
mod_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(id=ns("tabs"),
                tabPanel("Data import",value = "im",
                         fluidPage(
                           fluidRow(
                             mod_body_tab1_ui(ns("body_tab1_1"),
                                              box_title="Abundances",
                                              data_type=1)
                           ), #fluidRow close
                           fluidRow(
                             mod_body_tab1_ui(ns("body_tab1_2"),
                                              box_title="Annotations (run)",
                                              data_type=2),
                             mod_body_tab1_ui(ns("body_tab1_3"),
                                              box_title = "Annotations (sample)",
                                              data_type=3)

                           ) #fluidRow close
                         ) #fluidPage close

                         ), #tabPanel Data import close

                tabPanel("Exploratory data analysis",value="eda",
                         fluidPage(
                           fluidRow(
                             mod_body_plot_ui(ns("body_plot_1"),
                                              box_title="Boxplot of abundances by runs")
                           ), #fluidRow close
                           fluidRow(
                             mod_body_plot_ui(ns("body_plot_2"),
                                              box_title="Histogram of detected proteins in each run")
                           ) #fluidRow close
                         ) #fluidPage close
                ), #tabPanel EDA close
                tabPanel("Transformation",value = "trans",
                         fluidPage(
                           fluidRow(
                             mod_body_sumtab_ui(ns("body_sumtab_1"),
                                                box_title = "Summary statistics")
                           ) #fluidRow close
                         ) #fluidPage close
                ), #tabPanel Transformation close
                tabPanel("Normalization",value = "norm",
                         fluidPage(
                           fluidRow(
                             mod_body_normalize_ui(ns("body_normalize_1"),
                                                   box_title = "Boxplots of abundances")
                           ) #fluidRow close
                         ) #fluidPage close
                ), #tabPanel Normalization close
                tabPanel("Aggregation",value = "ag",
                         fluidPage(
                           h3("Before the aggregation:"),
                           fluidRow(
                             mod_body_plot_ui(ns("body_plot_5"),
                                              box_title="Barplot of number of detections")
                           ), #fluidRow close
                           fluidRow(
                             mod_body_aggregate_ui(ns("body_aggregate_1"),
                                              box_title="Heatmap of detections within a sample")
                           ), #fluidRow close
                           h3("After the aggregation:"),
                           fluidRow(
                             mod_body_tab1_ui(ns("body_tab1_4"),
                                              box_title="Aggregated abundances",
                                              data_type=4)
                           ), #fluidRow close
                           fluidRow(
                             mod_body_plot_ui(ns("body_plot_3"),
                                              box_title="Boxplot of abundances by samples")
                           ), #fluidRow close
                           fluidRow(
                             mod_body_plot_ui(ns("body_plot_4"),
                                              box_title="Histogram of detected proteins in each sample")
                           ) #fluidRow close
                         ) #fluidPage close
                ), #tabPanel Aggregation close
                tabPanel("Filtering",value = "f",
                         fluidPage(
                           fluidRow(
                             mod_body_filter_ui(ns("body_filter_1"),
                                                   box_title="Missingness exploration")
                           ) #fluidRow close
                         ) #fluidPage close
                ), #tabPanel Filtering close
                tabPanel("Imputation",value = "na",
                         fluidPage(
                           fluidRow(
                             mod_body_impute_ui(ns("body_impute_1"),
                                                box_title="Heatmap of protein abundances")
                           ) #fluidRow close
                         ) #fluidPage close
                         ), #tabPanel Imputation close
                tabPanel("Analysis",value = "an",
                         fluidPage(
                           fluidRow(
                             mod_body_analysis_ui(ns("body_analysis_1"),
                                                box_title = "Results of the selected test")
                           ), #fluidRow close
                           fluidRow(
                             mod_body_plot_ui(ns("body_plot_6"),
                                              box_title="Volcano plot")
                           ), #fluidRow close
                         ) #fluidPage close
                )#, #tabPanel Analysis close
                #tabPanel("Log",value = "log" ....to be finished
                #), #tabPanel Log close
    ) #tabsetPanel close


  ) #tagList close
}

# body Server Functions

#' @param r A "storage" for the variables used throughout the app
#'
#' @noRd
mod_body_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$tabs,{
      r$tabset_value=input$tabs
    })

    #####Tab 1----
    mod_body_tab1_server("body_tab1_1",
                         validate_message="Please upload a file with abundances.",
                         data_type=1,
                         r=r)
    mod_body_tab1_server("body_tab1_2",
                         validate_message="Please upload a file with run annotations.",
                         data_type=2,
                         r=r)
    mod_body_tab1_server("body_tab1_3",
                         validate_message="Please upload a file with sample annotations.",
                         data_type=3,
                         r=r)

    #####Tab 2 ----
    mod_body_plot_server("body_plot_1",
                         plot_type="eda_box_1",
                         r=r)
    mod_body_plot_server("body_plot_2",
                         plot_type="eda_hist_1",
                         r=r)

    #####Tab 3 ----
    mod_body_sumtab_server("body_sumtab_1",
                           validate_message="Please upload all files.",
                           r=r)

    #####Tab 4 ----
    mod_body_normalize_server("body_normalize_1",
                              validate_message="Please upload all files.",
                              r=r)

    #####Tab 5 ----
    mod_body_plot_server("body_plot_5",
                         plot_type="ag_bar_1",
                         r=r)
    mod_body_aggregate_server("body_aggregate_1",
                         r=r)
    mod_body_tab1_server("body_tab1_4",
                         validate_message="Please aggregate your data with abundances.",
                         data_type=4,
                         r=r)
    mod_body_plot_server("body_plot_3",
                         plot_type="ag_box_1",
                         r=r)
    mod_body_plot_server("body_plot_4",
                         plot_type="ag_hist_1",
                         r=r)

    #####Tab 6 - Filtering -----
    mod_body_filter_server("body_filter_1",
                           validate_message="Please aggregate your data with abundances.",
                           validate_message2="Please ease your filtering requirements - you would filter out all rows with current parameter values.",
                           r=r)
    #####Tab 7 - Imputation -----
    mod_body_impute_server("body_impute_1",
                           validate_message="Please aggregate your data with abundances.",
                           r=r)

    #####Tab 8 - Analysis -----
    mod_body_analysis_server("body_analysis_1",
                           validate_message="Set all parameters and click on the 'Analyse' button",
                           r=r)
    mod_body_plot_server("body_plot_6",
                         plot_type="an_volcano_1",
                         r=r)

    #####Modals when switchin tabs ----
    observeEvent(input$tabs, {
      if (input$tabs %in% c("f","na") && "imputed"%in% r$data_char) {
        showModal(modalDialog(
          title = "Reminder",
          "You've marked your abundances data as imputed. You probably won't need to use this tab.",
          footer = NULL,
          easyClose = TRUE
        ))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$tabs, {
      if (input$tabs == "norm" && "normalized"%in% r$data_char) {
        showModal(modalDialog(
          title = "Reminder",
          "You've marked your abundances data as normalized. You probably won't need to use this tab.",
          footer = NULL,
          easyClose = TRUE
        ))
      }
    }, ignoreInit = TRUE)



  })
}

## To be copied in the UI
#

## To be copied in the server
#
