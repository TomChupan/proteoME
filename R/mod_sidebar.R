# sidebar UI Function

#' @title mod_sidebar_ui and mod_sidebar_server
#' @description A shiny Module containing sidebar content
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_sidebar
#'
#' @export
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        HTML(".custom-h4 {margin-left: 10px;}"
        )
      )
    ),
    conditionalPanel(condition = "output.tabset_value =='im'",ns=ns,
    mod_upload_ui(ns("upload_1"),
                  menuItem_label="Abundances",
                  fileInput_label="Upload your dataset with abundances (.csv/.txt)"),
    mod_upload_ui(ns("upload_2"),
                  menuItem_label="Annotations (run)",
                  fileInput_label="Upload your run annotation dataset (.csv/.txt)"),
    mod_upload_ui(ns("upload_3"),
                  menuItem_label="Annotations (sample)",
                  fileInput_label="Upload your sample annotation dataset (.csv/.txt)")

    ), #conditionalPanel im close
    conditionalPanel(condition = "output.tabset_value =='eda'",ns=ns,
                     mod_plot_ui(ns("eda_1"),
                                 plot_type="eda_box_1",
                                 menuItem_label = "Boxplot of abundances by runs"),
                     mod_plot_ui(ns("eda_2"),
                                 plot_type="eda_hist_1",
                                 menuItem_label = "Histogram of detected
                                 proteins in each run")
                     ), #conditionalPanel eda close

    conditionalPanel(condition = "output.tabset_value =='trans'",ns=ns,
                     mod_transform_ui(ns("transform_1"))
    ), #conditionalPanel transform close
    conditionalPanel(condition = "output.tabset_value =='norm'",ns=ns,
                     mod_normalize_ui(ns("normalize_1"))
    ), #conditionalPanel normalize close
    conditionalPanel(condition = "output.tabset_value =='ag'",ns=ns,
                     h4("Before the aggregation:",class = "custom-h4"),
                     mod_plot_ui(ns("ag_3"),
                                 plot_type="ag_bar_1",
                                 menuItem_label = "Barplot of number of detections"),
                     mod_aggregate_ui(ns("aggregate_1")),
                     mod_plot_ui(ns("ag_1"),
                                 plot_type="ag_box_1",
                                 menuItem_label = "Boxplot of abundances by samples"
                                 ),
                     mod_plot_ui(ns("ag_2"),
                                 plot_type="ag_hist_1",
                                 menuItem_label = "Histogram of detected
                                 proteins in each sample")
    ), #conditionalPanel aggregate close
    conditionalPanel(condition = "output.tabset_value =='f'",ns=ns,
                     mod_filter_ui(ns("filter_1"))
    ) #conditionalPanel filtering close

  ) #tagList close
}

# sidebar Server Functions

#' @param r A "storage" for the variables used throughout the app
#'
#' @rdname mod_sidebar
#' @export
mod_sidebar_server <- function(id,r){
  moduleServer(
    id,
    function(input, output, session){
    ns <- session$ns


    output$tabset_value=renderText(r$tabset_value)
    outputOptions(output, 'tabset_value', suspendWhenHidden = FALSE)

    mod_upload_server("upload_1",data_type=1,r=r)
    mod_upload_server("upload_2",data_type=2,r=r)
    mod_upload_server("upload_3",data_type=3,r=r)
    mod_plot_server("eda_1",plot_type="eda_box_1",r=r)
    mod_plot_server("eda_2",plot_type="eda_hist_1",r=r)
    mod_transform_server("transform_1",r=r)
    mod_normalize_server("normalize_1",r=r)
    mod_aggregate_server("aggregate_1",r=r)
    mod_plot_server("ag_1",plot_type="ag_box_1",r=r)
    mod_plot_server("ag_2",plot_type="ag_hist_1",r=r)
    mod_plot_server("ag_3",plot_type="ag_bar_1",r=r)
    mod_filter_server("filter_1",r=r)

  })
}

## To be copied in the UI
#

## To be copied in the server
#
