# body_plot UI Function

#' @title mod_body_plot_ui and mod_body_plot_server
#' @description A shiny Module for creating plots in the app body.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title.
#'
#'
#' @rdname mod_body_plot
#'
#' @export
#'
#' @importFrom ggiraph renderGirafe girafeOutput

mod_body_plot_ui <- function(id,box_title="Your title."){
  ns <- NS(id)
  tagList(

    box(
      width=12,title = box_title,collapsible = T,
      collapsed = ifelse(box_title=="Boxplot of abundances by runs",F,T),
      status="primary", #just for CSS
      girafeOutput(ns("plot"))
    ) #box close


  ) #tagList close
} #mod_body_plot_ui close

# body_plot Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) plots built
#' in mod_plot. These are used here in a renderGirafe() function.
#' @param plot_type Character to specify the plot. It should be made up of 3 parts
#' separated by an underscore (_): \strong{tab ID}, \strong{plot type} and
#' \strong{order of the plot of the same type on the same tab}.
#' For example: eda_box_1 stands for the first box plot on the "Exploratory data analysis"
#' tab.
#'
#' @rdname mod_body_plot
#' @export
mod_body_plot_server <- function(id,
                                 plot_type,
                                 r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$plot=renderGirafe({
      switch(plot_type,
             "eda_box_1"=r$eda_box_1,
             "eda_hist_1"=r$eda_hist_1,
             NULL)
    }) #renderPlot close


  }) #moduleServer close
}

## To be copied in the UI
#

## To be copied in the server
#
