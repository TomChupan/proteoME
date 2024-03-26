# body_normalize UI Function

#' @title mod_body_normalize_ui and mod_body_normalize_server
#' @description A shiny module for creating boxplots (static and reactive)
#' in the app body to help decide whether/how to normalize data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title.
#'
#' @rdname mod_body_normalize
#' @export
#'
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs hide show
#' @importFrom preprocessCore normalize.quantiles
#' @importFrom MBQN mbqn
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_classic theme element_blank
mod_body_normalize_ui <- function(id,box_title){
  ns <- NS(id)
  tagList(
    box(width = 12,title=box_title,collapsible = F,status="primary",
        h4("Current version of the dataset:"),
        plotOutput(ns("plot_c"),height="300px"), #"static" boxplot for the current version of dataset
        br(),
        h4(textOutput(ns("plot_n_title"))),
        plotOutput(ns("plot_n"),height="300px"), #reactive boxplot depending on the selectInput with methods
        br()
    )
  )
}

# body_normalize Server Functions

#' @param validate_message Message to be displayed when there are no data to build a boxplot.
#' @param r A 'reactiveValues()' list containing (among other objects) all three
#' datasets and also a long-format version (combination of these datasets).
#' This is used in the boxplot rendering. We also use here the selected normalization
#' method from the mod_normalize module, the active tab and the information whether
#' the dataset was already normalized (r$normalizedTF).
#'
#'
#' @rdname mod_body_normalize
#' @export
mod_body_normalize_server <- function(id,validate_message,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ####Data preparation ----
    dTOplot_c=reactive({
      validate(need(not_null(r$d1) & not_null(r$d2) & not_null(r$d3),
                    validate_message))
      r$d_pivotlonger
    })

    dTOplot_n=reactive({
      validate(need(not_null(r$d1) & not_null(r$d2) & not_null(r$d3),
                    validate_message))
      req(r$normalizedTF==FALSE)
        normalized=r$d1
        normalized[,-1]=switch(r$norm_method,
                      "median"=sweep(normalized[,-1], 2L, #data, by rows/cols
                                     apply(normalized[,-1], 2L, median, na.rm = TRUE), #vector of column medians
                                     FUN = "-", check.margin = FALSE), #subtract it and don't check dimension
                      "quantile"=preprocessCore::normalize.quantiles(as.matrix(normalized[,-1])),
                      "mbqn_med"=MBQN::mbqn(as.matrix(normalized[,-1]),FUN = "median",
                                        method = "preprocessCore"),
                      "mbqn_mean"=MBQN::mbqn(as.matrix(normalized[,-1]),FUN = "mean",
                                            method = "preprocessCore")
        )
        d=r$d_pivotlonger
        d$abundances=c(t(as.matrix(normalized[,-1])))
        d
    }) #dTOplot_n close

    ####Boxplots ----
    ###Current dataset
    output$plot_c=renderPlot({
      ggplot2::ggplot(data=dTOplot_c(),ggplot2::aes(x=runID,y=abundances,
                                                    fill=treatment))+
        ggplot2::geom_boxplot(na.rm=T)+
        ggplot2::theme_classic()+
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }) #renderPlot close

    ####Normalized dataset
    output$plot_n_title=renderText({
      if(r$normalizedTF==FALSE){
        "After selected normalization method:"
      }else{"Your dataset has already been normalized. If you want to change a normalization method or return to an unnormalized dataset, please reset the data and import it again."}
    })


    output$plot_n=renderPlot({
      req(r$normalizedTF==FALSE)
      ggplot2::ggplot(data=dTOplot_n(),ggplot2::aes(x=runID,y=abundances,
                                                    fill=treatment))+
        ggplot2::geom_boxplot(na.rm=T)+
        ggplot2::theme_classic()+
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }) #renderPlot close

    ####After normalization ----
    observe({
      if(r$normalizedTF==TRUE){
        shinyjs::hide("plot_n")
      }else{
        shinyjs::show("plot_n")
      }
    })

  })
}

## To be copied in the UI
# mod_body_normalize_ui("body_normalize_1")

## To be copied in the server
# mod_body_normalize_server("body_normalize_1")
