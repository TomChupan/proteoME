# body_impute UI Function

#' @title mod_body_impute_ui and mod_body_impute_server
#' @description A shiny Module for creating content on the imputation tab
#' in the app body.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title.
#'
#'
#' @noRd
#'
#' @importFrom shinyjs show hide hidden
#' @importFrom ComplexHeatmap Heatmap Legend draw
#' @importFrom grid gpar

mod_body_impute_ui <- function(id,box_title="Your title."){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".custom-h4 {margin-left: 15px;}")
    ),
    box(width = 12,title=box_title,collapsible = T,status="primary",
        fluidRow(
          column(3,
                 numericInput(ns("clusters"),"Number of protein clusters to split:",
                              value = 5,min=1,max=20,step = 1)
                 )
        ), #fluidRow close
        fluidRow(
          h4("Current version of the dataset:",class="custom-h4"),
          column(12,plotOutput(ns("heat_c"),height="400px")), #"static" heatmap for the current version of dataset
        ), #fluidRow close
        fluidRow(
          column(5,shinyjs::hidden(downloadButton(ns("downheat_c"),
                                  "Download heatmap (current version) (.png)"))),
          br()
        ),#fluidRow close
        fluidRow(
          h4(textOutput(ns("heat_i_title")),class="custom-h4"),
          column(12,plotOutput(ns("heat_i"),height="400px")), #reactive heatmap depending on the selectInput with methods
        ), #fluidRow close
        fluidRow(
          column(5,shinyjs::hidden(downloadButton(ns("downheat_i"),
                                  "Download heatmap ('see the effect') (.png)"))),
          br()
        ) #fluidRow close
    ) #box close



  ) #tagList close
} #mod_body_impute_ui close

# body_impute Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) ___.
#' These are used here in a ___ function.
#'
#' @noRd
mod_body_impute_server <- function(id,validate_message,
                                   r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #Interactive imputed data ("preview") ----
    interactive_impute=reactive({
      req(not_null(r$d4) & r$imputedTF==F)
      d=r$d4
      if(r$im_method=="sampmin"){
        d=d %>%
          mutate_if(is.numeric, function(x) ifelse(is.na(x), min(x, na.rm = T), x))
      }else if(r$im_method=="knn"){
        req(not_null(r$k))
        knn_imputed=impute::impute.knn(as.matrix(d[,-1]),k = r$k,
                                       rowmax = 1, colmax = 1,maxp=nrow(r$d1))
        d[,-1]=knn_imputed$data
      }else if(r$im_method=="rf"){
        if(min(apply(d[,-1],1,function(x){sum(!is.na(x))}))<=5){
          shinyalert(title = "You do not have enough data to use this method.",
                     text="Please choose another method or be stricter when filtering your data.",
                     showConfirmButton = TRUE, type = "warning")
          d=NULL
        }else{
          req(not_null(r$setseed) && not_null(r$ntree))
          set.seed(r$setseed)
          rf_imputed=missForest::missForest(t(d[,-1]), ntree = r$ntree)
          d[,-1]=t(rf_imputed$ximp)
          r$d4_rf_imp=d #to avoid doing this again
        }
      }

      d
    })

    ###Heatmaps -----
    ######Data
    dTOplot_c=reactive({
      d=r$d4[,-1]
    }) #dTOplot_c close

    dTOplot_i=reactive({
      req(not_null(interactive_impute()))
      req(r$imputedTF==FALSE)
      d=interactive_impute()[,-1]
    }) #dTOplot_i close

    #####Plots
    ##Current dataset
    heat_c=reactive({
      req(not_null(dTOplot_c()))
      spl=merge(data.frame(sampleID=names(dTOplot_c())),r$d3[,1:2],by="sampleID",
                sort = F)
      clusterrowsTF=(sum(is.na(ChemoSpec::rowDist(as.matrix(dTOplot_c()),
                                                  method="euclidean")))==0)
      ht=ComplexHeatmap::Heatmap(as.matrix(dTOplot_c()),
                                 cluster_rows = clusterrowsTF,
                                 cluster_columns = F,
                                 name = "Protein abundances \nbefore imputation",
                                 column_split = spl$treatment,
                                 row_split = if(clusterrowsTF){input$clusters}else{NULL},
                                 column_gap = unit(0.5,"cm"),
                                 row_gap = unit(0.35,"cm"))
      if(sum(is.na(dTOplot_c()))>0){
      lgd2=ComplexHeatmap::Legend(at=1,labels="NA",title = "",
                                  legend_gp = gpar(fill = "grey"))
      }
      suppressMessages(
        if(sum(is.na(dTOplot_c()))>0){
        draw(ht, heatmap_legend_list = list(lgd2))
        }else{
        draw(ht)
        }
      )
    })
    output$heat_c=renderPlot({
      validate(need(not_null(r$d4),validate_message))
      req(not_null(heat_c()))
      heat_c()
    }) #renderPlot close

    ##Imputed dataset
    output$heat_i_title=renderText({
      if(r$imputedTF==FALSE){
        "After imputation with the current parameter values:"
      }else{""}
    })

    heat_i=reactive({
      req(not_null(interactive_impute()))
      spl=merge(data.frame(sampleID=names(dTOplot_i())),r$d3[,1:2],by="sampleID",
                sort = F)
      suppressMessages(
      ComplexHeatmap::Heatmap(as.matrix(dTOplot_i()),cluster_rows = T,
                              cluster_columns = F,
                              name = "Protein abundances \nafter imputation",
                              column_split = spl$treatment,row_split = input$clusters,
                              column_gap = unit(0.5,"cm"),
                              row_gap = unit(0.35,"cm"))
      )
    })

    output$heat_i=renderPlot({
      validate(need(not_null(r$d4),validate_message))
      req(not_null(heat_i()))
        heat_i()
    }) #renderPlot close



    #### After imputation ----
    observe({
      if(r$imputedTF==TRUE){
        shinyjs::hide("heat_i")
        shinyjs::hide("downheat_i")
      }else{
        shinyjs::show("heat_i")
        shinyjs::show("downheat_i")
      }
    })

    ##### Downloads ----
    observe({
      if(not_null(interactive_impute())){
        shinyjs::show("downheat_c")
        shinyjs::show("downheat_i")
      }
    })

    output$downheat_c = downloadHandler(

      filename = function() {
        "heatmap1.png"
      },
      content = function(file) {
        png(file,width=1000,height = 563)
        plot(heat_c())
        dev.off()
      }
    )

    output$downheat_i = downloadHandler(

      filename = function() {
        "heatmap2.png"
      },
      content = function(file) {
        png(file,width=1000,height = 563)
        plot(heat_i())
        dev.off()
      }
    )


  }) #moduleServer close
}

## To be copied in the UI
#

## To be copied in the server
#
