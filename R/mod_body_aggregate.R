# body_aggregate UI Function

#' @title mod_body_aggregate_ui and mod_body_aggregate_server
#' @description A shiny Module for creating content on the Aggregation tab
#' in the app body.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title.
#'
#'
#' @noRd
#'
#' @importFrom ComplexHeatmap rowAnnotation Heatmap ht_opt HeatmapAnnotation
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom viridis turbo
#' @importFrom grid unit
#' @importFrom shinyscreenshot screenshot
#' @importFrom shinyjs show hide hidden

mod_body_aggregate_ui <- function(id,box_title="Your title."){
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      actionButton(ns("download"),"Download box content (.png)")
    ),
    box(id=ns("heatbox"),
      width=12,title = box_title,collapsible = T,collapsed = T,
      status="primary", #just for CSS
      column(9,plotOutput(ns("plot"))),
      column(3,"Median of samples with a given \nnumber of protein detections \n(overall and by treatment group)",
             tableOutput(ns("table")))
    ) #box close

  ) #tagList close
} #mod_body_aggregate_ui close

# body_aggregate Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) ___.
#' These are used here in a ___ function.
#'
#' @noRd
mod_body_aggregate_server <- function(id,r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ###Data preparation: -----
    detected=eventReactive(r$render_heatmap,{
      req(not_null(r$d_pivotlonger))
      if(is.null(r$d_detected)){
      detected=aggregate(abundances ~ Accession + sampleID,
                         data = r$d_pivotlonger,
                         FUN = function(x){
                           length(x)-sum(is.na(x))-sum(x==0,na.rm = T)
                         },
                         na.action = na.pass)
      detected=detected %>%
        tidyr::pivot_wider(names_from = "sampleID",values_from = "abundances",
                           names_sort = F)
      r$d_detected=detected
      }

      r$d_detected
    })

    ###Heatmap annotations ----
    row_ha=eventReactive(r$render_heatmap,{
      req(not_null(detected()))
      detmax=max(detected()[,-1])
      annotations=list()
      for (i in 0:detmax) {
        annotations[[paste0("detected", i)]]=apply(detected()[, -1], 1,
                                                   function(x) sum(x == i))
      }
      row_ha=ComplexHeatmap::rowAnnotation(df=as.data.frame(annotations))
      row_ha
    })

    ####Heatmap ----
    ht_opt$message = FALSE
    heatmap=eventReactive(r$render_heatmap,{
      req(not_null(detected()) & not_null(row_ha()))
      detmax=max(detected()[,-1])
      colors=structure(c("white",viridis::turbo(detmax,direction = -1)),
                      names=as.character(c(0:detmax)))
      #Splitting by group:
      spl=merge(data.frame(sampleID=names(detected()[,-1])),
                r$d3[,1:2],by="sampleID",sort = F)
      Heatmap(as.matrix(detected()[,-1]),cluster_rows = F,cluster_columns = F,
              col = colors,column_split=spl$treatment,column_gap = grid::unit(0.5,"cm"),
              name = "Number of detections \nfor a protein within \none sample",
              right_annotation = row_ha())
    })

    #To be able to set it back to NULL after the dataset reset
    observe({
      r$ag_heatmap_1=heatmap()
    })

    output$plot=renderPlot({
      r$ag_heatmap_1
    }) #renderPlot close

    ##### Table with medians ----
    table=eventReactive(r$render_heatmap,{
      req(not_null(r$d_pivotlonger))
      detectionMedians(detected(),r$d3)
    })

    output$table=renderTable(
      expr = {
        validate(need(not_null(r$d_pivotlonger),
                      "Please upload all files and render the heatmap."))
        validate(need(not_null(r$ag_heatmap_1),
                      "Please render the heatmap."))
        table()
      },
      digits = 0
    ) #renderTable close

    ##### Screenshot ----
    observe({
      if(not_null(r$ag_heatmap_1)){
        shinyjs::show("download")
      }else{
        shinyjs::hide("download")
      }
    })

    observeEvent(input$download,{
      shinyscreenshot::screenshot(id="heatbox",filename = "heatmap",scale=2)
    })






  }) #moduleServer close
}

## To be copied in the UI
#

## To be copied in the server
#
