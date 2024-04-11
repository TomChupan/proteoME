# body_filter UI Function

#' @title mod_body_filter_ui and mod_body_filter_server
#' @description A shiny Module for creating content on the Filtering tab
#' in the app body.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param box_title Character to be used as a box title.
#'
#'
#' @rdname mod_body_filter
#'
#' @export
#'
#' @importFrom viridis scale_fill_viridis
#' @importFrom shinyscreenshot screenshot
#' @importFrom shinyjs show hide hidden
#' @importFrom naniar gg_miss_fct gg_miss_case
#' @importFrom ggplot2 theme theme_classic element_blank element_line element_text xlab ylab

mod_body_filter_ui <- function(id,box_title="Your title."){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".custom-h4 {margin-left: 15px;}")
    ),
    box(width = 12,title=box_title,collapsible = T,status="primary",
        h4(textOutput(ns("valueboxes_h4"))),
        fluidRow(
          valueBoxOutput(ns("n_proteins"),width=4),
          valueBoxOutput(ns("toremove_proteins"),width=4),
          valueBoxOutput(ns("newn_proteins"),width=4)
        ), #fluidRow close
        fluidRow(id=ns("r1"),
          h4("Current version of the dataset:",class="custom-h4"),
          column(6,plotOutput(ns("naplot1_c"),height="300px")),
          column(6,plotOutput(ns("naplot2_c"),height="300px")),#"static" NA plots for the current version of dataset
          br()
        ), #fluidRow close
        shinyjs::hidden(
          actionButton(ns("download_c"),"Download missingness plots (current version) (.png)")
        ),
        fluidRow(id=ns("r2"),
          h4(textOutput(ns("naplot_f_title")),class="custom-h4"),
          column(6,plotOutput(ns("naplot1_f"),height="300px")),
          column(6,plotOutput(ns("naplot2_f"),height="300px")),#reactive NA plots depending on the selectInput with methods
          br()
        ), #fluidRow close
        shinyjs::hidden(
          actionButton(ns("download_f"),"Download missingness plots ('see the effect') (.png)")
        )
    ), #box close



  ) #tagList close
} #mod_body_filter_ui close

# body_filter Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) ___.
#' These are used here in a ___ function.
#'
#' @rdname mod_body_filter
#' @export
mod_body_filter_server <- function(id,validate_message,
                                   validate_message2,
                                   r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #Interactive filtered data ("preview") ----
    interactive_filter=reactive({
      req(not_null(r$d4) & r$filteredTF==F)
      d=proteoFI(r$d4,r$d3,r$fil_n,r$fil_method)
      d
    })

    #Value boxes----
    output$valueboxes_h4=renderText({
      if(r$filteredTF==FALSE){
        "See the filtering effect with the current parameter values:"
        }else{""}
    })


    output$n_proteins=renderValueBox({
        if(is.null(r$d4)){
          valueBox("Not aggregated","Total number of proteins",icon=icon("list"))
        }else{
          valueBox(nrow(r$d4),"Current number of proteins (rows)",icon=icon("list"),color="green")
        }
    }) #renderValueBox close

    output$toremove_proteins=renderValueBox({
      req(r$filteredTF==F)
      if(is.null(r$d4)){
        valueBox("Not aggregated","Proteins to be filtered out",icon=icon("xmark"))
      }else{
        valueBox(paste0(nrow(r$d4)-nrow(interactive_filter())," (",
                        round(100*(nrow(r$d4)-nrow(interactive_filter()))/nrow(r$d4),1),
                        " %)"),
                 "Proteins to be filtered out",icon=icon("xmark"),color="red")
      }
    }) #renderValueBox close

    output$newn_proteins=renderValueBox({
      req(r$filteredTF==F)
      if(is.null(r$d4)){
        valueBox("Not aggregated","Number of proteins after filtering",icon=icon("list"))
      }else{
        valueBox(paste0(nrow(interactive_filter())," (",
                        round(100*nrow(interactive_filter())/nrow(r$d4),1)," %)"),
                 "Number of proteins after filtering",icon=icon("list"),
                 color="blue")
      }
    }) #renderValueBox close

    #Hide value boxes when filtered:
    observe({
      if(r$filteredTF==T){
        shinyjs::hide("toremove_proteins")
        shinyjs::hide("newn_proteins")
      }else{
        shinyjs::show("toremove_proteins")
        shinyjs::show("newn_proteins")
      }
    })

    ###NA plots -----
    ######Data
    dTOplot_c=reactive({
      req(not_null(r$d4))
      dT=as.data.frame(t(r$d4[,-1]))
      colnames(dT)=r$d4$Accession
      dT$sampleID=rownames(dT)
      dT=merge(dT,r$d3[,1:2],by = "sampleID")
      dT$sampleID=NULL
      dT
    }) #dTOplot_c close

    dTOplot_f=reactive({
      req(not_null(interactive_filter()))
      req(r$filteredTF==FALSE)
      dT=as.data.frame(t(interactive_filter()[,-1]))
      colnames(dT)=interactive_filter()$Accession
      dT$sampleID=rownames(dT)
      dT=merge(dT,r$d3[,1:2],by = "sampleID")
      dT$sampleID=NULL
      dT
    }) #dTOplot_f close

    #####Plots
    ##Current dataset
    output$naplot1_c=renderPlot({
      validate(need(not_null(r$d4),validate_message))
      suppressMessages(
      dTOplot_c() |> naniar::gg_miss_fct(fct = treatment) +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle=0,hjust = 1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        ylab("Proteins")+xlab("Treatment group")+
        viridis::scale_fill_viridis(name = "% of NA's")
      )
    }) #renderPlot close

    output$naplot2_c=renderPlot({
      validate(need(not_null(r$d4),validate_message))
      dTOplot_c() |> gg_miss_case(order_cases = TRUE,
                         show_pct = TRUE,facet = treatment)+
        theme_classic()+
        theme(
          panel.grid.major = element_line(colour="grey")
        )
    }) #renderPlot close

    ##Filtered dataset
    output$naplot_f_title=renderText({
      if(r$filteredTF==FALSE){
        "After filtering with the current parameter values:"
      }else{""}
    })

    output$naplot1_f=renderPlot({
      validate(need(not_null(r$d4),validate_message))
      if(not_null(interactive_filter())){
        validate(need(nrow(interactive_filter())!=0,validate_message2))
      }
      suppressMessages(
        dTOplot_f() |> naniar::gg_miss_fct(fct = treatment) +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(angle=0,hjust = 1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        ylab("Proteins")+xlab("Treatment group")+
        viridis::scale_fill_viridis(name = "% of NA's")
      )
    }) #renderPlot close

    output$naplot2_f=renderPlot({
      validate(need(not_null(r$d4),validate_message))
      if(not_null(interactive_filter())){
        validate(need(nrow(interactive_filter())!=0,validate_message2))
      }
      dTOplot_f() |> gg_miss_case(order_cases = TRUE,
                                show_pct = TRUE,facet = treatment)+
        theme_classic()+
        theme(
          panel.grid.major = element_line(colour="grey")
        )
    }) #renderPlot close


    ####After filtering ----
    observe({
      if(r$filteredTF==TRUE){
        shinyjs::hide("naplot1_f")
        shinyjs::hide("naplot2_f")
      }else{
        shinyjs::show("naplot1_f")
        shinyjs::show("naplot2_f")
      }
    })


    ##### Screenshots ----
    observe({
      if(r$aggregatedTF==TRUE){
        shinyjs::show("download_c")
      }else{
        shinyjs::hide("download_c")
      }
    })

    observe({
      if(r$aggregatedTF==TRUE & r$filteredTF==FALSE){
        shinyjs::show("download_f")
      }else{
        shinyjs::hide("download_f")
      }
    })


    observeEvent(input$download_c,{
      shinyscreenshot::screenshot(id="r1",filename = "naplots_c",scale=1.5)
    })

    observeEvent(input$download_f,{
      shinyscreenshot::screenshot(id="r2",filename = "naplots_f",scale=1.5)
    })







  }) #moduleServer close
}

## To be copied in the UI
#

## To be copied in the server
#
