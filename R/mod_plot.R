# plot UI Function

#' @title mod_plot_ui and mod_plot_server
#' @description A shiny Module containing sidebar content for making plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param menuItem_label Character to name the menu item. It should be the name
#' and a short description of the plot (e.g. 'Boxplot of abundances by runs').
#' @param plot_type Character to specify the plot. It should be made up of 3 parts
#' separated by an underscore (_):
#' \itemize{
#'  \item Tab ID
#'  \item Plot type
#'  \item Order of the plot of the same type on the same tab
#' }
#' For example: eda_box_1 stands for the first box plot on the "Exploratory data analysis"
#' tab.
#'
#' @rdname mod_plot
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot after_stat theme_classic theme_minimal aes ggsave ylab xlab theme element_text ylim scale_y_continuous element_line element_blank position_dodge2
#' @importFrom ggiraph geom_boxplot_interactive girafe opts_hover opts_zoom opts_toolbar geom_histogram_interactive opts_selection geom_col_interactive
#' @importFrom htmlwidgets saveWidget
#' @importFrom plotly as_widget
#' @importFrom shinyWidgets dropdownButton toggleDropdownButton
#' @importFrom colourpicker colourInput
#' @importFrom shinyjs hide
#' @importFrom tidyr pivot_wider
mod_plot_ui <- function(id,plot_type,menuItem_label){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        HTML(paste0("#",ns("help"),"{display: inline;
                     margin: 0px;
                     }"
        ))
      ),
      tags$style(".skin-blue .sidebar .shiny-download-link { color: #444;
                 padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px;}"),
      tags$style(".drop {color: black;}")
    ),
    sidebarMenu(
      menuItem(text = menuItem_label,
               startExpanded = switch(plot_type,
                                      "eda_box_1"=T,
                                      "ag_bar_1"=T,
                                      F),
               br(),
               HTML("What does this plot show",as.character(
                 actionLink(inputId = ns("help"),
                            label = "",
                            icon = icon("circle-question")))
               ),
               selectInput(ns("select_1"),
                           label=switch(plot_type,
                                        "eda_box_1"="Select the form of abundances:",
                                        "eda_hist_1"="Select the grouping option:",
                                        "ag_hist_1"="Select the grouping option:",
                                        "ag_bar_1"="Select the grouping option",
                                        NULL),
                           choices = switch(plot_type,
                                            "eda_box_1"=c("just the values"="original",
                                                          "log-transformed","log2(x+1)",
                                                          "square root"),
                                            "eda_hist_1"=c("without grouping","with grouping"),
                                            "ag_hist_1"=c("without grouping","with grouping"),
                                            "ag_bar_1"=c("without grouping","with grouping"),
                                            NULL),multiple = F
               ), #selectInput close

      ####Dropdown button ----
               dropdownButton(
                 circle=F,status="primary",size="sm",inputId=ns("dropmenu"),
                 label="Edit plot appearance",
                 tags$div(class="drop",
                 h4("General plot appearence:"),
                 fluidRow(
                   column(6,numericInput(ns("height"),"Height (inches)",min=1,
                                         max=30,value=5,step=1),
                          numericInput(ns("axis_text_size"),"Axis text size",
                                         min=1,max=50,value=12,step=1),
                          numericInput(ns("legend_text_size"),"Legend text size",
                                       min=1,max=50,value=12,step=1),
                          textInput(ns("xlab"),label = "X-axis label",
                                    value = switch(plot_type,
                                                   "eda_box_1"="runID",
                                                   "eda_hist_1"="number of detected proteins in each run",
                                                   "ag_box_1"="sampleID",
                                                   "ag_hist_1"="number of detected proteins in each sample",
                                                   "ag_bar_1"="number of detections of one protein within one sample"
                                                   ))
                          ),
                   column(6,numericInput(ns("width"),"Width (inches)",min=1,
                                         max=30,value=9,step=1),
                          numericInput(ns("axis_title_size"),"Axis title size",
                                         min=1,max=50,value=16,step=1),
                          numericInput(ns("legend_title_size"),"Legend title size",
                                       min=1,max=50,value=16,step=1),
                          textInput(ns("ylab"),label = "Y-axis label",
                                    value = switch(plot_type,
                                                   "eda_box_1"="abundances",
                                                   "eda_hist_1"="count",
                                                   "ag_box_1"="abundances",
                                                   "ag_hist_1"="count",
                                                   "ag_bar_1"="count"
                                    )))
                 ), #fludiRow close
                 h4("Features specific to the plot type:"),
                 if(grepl("box",plot_type)){ #boxplots
                   fluidRow(
                   checkboxInput(ns("yzero"),"Start y-axis from zero",value=F),
                   checkboxInput(ns("notch"),"Notch (approx 95% CI for median)",
                                 value=F),
                   checkboxInput(ns("xaxis"),"Remove x-axis text",value = F),
                   column(6,colourInput(ns("out_color"),"Outlier color",
                                        palette = "limited",value = "#000000")
                          ),
                   column(6,numericInput(ns("out_size"),"Outlier size",min=1,
                                         max=50,value=2,step=1))
                   )#fluidRow close
                 },
                 if(grepl("hist",plot_type)){ #histograms
                   fluidRow(
                     column(6,numericInput(ns("binwidth"),"Bin width",min=1,
                                           max=10000,value=30,step=1),
                            colourInput(ns("binfill"),"Bin color (fill)",
                                        palette = "square",value = "lightblue")),
                     column(6,numericInput(ns("alpha"),
                                           "Degree of transparency (alpha)",min=0,
                                           max=1,value=0.5,step=0.01),
                           colourInput(ns("bincol"),"Bin color (border)",
                                        palette = "limited",value = "#000000"))
                   ) #fluidRow close
                 },
                 if(grepl("bar",plot_type)){ #barplots
                   fluidRow(
                     column(6,colourInput(ns("barfill"),"Bar color (fill)",
                                          palette = "square",value = "lightblue")),
                     column(6,colourInput(ns("barcol"),"Bar color (border)",
                                          palette = "limited",value = "#000000"))
                   ) #fluidRow close
                 },
                 actionButton(ns("apply"),"Apply changes")
                   ) #div close

                 ), #dropdownButton close
               actionButton(ns("render"),label = "Render plot",icon = icon("play")),
               downloadButton(ns("down"),label = "Download plot (.png)"),br(),
               downloadButton(ns("downI"),label = "Download interactive plot (.html)")

      ) #menuItem close
    ) #sidebarMenu close

  ) #tagList close
} #mod_plot_ui close

# plot Server Functions

#' @param r A "storage" for the variables used throughout the app
#' @param plot_type Character to specify the plot. It should be made up of 3 parts
#' separated by an underscore (_):
#' \itemize{
#'  \item Tab ID
#'  \item Plot type
#'  \item Order of the plot of the same type on the same tab
#' }
#' For example: eda_box_1 stands for the first box plot on the "Exploratory data analysis"
#' tab.
#'
#' @rdname mod_plot
#' @export
#'
mod_plot_server <- function(id,plot_type,r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ####Data preparation----

    dTOplot=eventReactive(input$render | input$apply,{
      req(not_null(r$d1) & not_null(r$d2) & not_null(r$d3))
      switch(plot_type,
             "eda_box_1"={
               if(all(r$d1[,-1]>0,na.rm = T) | input$select_1!="log-transformed"){
               d=r$d_pivotlonger
               d$abundances=switch(input$select_1,
                                   "original"=d$abundances,
                                   "log-transformed"=log2(d$abundances),
                                   "log2(x+1)"=log2(d$abundances+1),
                                   "square root"=sqrt(d$abundances)
               )
               d
               }else{
                 return(NULL)
               }
             }, #eda_box_1 close
             "eda_hist_1"={
               d=data.frame(detected=as.numeric(colSums(not_na(r$d1[-1]) &
                                                          r$d1[-1]!=0)),
                            runID=names(colSums(not_na(r$d1[-1]) &
                                                  r$d1[-1]!=0))
               )

               d=merge(d,r$d2[,c(1:2)],by="runID")
               d=merge(d,r$d3[,c(1:2)],by="sampleID")
               d

             }, #eda_hist_1 close
             "ag_box_1"={
               req(not_null(r$d4))
               d=r$dAG_pivotlonger
               d
             }, #ag_box_1 close
             "ag_hist_1"={
               req(not_null(r$d4))
               d=data.frame(detected=as.numeric(colSums(not_na(r$d4[-1]) &
                                                          r$d4[-1]!=0)),
                            sampleID=names(colSums(not_na(r$d4[-1]) &
                                                  r$d4[-1]!=0))
               )
               d=merge(d,r$d3[,c(1:2)],by="sampleID")
               d
             }, #eda_hist_1 close
             "ag_bar_1"={
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
               detmax=max(r$d_detected[,-1])
               if(input$select_1=="without grouping"){ #without grouping
                 d=data.frame(detected_n=0:detmax)
                 d$count=sapply(0:detmax,function(value){
                   sum(apply(r$d_detected[,-1],1,function(x) sum(x==value)))
                 })
                 d=d %>%
                   mutate(relative_count = count/sum(count)*100)
                 d$tooltip=paste0(d$detected_n," detected: ",d$count," (",
                                       round(d$relative_count,2), "%)")
               }else{ #with grouping
                 groups=levels(as.factor(r$d3[,"treatment"]))
                 n_levels=length(groups)
                 d=data.frame(detected_n=rep(0:detmax,each=n_levels),
                                    treatment=rep(groups,detmax+1),
                                    count=rep(NA,n_levels*(detmax+1)))
                 for(j in 1:n_levels){
                   id=r$d3[r$d3[,"treatment"]==groups[j],"sampleID"]
                   d[d$treatment==groups[j],"count"]=
                     sapply(0:detmax, function(value) {
                       sum(apply(r$d_detected[, id], 1, function(x) sum(x == value)))
                     })
                 }
                 d=d %>%
                   group_by(treatment) %>%
                   mutate(relative_count = count/sum(count)*100)
                 d$tooltip=paste0("Treatment group ",d$treatment,": ",
                                        d$detected_n," detected: ",d$count," (",
                                        round(d$relative_count,2), "%)")
               }
               d
             }, #ag_bar_1 close
             NULL)
    }) #dTOplot close

    ####Plot ----

    plot=eventReactive(input$render | input$apply,{
      req(not_null(dTOplot()))
      switch(plot_type,
             "eda_box_1"={
               eda_box_1=ggplot(data=dTOplot(),aes(x=runID,y=abundances,
                  fill=treatment,tooltip=after_stat({
                                paste0(
                                  "runID: ", levels(as.factor(dTOplot()$runID)),
                                  "\ndetected proteins: ",
                                  as.numeric(table(dTOplot()$runID[not_na(dTOplot()$abundances)])),
                                  "\nQ3: ", round(as.data.frame(dTOplot() %>%
                                                                  group_by(runID) %>%
                                                                  dplyr::summarize(q75=quantile(abundances, probs = 0.75,na.rm=T)))$q75,2),
                                  "\nmedian: ", round(as.data.frame(dTOplot() %>%
                                                                      group_by(runID) %>%
                                                                      dplyr::summarize(m=median(abundances,na.rm=T)))$m,2),
                                  "\nQ1: ", round(as.data.frame(dTOplot() %>%
                                                                  group_by(runID) %>%
                                                                  dplyr::summarize(q25=quantile(abundances, probs = 0.25,na.rm=T)))$q25,2),
                                  "\nnot detected (of all detected proteins in the data set): ",
                                  as.numeric(table(dTOplot()$runID[is.na(dTOplot()$abundances) | dTOplot()$abundances==0])),
                                  "\n% of detected proteins: ",
                                  round(100*as.numeric(table(dTOplot()$runID[not_na(dTOplot()$abundances)]))/length(unique(dTOplot()$Accession)),2),
                                  " %"
                                )}),data_id=runID))+
                 geom_boxplot_interactive(varwidth=T,outlier.colour=input$out_color,
                                          outlier.size=input$out_size,
                                          notch=input$notch,na.rm=T)+
                 ylab(input$ylab)+xlab(input$xlab)+
                 theme_classic()+
                 theme(
                   axis.title = element_text(size=input$axis_title_size),
                   axis.text = element_text(size=input$axis_text_size),
                   legend.title = element_text(size=input$legend_title_size),
                   legend.text = element_text(size=input$legend_text_size)
                 )

               if(input$yzero){
                 eda_box_1=eda_box_1+ylim(0,NA)
               }

               if(input$xaxis){
                 eda_box_1=eda_box_1+
                   theme(axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())
               }

               x=ggiraph::girafe(ggobj = eda_box_1,
                                 height_svg = input$height,width_svg = input$width,
                                 options = list(
                                   opts_hover(css = ""),
                                   opts_zoom(max=4) ,
                                   opts_selection(type = "none",css = NULL),
                                   opts_toolbar(saveaspng=FALSE)
                                 )
               )
               x
             }, #eda_box_1 close
             "eda_hist_1"={
               if(input$select_1=="without grouping"){ #without grouping
                 eda_hist_1=ggplot(data=dTOplot(),aes(x=detected))+
                   geom_histogram_interactive(aes(tooltip=paste0("Bin range: [",
                                                                 round(after_stat(xmin),2),
                                                                 ",",round(after_stat(xmax),2),
                                                                 "] \ncount: ",after_stat(count))),
                                              fill=input$binfill,color=input$bincol,
                                              alpha=input$alpha,
                                              binwidth=input$binwidth)+
                   theme_classic()
               }else{ #with grouping (by treatment)
                 eda_hist_1=ggplot(data=dTOplot(),aes(x=detected,fill=treatment))+
                   geom_histogram_interactive(aes(tooltip=paste0("Bin range: [",
                                                                 round(after_stat(xmin),2),
                                                                 ",",round(after_stat(xmax),2),"]")),
                                              alpha=input$alpha,color=input$bincol,
                                              position="identity",
                                              binwidth=input$binwidth)+
                   theme_classic()+
                   theme(
                     panel.grid.major.y = element_line(colour="darkgrey"),
                     panel.grid.minor.y = element_line(colour="grey")
                   ) #no count tooltips so let's have major and minor grid
               }
               eda_hist_1=eda_hist_1+
                 xlab(input$xlab)+
                 theme(
                   axis.title = element_text(size=input$axis_title_size),
                   axis.text = element_text(size=input$axis_text_size),
                   legend.title = element_text(size=input$legend_title_size),
                   legend.text = element_text(size=input$legend_text_size)
                 )+ylab(input$ylab)

               x=ggiraph::girafe(ggobj = eda_hist_1,
                                 height_svg = input$height,width_svg = input$width,
                                 options = list(
                                   opts_hover(css = ""),
                                   opts_zoom(max=4),
                                   opts_selection(type = "none",css = NULL),
                                   opts_toolbar(saveaspng=FALSE)
                                 )
               )
               x
             }, #eda_hist_1 close
             "ag_box_1"={
               ag_box_1=ggplot(data=dTOplot(),aes(x=sampleID,y=abundances,
                                                   fill=treatment,tooltip=after_stat({
                                                     paste0(
                                                       "sampleID: ", levels(as.factor(dTOplot()$sampleID)),
                                                       "\ndetected proteins: ",
                                                       as.numeric(table(dTOplot()$sampleID[not_na(dTOplot()$abundances)])),
                                                       "\nQ3: ", round(as.data.frame(dTOplot() %>%
                                                                                       group_by(sampleID) %>%
                                                                                       dplyr::summarize(q75=quantile(abundances, probs = 0.75,na.rm=T)))$q75,2),
                                                       "\nmedian: ", round(as.data.frame(dTOplot() %>%
                                                                                           group_by(sampleID) %>%
                                                                                           dplyr::summarize(m=median(abundances,na.rm=T)))$m,2),
                                                       "\nQ1: ", round(as.data.frame(dTOplot() %>%
                                                                                       group_by(sampleID) %>%
                                                                                       dplyr::summarize(q25=quantile(abundances, probs = 0.25,na.rm=T)))$q25,2),
                                                       "\nnot detected (of all detected proteins in the data set): ",
                                                       as.numeric(table(dTOplot()$sampleID[is.na(dTOplot()$abundances) | dTOplot()$abundances==0])),
                                                       "\n% of detected proteins: ",
                                                       round(100*as.numeric(table(dTOplot()$sampleID[not_na(dTOplot()$abundances)]))/length(unique(dTOplot()$Accession)),2),
                                                       " %"
                                                     )}),data_id=sampleID))+
                 geom_boxplot_interactive(varwidth=T,outlier.colour=input$out_color,
                                          outlier.size=input$out_size,
                                          notch=input$notch,na.rm=T)+
                 ylab(input$ylab)+xlab(input$xlab)+
                 theme_classic()+
                 theme(
                   axis.title = element_text(size=input$axis_title_size),
                   axis.text = element_text(size=input$axis_text_size),
                   legend.title = element_text(size=input$legend_title_size),
                   legend.text = element_text(size=input$legend_text_size)
                 )

               if(input$yzero){
                 ag_box_1=ag_box_1+ylim(0,NA)
               }

               if(input$xaxis){
                 ag_box_1=ag_box_1+
                   theme(axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())
               }

               x=ggiraph::girafe(ggobj = ag_box_1,
                                 height_svg = input$height,width_svg = input$width,
                                 options = list(
                                   opts_hover(css = ""),
                                   opts_zoom(max=4) ,
                                   opts_selection(type = "none",css = NULL),
                                   opts_toolbar(saveaspng=FALSE)
                                 )
               )
               x
             }, #ag_box_1 close
             "ag_hist_1"={
               if(input$select_1=="without grouping"){ #without grouping
                 ag_hist_1=ggplot(data=dTOplot(),aes(x=detected))+
                   geom_histogram_interactive(aes(tooltip=paste0("Bin range: [",
                                                                 round(after_stat(xmin),2),
                                                                 ",",round(after_stat(xmax),2),
                                                                 "] \ncount: ",after_stat(count))),
                                              fill=input$binfill,color=input$bincol,
                                              alpha=input$alpha,
                                              binwidth=input$binwidth)+
                   theme_classic()
               }else{ #with grouping (by treatment)
                 ag_hist_1=ggplot(data=dTOplot(),aes(x=detected,fill=treatment))+
                   geom_histogram_interactive(aes(tooltip=paste0("Bin range: [",
                                                                 round(after_stat(xmin),2),
                                                                 ",",round(after_stat(xmax),2),"]")),
                                              alpha=input$alpha,color=input$bincol,
                                              position="identity",
                                              binwidth=input$binwidth)+
                   theme_classic()+
                   theme(
                     panel.grid.major.y = element_line(colour="darkgrey"),
                     panel.grid.minor.y = element_line(colour="grey")
                   ) #no count tooltips so let's have major and minor grid
               }
               ag_hist_1=ag_hist_1+
                 xlab(input$xlab)+
                 theme(
                   axis.title = element_text(size=input$axis_title_size),
                   axis.text = element_text(size=input$axis_text_size),
                   legend.title = element_text(size=input$legend_title_size),
                   legend.text = element_text(size=input$legend_text_size)
                 )+ylab(input$ylab)

               x=ggiraph::girafe(ggobj = ag_hist_1,
                                 height_svg = input$height,width_svg = input$width,
                                 options = list(
                                   opts_hover(css = ""),
                                   opts_zoom(max=4),
                                   opts_selection(type = "none",css = NULL),
                                   opts_toolbar(saveaspng=FALSE)
                                 )
               )
               x
             }, #ag_hist_1 close
             "ag_bar_1"={
               if(input$select_1=="without grouping"){ #without grouping
                 ag_bar_1=ggplot(data=dTOplot(),aes(x=as.factor(detected_n),y=count,
                                                    tooltip=tooltip,
                                                    data_id=detected_n))+
                   geom_col_interactive(fill=input$barfill,color=input$barcol)
               }else{ #with grouping (by treatment)
                 ag_bar_1=ggplot(data=dTOplot(),aes(x=as.factor(detected_n),y=count,
                                                    fill=treatment,
                                                    tooltip=tooltip,
                                                    data_id=detected_n))+
                   geom_col_interactive(color=input$barcol,position=position_dodge2())
               }

               ag_bar_1=ag_bar_1+
                 xlab(input$xlab)+
                 ylab(input$ylab)+
                 theme_classic()+
                 theme(
                   axis.title = element_text(size=input$axis_title_size),
                   axis.text = element_text(size=input$axis_text_size),
                   legend.title = element_text(size=input$legend_title_size),
                   legend.text = element_text(size=input$legend_text_size)
                 )

               x=ggiraph::girafe(ggobj = ag_bar_1,
                                 height_svg = input$height,width_svg = input$width,
                                 options = list(
                                   opts_hover(css = ""),
                                   opts_zoom(max=4),
                                   opts_selection(type = "none",css = NULL),
                                   opts_toolbar(saveaspng=FALSE)
                                 )
               )
               x
             }, #ag_bar_1 close
             NULL) #switch close

    }) #plot close

    observeEvent(input$render | input$apply,{
      req(plot())
      switch(plot_type,
             "eda_box_1"={r$eda_box_1=plot()},
             "eda_hist_1"={r$eda_hist_1=plot()},
             "ag_box_1"={r$ag_box_1=plot()},
             "ag_hist_1"={r$ag_hist_1=plot()},
             "ag_bar_1"={r$ag_bar_1=plot()}
             )
    })

    ####Downloads ----

    #Static:
    output$down = downloadHandler(

      filename = function() {
        paste0(plot_type, ".png")
      },

      content = function(file) {
        ggsave(file,height=input$height,width=input$width)
      }
    )

    #Interactive:
    output$downI = downloadHandler(

      filename = function() {
        paste0(plot_type, ".html")
      },

      content = function(file) {
        saveWidget(as_widget(plot()),file,selfcontained = T)
      }
    )

    #### Helpers ----
    observeEvent(input$help,{
      showModal(
        modalDialog(
          switch(plot_type,
                 "eda_box_1"=includeMarkdown(app_sys("app/www/helper_eda_box_1.Rmd")),
                 "eda_hist_1"=includeMarkdown(app_sys("app/www/helper_eda_hist_1.Rmd")),
                 "ag_box_1"=includeMarkdown(app_sys("app/www/helper_eda_box_1.Rmd")),
                 "ag_hist_1"=includeMarkdown(app_sys("app/www/helper_eda_hist_1.Rmd")),
                 "ag_bar_1"=includeMarkdown(app_sys("app/www/helper_ag_bar_1.Rmd"))
          ),
          footer = modalButton("Close"),
          size="l",
          easyClose = T
        )
      )
    })

    #### Dropdown ----
    observeEvent(input$apply, {
      toggleDropdownButton("dropmenu", session)
    })

    observe({
      if(input$select_1=="with grouping"){
        shinyjs::hide("binfill")
      }else{
        shinyjs::show("binfill")
      }
    })

    #### Alerts ----
    observeEvent(input$render | input$apply,{
      if(plot_type=="eda_box_1" & !all(r$d1[,-1]>0,na.rm = T) & input$select_1=="log-transformed"){
      shinyalert(title = "There are zeros in the dataset!",
                 text = "Cannot logarithm the data, please choose another form of abundances - e.g. log2(x+1).",
                 showConfirmButton = TRUE, type = "error")
        r$eda_box_1=NULL
      }
    })

    #### Other stuff ----
    if(plot_type=="ag_box_1"){
      shinyjs::hide("select_1")
    }

  })
}

## To be copied in the UI
#

## To be copied in the server
#
