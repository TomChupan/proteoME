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
#' @noRd
#'
#' @importFrom dplyr %>% mutate if_else
#' @importFrom ggplot2 ggplot after_stat theme_classic theme_minimal aes ggsave ylab xlab theme element_text ylim scale_y_continuous element_line element_blank position_dodge2 geom_hline geom_vline scale_color_manual
#' @importFrom ggiraph geom_boxplot_interactive girafe opts_hover opts_zoom opts_toolbar geom_histogram_interactive opts_selection geom_col_interactive geom_point_interactive
#' @importFrom htmlwidgets saveWidget
#' @importFrom plotly as_widget
#' @importFrom shinyWidgets dropdownButton toggleDropdownButton
#' @importFrom colourpicker colourInput
#' @importFrom shinyjs hide show toggle
#' @importFrom tidyr pivot_wider
#' @importFrom utils combn
#' @importFrom ggrepel geom_text_repel
#' @importFrom magrittr set_names
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
                                      F),
               br(),
               HTML("What does this plot show",as.character(
                 actionLink(inputId = ns("help"),
                            label = "",
                            icon = icon("circle-question")))
               ),
               uiOutput(ns("input_select_1")),
               uiOutput(ns("input_select_2")),

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
                          uiOutput(ns("input_xlab"))
                   ),
                   column(6,numericInput(ns("width"),"Width (inches)",min=1,
                                         max=30,value=9,step=1),
                          numericInput(ns("axis_title_size"),"Axis title size",
                                         min=1,max=50,value=16,step=1),
                          if(plot_type!="an_volcano_1"){
                          numericInput(ns("legend_title_size"),"Legend title size",
                                       min=1,max=50,value=16,step=1)
                            },
                          uiOutput(ns("input_ylab")))
                 ), #fludiRow close
                 h4("Features specific to the plot type:"),
                 if(grepl("box",plot_type)){ #boxplots
                   fluidRow(
                   checkboxInput(ns("yzero"),"Start y-axis from zero",value=F),
                   checkboxInput(ns("notch"),"Notch (approx 95% CI for median)",
                                 value=F),
                   checkboxInput(ns("xaxis"),"Remove x-axis text",value = T),
                   column(6,colourInput(ns("out_color"),"Outlier color",
                                        palette = "limited",value = "#000000")
                          ),
                   column(6,numericInput(ns("out_size"),"Outlier size",min=1,
                                         max=50,value=2,step=1))
                   )#fluidRow close
                 }, #boxplots close
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
                 }, #histograms close
                 if(grepl("bar",plot_type)){ #barplots
                   fluidRow(
                     column(6,colourInput(ns("barfill"),"Bar color (fill)",
                                          palette = "square",value = "lightblue")),
                     column(6,colourInput(ns("barcol"),"Bar color (border)",
                                          palette = "limited",value = "#000000"))
                   ) #fluidRow close
                 }, #barplots close
                 if(grepl("volcano",plot_type)){ #volcano
                   fluidRow(
                     column(6,
                            textInput(ns("incon"),"Label for inconclusive difference",
                                      value="Inconclusive"),
                            uiOutput(ns("input_firstB")),
                            numericInput(ns("diffthresh"),"Threshold for difference",
                                         min=0,value=4,step=0.1),
                            numericInput(ns("alpha"),
                                         "Degree of transparency (alpha)",min=0,
                                         max=1,value=0.75,step=0.01)),
                     column(6,
                            textInput(ns("nonsig"),"Label for non-significant difference",
                                        value="Non-significant"),
                            uiOutput(ns("input_secondB")),
                            numericInput(ns("pvalthresh"),"Threshold for p-value",
                                         min=0.001,max=1,value=0.01,step=0.001))
                   ) #fluidRow close
                 }, #volcano close
                 actionButton(ns("apply"),"Apply changes")
                   ) #div close

                 ), #dropdownButton close
               actionButton(ns("render"),label = "Render plot",icon = icon("play")),
      shinyjs::hidden(
               downloadButton(ns("down"),label = "Download plot (.png)")
               ),br(),
      shinyjs::hidden(
               downloadButton(ns("downI"),label = "Download interactive plot (.html)")
      )

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
#' @noRd
#'
mod_plot_server <- function(id,plot_type,r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ####Interactive inputs----
    output$input_select_1=renderUI({
      selectInput(ns("select_1"),
                  label=switch(plot_type,
                               "eda_box_1"="Select the form of abundances:",
                               "eda_hist_1"="Select the grouping option:",
                               "ag_hist_1"="Select the grouping option:",
                               "ag_bar_1"="Select the grouping option:",
                               "an_volcano_1"="Select the type of p-value:",
                               NULL),
                  choices = switch(plot_type,
                                   "eda_box_1"={
                                     if(r$transformedTF==F & r$normalizedTF==F){
                                       c("just the values"="original",
                                                 "log-transformed","log2(x+1)",
                                                 "square root")}else{
                                        c("just the values"="original")
                                                 }},
                                   "eda_hist_1"=c("without grouping","with grouping"),
                                   "ag_hist_1"=c("without grouping","with grouping"),
                                   "ag_bar_1"=c("without grouping","with grouping"),
                                   "an_volcano_1"=c("original p-value"="original",
                                                    "adjusted p-value"="adjusted"),
                                   NULL),multiple = F
      )
    })

    output$input_select_2=renderUI({
      req(not_null(r$ngroups))
      req(plot_type=="an_volcano_1" & r$ngroups>2)
      if(r$ngroups!=length(levels(as.factor(r$d3$treatment)))){
        req(not_null(r$groups))
        pairs=combn(r$groups, 2)
      }else{
        pairs=combn(as.character(levels(as.factor(r$d3$treatment))),2)
      }
      pairs=apply(pairs, 2, function(pair) paste(pair, collapse = ", "))
      selectInput(ns("select_2"),"Select a pair of treatment groups:",
                  choices = pairs,multiple = F)

    })

    output$input_xlab=renderUI({
      if(plot_type=="an_volcano_1"){
      req(not_null(r$ngroups))
      n=length(levels(as.factor(r$d3$treatment)))
      if(n==2){
        g1=levels(as.factor(r$d3$treatment))[1]
        g2=levels(as.factor(r$d3$treatment))[2]
      }else if(r$ngroups==2){
        req(not_null(r$groups))
        g1=r$groups[1]
        g2=r$groups[2]
      }else{
        req(input$select_2)
        g1=strsplit(input$select_2, ", ")[[1]][1]
        g2=strsplit(input$select_2, ", ")[[1]][2]
      }
      }
      textInput(ns("xlab"),label = "X-axis label",
                value = switch(plot_type,
                               "eda_box_1"="runID",
                               "eda_hist_1"="number of detected proteins in each run",
                               "ag_box_1"="sampleID",
                               "ag_hist_1"="number of detected proteins in each sample",
                               "ag_bar_1"="number of detections of one protein within one sample",
                               "an_volcano_1"={
                                 req(r$analysedTF==TRUE)
                                 paste0("Difference of group medians (",
                                                     g1,"-",g2,")")
                               }

                ))
    })
    outputOptions(output, "input_xlab", suspendWhenHidden=FALSE)

    output$input_ylab=renderUI({
      req(input$select_1)
      textInput(ns("ylab"),label = "Y-axis label",
                value = switch(plot_type,
                               "eda_box_1"="abundances",
                               "eda_hist_1"="count",
                               "ag_box_1"="abundances",
                               "ag_hist_1"="count",
                               "ag_bar_1"="count",
                               "an_volcano_1"=ifelse(r$ngroups==2,
                                                     ifelse(input$select_1=="original","-log10(p-value)","-log10(adjusted p-value)"),
                                                     ifelse(r$an_method=="anova","-log10(Tukey adjusted p-value)","-log10(Dunn adjusted p-value)")
                               )
                               ))
    })
    outputOptions(output, "input_ylab", suspendWhenHidden=FALSE)

    output$input_firstB=renderUI({
      req(not_null(r$ngroups))
      n=length(levels(as.factor(r$d3$treatment)))
      if(n==2){
        g=levels(as.factor(r$d3$treatment))[1]
      }else if(r$ngroups==2){
        req(not_null(r$groups))
        g=r$groups[1]
      }else{
        req(input$select_2)
        g=strsplit(input$select_2, ", ")[[1]][1]
      }
      textInput(ns("firstB"),"Label for significant positive difference",
                          value=paste0(g," up-regulated")
      )
    })
    outputOptions(output, "input_firstB", suspendWhenHidden=FALSE)

    output$input_secondB=renderUI({
      req(not_null(r$ngroups))
      n=length(levels(as.factor(r$d3$treatment)))
      if(n==2){
        g=levels(as.factor(r$d3$treatment))[1]
      }else if(r$ngroups==2){
        req(not_null(r$groups))
        g=r$groups[1]
      }else{
        req(input$select_2)
        g=strsplit(input$select_2, ", ")[[1]][1]
      }
      textInput(ns("secondB"),"Label for significant negative difference",
                value=paste0(g," down-regulated")
      )
    })
    outputOptions(output, "input_secondB", suspendWhenHidden=FALSE)

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
               req(not_null(r$trans_method))
               if((r$transformedTF==TRUE & r$trans_method=="log2(x)")|r$normalizedTF==TRUE){
                 det=as.numeric(colSums(not_na(r$d1[-1])))
                 name_det=names(colSums(not_na(r$d1[-1])))
               }else{
                 det=as.numeric(colSums(not_na(r$d1[-1]) &
                                          r$d1[-1]!=0))
                 name_det=names(colSums(not_na(r$d1[-1]) &
                                          r$d1[-1]!=0))
               }
               d=data.frame(detected=det,
                            runID=name_det
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
               req(not_null(r$trans_method))
               if((r$transformedTF==TRUE & r$trans_method=="log2(x)")|r$normalizedTF==TRUE){
                 det=as.numeric(colSums(not_na(r$d4[-1])))
                 name_det=names(colSums(not_na(r$d4[-1])))
               }else{
                 det=as.numeric(colSums(not_na(r$d4[-1]) &
                                          r$d4[-1]!=0))
                 name_det=names(colSums(not_na(r$d4[-1]) &
                                          r$d4[-1]!=0))
               }
               d=data.frame(detected=det,
                            sampleID=name_det
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
                 if(is.factor(r$d3$treatment)==T){
                   d$treatment=factor(d$treatment,levels = levels(r$d3$treatment))
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
             "an_volcano_1"={
               req(not_null(r$results))
               d=r$results
               if(r$ngroups==2){ #t-test/wilcoxon
               #Keep just the selected form of p-value:
               if(input$select_1=="original"){
                 d$adj.p.value=NULL
                 }else{
                   d$p.value=NULL
                   names(d)[grepl("value",names(d))]="p.value"
                 }
                 #-log10(p-value):
                 d$log_pval=(-1)*log10(d$p.value)
               }else{ #anova/kw with MC
                 pair_string=paste(strsplit(input$select_2, ", ")[[1]],collapse="_")
                 rev_pair_string=paste(rev(strsplit(pair_string, "")[[1]]), collapse = "")
                 d$log_pval=switch(r$an_method,
                                   "anova"={if(sum(grepl(paste0("^tukey.+",pair_string,"$"), names(d)))>0){
                                     (-1)*log10(d[,grepl(paste0("^tukey.+",pair_string,"$"), names(d))])
                                      }else{
                                        (-1)*log10(d[,grepl(paste0("^tukey.+",rev_string,"$"), names(d))])
                                      }},
                                   "kw.test"={if(sum(grepl(paste0("^dunn.+",pair_string,"$"), names(d)))>0){
                                     (-1)*log10(d[,grepl(paste0("^dunn.+",pair_string,"$"), names(d))])
                                   }else{
                                     (-1)*log10(d[,grepl(paste0("^dunn.+",rev_string,"$"), names(d))])
                                   }},
                                   NULL
                 )
               }


               #diff column (x-axis of the volcano):
               if(r$ngroups==2){ #two groups (t-test/wilcoxon):
                 #Rename diffmedian of the only pair:
                 names(d)[grepl("diffm",names(d))]="diff"
               }else{ #more than 2 groups (anova/kw):
                 if(sum(grepl(paste0("^diffm.+",pair_string,"$"), names(d)))>0){
                   names(d)[grepl(paste0("^diffm.+",pair_string,"$"), names(d))]="diff"
                 }else{
                   names(d)[grepl(paste0("^diffm.+",rev_pair_string,"$"), names(d))]="diff"
                 }
               }

               #threshold:
               d=d %>%
                 mutate(threshold = if_else(diff >= input$diffthresh & log_pval >= (-1)*log10(input$pvalthresh),
                                            input$firstB,
                                            if_else(diff <= (-1)*input$diffthresh & log_pval >= (-1)*log10(input$pvalthresh),
                                                    input$secondB,
                                                    if_else(abs(diff)<input$diffthresh & log_pval >= (-1)*log10(input$pvalthresh),
                                                            input$incon,
                                                            input$nonsig)
                                            ))
                 )

               r$results$threshold=d$threshold
               d
             }, #an_volcano_1 close
             NULL)
    }) #dTOplot close

    ####Plot ----

    splot=eventReactive(input$render | input$apply,{
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

               eda_box_1
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


               eda_hist_1
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


               ag_box_1
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


               ag_hist_1
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


               ag_bar_1
             }, #ag_bar_1 close
             "an_volcano_1"={
               an_volcano_1=ggplot(data=dTOplot(),aes(x=diff,y=log_pval,
                                                      colour = threshold))+
                 geom_point_interactive(aes(tooltip = paste0("Accession number: ", Accession, "<br>",
                                                             "Difference of group medians: ",
                                                             round(diff,3), "<br>",
                                                             ifelse(input$select_1=="original",
                                                                    "","adjusted "),
                                                             "p-value: ", signif(10^(-log_pval),3))),
                                        alpha = input$alpha)+
                 geom_hline(yintercept = (-1)*log10(input$pvalthresh), linetype = 2, alpha = 0.5) +
                 geom_vline(xintercept = input$diffthresh, linetype = 2, alpha = 0.5) +
                 geom_vline(xintercept = (-1)*input$diffthresh, linetype = 2, alpha = 0.5) +
                 xlab(input$xlab)+ylab(input$ylab) +
                 theme_classic()+
                 theme(
                   axis.title = element_text(size=input$axis_title_size),
                   axis.text = element_text(size=input$axis_text_size),
                   legend.title = element_blank(),
                   legend.text = element_text(size=input$legend_text_size)
                 )+
                 ggrepel::geom_text_repel(
                   data = subset(dTOplot(), threshold%in%c(input$firstB,input$secondB)),
                   aes(diff,log_pval, label = Accession),
                   alpha = 0.6, max.overlaps = 50)+
                 scale_color_manual(values=magrittr::set_names(c("black","grey","green", "red"),
                                                               c(as.character(input$nonsig),
                                                                 as.character(input$incon),
                                                                 as.character(input$firstB),
                                                                 as.character(input$secondB))))

               an_volcano_1
             }, #an_volcano_1 close
             NULL) #switch close

    }) #splot close

    plot=eventReactive(input$render | input$apply,{
      req(not_null(splot()))
      x=ggiraph::girafe(ggobj = splot(),
                        height_svg = input$height,width_svg = input$width,
                        options = list(
                          opts_hover(css = ""),
                          opts_zoom(max=4),
                          opts_selection(type = "none",css = NULL),
                          opts_toolbar(saveaspng=FALSE)
                        )
      )
      x
    })

    observeEvent(input$render | input$apply,{
      req(plot())
      switch(plot_type,
             "eda_box_1"={r$eda_box_1=plot()},
             "eda_hist_1"={r$eda_hist_1=plot()},
             "ag_box_1"={r$ag_box_1=plot()},
             "ag_hist_1"={r$ag_hist_1=plot()},
             "ag_bar_1"={r$ag_bar_1=plot()},
             "an_volcano_1"={r$an_volcano_1=plot()}
             )
    })

    ####Downloads ----

    #Static:
    output$down = downloadHandler(

      filename = function() {
        paste0(plot_type, ".png")
      },

      content = function(file) {
        ggsave(file,splot(),height=input$height,width=input$width)
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

    observe({
      if(not_null(r[[paste0(plot_type)]])){
        shinyjs::show("down")
        shinyjs::show("downI")
      }else{
        shinyjs::hide("down")
        shinyjs::hide("downI")
      }
    })



    #### Helpers ----
    observeEvent(input$help,{
      showModal(
        modalDialog(
          switch(plot_type,
                 "eda_box_1"=includeMarkdown(app_sys("app/www/helper_eda_box_1.Rmd")),
                 "eda_hist_1"=includeMarkdown(app_sys("app/www/helper_eda_hist_1.Rmd")),
                 "ag_box_1"=includeMarkdown(app_sys("app/www/helper_eda_box_1.Rmd")),
                 "ag_hist_1"=includeMarkdown(app_sys("app/www/helper_eda_hist_1.Rmd")),
                 "ag_bar_1"=includeMarkdown(app_sys("app/www/helper_ag_bar_1.Rmd")),
                 "an_volcano_1"=includeMarkdown(app_sys("app/www/helper_an_volcano_1.Rmd"))
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
      req(input$select_1)
      if(input$select_1=="with grouping"){
        shinyjs::hide("binfill")
        shinyjs::hide("barfill")
      }else{
        shinyjs::show("binfill")
        shinyjs::show("barfill")
      }
    })

    #### Alerts ----
    observeEvent(input$render | input$apply,{
      req(input$select_1)
      if(plot_type=="eda_box_1" & !all(r$d1[,-1]>0,na.rm = T) & input$select_1=="log-transformed"){
      shinyalert(title = "There are zeros in the dataset!",
                 text = "Cannot logarithm the data, please choose another form of abundances - e.g. log2(x+1).",
                 showConfirmButton = TRUE, type = "error")
        r$eda_box_1=NULL
      }
    })

    #### Other stuff ----
    if(plot_type=="ag_box_1"){
      shinyjs::hide("input_select_1")
    }

    observe({
      req(r$ngroups)
      if(r$ngroups>2 & plot_type=="an_volcano_1"){
        shinyjs::hide("input_select_1")
      }
      if(r$ngroups==2 & plot_type=="an_volcano_1"){
        shinyjs::show("input_select_1")
      }
    })



  })
}

## To be copied in the UI
#

## To be copied in the server
#
