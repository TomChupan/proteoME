# analysis UI Function

#' #' @title mod_analysis_ui and mod_analysis_server
#' @description A shiny Module containing sidebar content for data analysis
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_analysis
#'
#' @export
#'
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs hide hidden show
#' @importFrom dplyr %>% group_by summarise across everything mutate_if
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom rstatix dunn_test

mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(".button {background-color:#90EE90;}
                  .custom-h4 {margin-left: 10px;}")
    ),
    sidebarMenu(
      menuItem(text = "Analyse dataset",
               startExpanded = T,
               uiOutput(ns("input_ngroups")),
               uiOutput(ns("input_groups")),
               uiOutput(ns("input_method")),
               shinyjs::hidden(
                 sliderInput(ns("alpha"),"P-value treshold for multiple comparison",
                              min=0,max=0.2,value=0.05)
               ),
               actionButton(ns("analyse"),"Analyse"),
               shinyjs::hidden(
                 actionButton(ns("ancheck"),"Analysed",class="button",
                              icon=icon("check")),
                 actionButton(ns("reset"),"Reset (change groups/method/parameters)")
                 )
      ) #menuItem close
    ) #sidebarMenu close


  ) #tagList close
}

# analysis Server Functions

#' @param r A 'reactiveValues()' list containing (among other objects) data set with
#' abundances. This is analysed here with one of the available methods.
#' A logical indicator (analysed or not?) is toggled here.
#' @rdname mod_analysis
#'
#' @export
#'
mod_analysis_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      req(input$ngroups)
      r$ngroups=input$ngroups
      r$an_method=input$method
    })

    #####Reactive inputs ----
    output$input_ngroups=renderUI({
      req(not_null(r$d3))
      numericInput(ns("ngroups"),"Number of groups to compare:",
                   min=2,max=length(levels(as.factor(r$d3$treatment))),
                   value=length(levels(as.factor(r$d3$treatment))),step=1)
    })

    output$input_groups=renderUI({
      req(input$ngroups)
      if(input$ngroups!=length(levels(as.factor(r$d3$treatment))))
      selectInput(ns("groups"),"Groups to compare:",
                  choices = levels(as.factor((r$d3$treatment))),multiple = T)
    })
    #shinyFeedback for this input (we need it to be equal to input$ngroups):
    observeEvent(input$groups,{
      if (length(input$groups)!=input$ngroups){
        showFeedbackWarning(
          inputId = "groups",
          text = "You should pick a number of groups selected in the previous input."
        )
        shinyjs::hide("analyse")
      }else{
        hideFeedback("groups")
        shinyjs::show("analyse")
      }
    })

    output$input_method=renderUI({
      req(input$ngroups)
      selectInput(ns("method"),"Select a method for comparing groups:",
                  choices = {if(input$ngroups==2){
                    c("t-test"="t.test",
                      "Wilcoxon signed-rank test"="wilcoxon")
                  }else{
                    c("ANOVA"="anova","Kruskal–Wallis test"="kw.test")
                  }
                  },multiple = F)
    })

    observe({
      req(input$method)
      if(input$method %in% c("anova","kw.test") & r$analysedTF==FALSE){
        shinyjs::show("alpha")
      }else{
        shinyjs::hide("alpha")
      }
    })

    #####Analysis process -----

    observeEvent(input$analyse,{
      if(is.null(r$d4)){
        shinyalert(title = "Please aggregate your dataset first!",
                   text="Analysis is performed on aggregated data. For aggregation, use the 'Aggregate' tab.",
                   showConfirmButton = TRUE, type = "info")
      }else{ #we are ready for analysis
        ######Data preparation ----
        dTOtest=r$d4[,-1]
        proteins=as.data.frame(r$d4[,1])
        groups=merge(data.frame(sampleID=names(dTOtest)),r$d3[,1:2],
                     by="sampleID",sort = F)$treatment
        if(input$ngroups!=length(levels(as.factor(r$d3$treatment)))){ #only some groups
          #filter columns we don't need:
          ind=which(groups %in% input$groups)
          dTOtest=dTOtest[,ind]
          groups=groups[ind]
        }
        #Do we have enough data?
        n=min(unlist(apply(dTOtest,1,function(x){
          table(groups[!is.na(as.numeric(x))])
        }))) #minimal number of not-NA values in any group
        if(n<2){
          shinyalert(title = "You do not have enough data to perform an analysis.",
                     text="Please be stricter when filtering your data or impute your data.",
                     showConfirmButton = TRUE, type = "warning")
        }else{ #enough data, go ahead
        ######Group means/medians: ----
        tdTOtest=as.data.frame(t(dTOtest))
        tdTOtest$groups=groups

        if(input$method %in% c("t.test","anova")){ #means for parametric test
          #group means:
          means=tdTOtest %>%
            group_by(groups) %>%
            summarise(across(everything(), ~mean(., na.rm = TRUE)))
          group_names=means$groups
          m_df=as.data.frame(t(as.data.frame(means)[,-1]))
          colnames(m_df)=paste0("mean_",group_names)
          #differences:
          for(i in 1:(length(group_names)-1)){
            for(j in (i + 1):length(group_names)){
              difference=as.numeric(m_df[, i])-as.numeric(m_df[, j])
              column_name=paste0("diffmean",group_names[i],"_",group_names[j])
              m_df[[column_name]]=difference
            }
          }
        } #close if means
        else{ #medians for non-parametric test
          #group medians:
          medians=tdTOtest %>%
            group_by(groups) %>%
            summarise(across(everything(), ~median(., na.rm = TRUE)))
          group_names=medians$groups
          m_df=as.data.frame(t(as.data.frame(medians)[,-1]))
          colnames(m_df)=paste0("median_",group_names)
          #differences:
          for(i in 1:(length(group_names)-1)){
            for(j in (i + 1):length(group_names)){
              difference=as.numeric(m_df[, i])-as.numeric(m_df[, j])
              column_name=paste0("diffmedian",group_names[i],"_",group_names[j])
              m_df[[column_name]]=difference
            }
          }
        } #close else medians

        #####Test ----
        #t-test:
        if(input$method=="t.test"){
            pval=apply(dTOtest,1,function(x){
              t=t.test(as.numeric(x)~groups)
              t$p.value
            })
        }
        #wilcoxon:
        if(input$method=="wilcoxon"){
          suppressWarnings({
            pval=apply(dTOtest,1,function(x){
              t=wilcox.test(as.numeric(x)~groups)
              t$p.value
            })
          })
        }
        #anova:
        if(input$method=="anova"){
          pval=vector()
          signif_diff=vector()
          for(i in 1:nrow(dTOtest)){
            model=aov(as.numeric(dTOtest[i,])~groups)
            t=summary(model)
            pval[i]=t[[1]][["Pr(>F)"]][1]
            tukey=as.data.frame(TukeyHSD(model)[[1]])
            rows=which(tukey$`p adj`<input$alpha)
            if(length(rows)>0){
              signif_diff[i]=paste(rownames(tukey)[rows],collapse = ",")
            }else{
              signif_diff[i]=NA
            }
          }
        }
        if(input$method=="kw.test"){
          pval=vector()
          signif_diff=vector()
          for(i in 1:nrow(dTOtest)){
            pval[i]=kruskal.test(as.numeric(dTOtest[i,])~groups)$p.value
            df_dunn=data.frame(abundances=as.numeric(dTOtest[i,]),groups=groups)
            dunn=rstatix::dunn_test(data=df_dunn,formula=abundances~groups,
                                    p.adjust.method = "BH")
            rows=which(dunn$p.adj<input$alpha)
            if(length(rows)>0){
              signif_diff[i]=paste0(dunn$group1[rows],"-",dunn$group2[rows],collapse = ",")
            }else{
              signif_diff[i]=NA
            }
          }
        }

        m_df$p.value=pval
        m_df$adj.p.value=p.adjust(pval,method = "BH")
        if(input$method %in% c("anova","kw.test")){
          m_df$signif_diff=signif_diff
        }

        results_df=cbind(data.frame(Accession=proteins),m_df)

        rounding=function(x){ifelse(abs(x)<1,signif(x,3),round(x,3))}

        results_df=results_df %>%
          mutate_if(is.numeric, rounding)#####když <1, signif, jinak round

        r$analysedTF=TRUE
        shinyjs::hide("ngroups")
        shinyjs::hide("groups")
        shinyjs::hide("method")
        shinyjs::hide("alpha")
        shinyjs::hide("analyse")
        shinyjs::show("ancheck")
        shinyjs::show("reset")

                r$results=results_df
        } #else 'enough data' close
      } #else 'analysis' close
    }) #observeEvent close

    ####After analysis ----
    observeEvent(input$ancheck,{
      shinyalert(title = "Your dataset has already been analysed!",
                 text="If you want to change analysed groups, method or parameters please click on the reset button below.",
                 showConfirmButton = TRUE, type = "info")
    })

    ####Resets -----
    observeEvent(input$reset,{
      ask_confirmation(inputId = "reconfirm",title = "Are you sure?",
                       text = "You may lose results of the current analysis (table/volcano plot). Make sure you have downloaded them.",
                       type = "info",cancelOnDismiss = T,
                       btn_labels = c("No, I'll think about it.","Yes, I want to analyse data another way!")
      )
    })

    observeEvent(input$reconfirm,{
      if(isTRUE(input$reconfirm)){
      #Indicator:
      r$analysedTF=FALSE
      #Analysis results:
      r$results=NULL
      #Plots:
      r$an_volcano_1=NULL
      }
    })

    observe({
      req(input$method)
      if(r$analysedTF==FALSE){
        shinyjs::show("ngroups")
        shinyjs::show("groups")
        shinyjs::show("method")
        if(input$method %in% c("anova","kw.test")){
        shinyjs::show("alpha")
        }
        shinyjs::show("analyse")
        shinyjs::hide("ancheck")
        shinyjs::hide("reset")
      }
    })



    ####Helpers ----
    #observeEvent(input$help,{
    #  showModal(
    #    modalDialog(
    #      includeMarkdown(app_sys("app/www/helper_an_1.Rmd")),
    #      footer = modalButton("Close"),
    #      size="l",
    #      easyClose = T
    #    )
    #  )
    #})


  })
}

## To be copied in the UI
#

## To be copied in the server
#
