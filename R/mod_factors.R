# factors UI Function

#' @title mod_factors_ui and mod_factors_server
#' @description A shiny Module for setting factor variables and editing factor levels
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList conditionalPanel fileInput actionButton checkboxGroupInput updateCheckboxGroupInput observeEvent observe actionLink showModal modalDialog includeMarkdown
#' @importFrom dplyr group_by mutate row_number
#' @importFrom magrittr %>%
#' @importFrom DT DTOutput renderDT editData

mod_factors_ui <- function(id){
  ns <- NS(id)
  tagList(


  ) #tagList close
} #mod_factors_ui close

# factors Server Functions

#' @param r A "storage" for the variables used throughout the app
#'
#' @noRd

mod_factors_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


observeEvent(r$edit_factors_button, {
    showModal(
      modalDialog(
        title = "Set factor variables, their levels and order (1 = reference)",
        size="l",
        easyClose = F,
        footer = tagList(
          column(1,actionButton(ns("help"),"Guide")),
          column(6),
          column(2,actionButton(ns("apply_changes"), "Apply changes")),
          column(2,actionButton(ns("close"), "Close without changes"))
        ),
        fluidPage(
          fluidRow(
            selectInput(ns("factor_cols"), "Factor Columns", choices = names(r$d3)[-1], multiple = TRUE)
          ),#fluidRow close
          fluidRow(
            DTOutput(ns("factor_levels_table"))
          ), #fluidRow close
          fluidRow(
            uiOutput(ns("guide"))
          ) #fluidRow close
        ) #fluidPage close
      ) #modalDialog close
    ) #showModal close

})

    observe({
      req(not_null(r$d3))
      r$edit_table=data.frame(
        Variable = rep(input$factor_cols,lapply(lapply(r$d3[input$factor_cols],unique),length)),
        Original_Levels = c(unlist(lapply(r$d3[input$factor_cols],function(column){
          if(is.factor(column)){levels(column)}else{unique(column)}
        }
        ))),
        New_Levels = c(unlist(lapply(r$d3[input$factor_cols],function(column){
          if(is.factor(column)){levels(column)}else{unique(column)}
        }
        ))),
        stringsAsFactors = FALSE
      )

      {
        req(length(input$factor_cols)>0)
        r$edit_table=isolate(r$edit_table) %>%
          group_by(Variable) %>%
          mutate(Original_Order = row_number()) %>%
          mutate(New_Order = row_number()) #%>%
          #mutate(Color = row_number()) #in the future
      }
    })

    observe({
      req(length(input$factor_cols)>0)
      output$factor_levels_table={
        renderDT(r$edit_table,
                 editable=list(target = 'cell',
                               disable = list(columns = c(0,1,3))),
                 selection="none",
                 options = list(dom = 't',
                                pageLength = 10,
                                autoWidth = TRUE),
                 rownames = FALSE)
      }

    })

    observeEvent(input$factor_levels_table_cell_edit, {
      r$edit_table <<- editData(r$edit_table,
                                input$factor_levels_table_cell_edit, 'factor_levels_table',rownames = F)
    })

    observeEvent(input$apply_changes, {
      req(length(input$factor_cols) > 0)
      if(sum(unlist(r$edit_table[,"Original_Levels"])!=unlist(r$edit_table[,"New_Levels"]))==0){ #nothing changed in levels
        for(col in unlist(unique(r$edit_table[,"Variable"]))){ #by variable
          rows=r$edit_table[,"Variable"]==col
          r$d3[,col]=factor(r$d3[,col],
                              levels = as.character(unlist(r$edit_table[rows,"New_Levels"]))[as.numeric(unlist(r$edit_table[rows,"New_Order"]))])
        }
      }else{ #new levels
        for(col in unlist(unique(r$edit_table[,"Variable"]))){ #by variable
          r$d3[,col]=as.character(r$d3[,col])
          rows=which(r$edit_table[,"Variable"]==col)
          for(original in unlist(r$edit_table[rows,"Original_Levels"])){ #by individual levels
            index=which(r$edit_table[,"Original_Levels"]==original)
            index=base::intersect(index,rows)
            r$d3[r$d3[,col]==original,col]=r$edit_table[index,"New_Levels"]
            if(col=="treatment"){ #we need to change r$d_pivotlonger and r$dAG_pivotlonger here
              r$d_pivotlonger=as.data.frame(r$d_pivotlonger)
              r$d_pivotlonger[r$d_pivotlonger[,col]==original,col]=r$edit_table[index,"New_Levels"]
              if(not_null(r$dAG_pivotlonger)){
                r$dAG_pivotlonger=as.data.frame(r$dAG_pivotlonger)
                r$dAG_pivotlonger[r$dAG_pivotlonger[,col]==original,col]=r$edit_table[index,"New_Levels"]
              }
            }
          }
          #set factors in specified order:
          r$d3[,col]=factor(r$d3[,col],
                              levels = as.character(unlist(r$edit_table[rows,"New_Levels"]))[as.numeric(unlist(r$edit_table[rows,"New_Order"]))])
          if(col=="treatment"){
            r$d_pivotlonger[,col]=factor(r$d_pivotlonger[,col],
                              levels = as.character(unlist(r$edit_table[rows,"New_Levels"]))[as.numeric(unlist(r$edit_table[rows,"New_Order"]))])
            if(not_null(r$dAG_pivotlonger)){
              r$dAG_pivotlonger[,col]=factor(r$dAG_pivotlonger[,col],
                                           levels = as.character(unlist(r$edit_table[rows,"New_Levels"]))[as.numeric(unlist(r$edit_table[rows,"New_Order"]))])
            }
          }
        }
      }


      r$analysedTF=FALSE
      r$results=NULL
      r$an_volcano_1=NULL

      removeModal()
      })

    observeEvent(input$help,{
      output$guide=renderUI(includeMarkdown("inst/app/www/helper_factors.Rmd"))
    })

    observeEvent(input$apply_changes,{
      req(length(input$factor_cols) > 0)
      output$guide=NULL
    })

    observeEvent(input$close,{
      output$guide=NULL
      removeModal()
    })







  }) #moduleServer close
} #mod_factors_server close

## To be copied in the UI
#

## To be copied in the server
#
