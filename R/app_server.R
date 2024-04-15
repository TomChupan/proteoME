#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  #Max File Size:
  options(shiny.maxRequestSize=100*1024^2) #100 = file size limit (MB)
  #when changing this limit - don't forgot to change a placeholder in a fileInput
  #within a mod_upload

  r=reactiveValues(
    transformedTF=FALSE,normalizedTF=FALSE,aggregatedTF=FALSE,filteredTF=FALSE,
    imputedTF=FALSE,turnoff_data_char=FALSE,analysedTF=FALSE
  )
  mod_sidebar_server("sidebar_1",r=r)
  mod_body_server("body_1",r=r)
}
