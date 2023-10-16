#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  #Max File Size:
  n=100 #file size limit (MB)
  options(shiny.maxRequestSize=n*1024^2)
}
