#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import markdown
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyFeedback useShinyFeedback
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "proteoME app"
                      ),
      dashboardSidebar(width = '25%',
                       mod_sidebar_ui("sidebar_1")
                       ),
      dashboardBody(
        tags$head(
          tags$style(
            HTML(".sidebar-toggle { display: none; }")
                    ) #I don't want to display the sidebar toggle
                 ),
        add_busy_spinner(spin = "fading-circle"),
        mod_body_ui("body_1")

      ) #dashboardBody close
    ) #dashboardPage close
  ) #tagList close
} #app_ui close

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "proteoME"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    useShinyjs(),
    useSweetAlert(),
    useShinyFeedback()
  )
}
