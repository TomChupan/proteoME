# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "first_try", with_test = TRUE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

#Is the app in dev-mode?
# Setting the option to FALSE
options( "golem.app.prod" = FALSE)
# Function runs as expected
golem::cat_dev("In dev\n")

## Add packages
## Recommended:
#golem::use_recommended_deps()
## Other packages:
#usethis::use_package("pkg.you.want.to.add")
usethis::use_package("shinydashboard")
usethis::use_package("shinyalert")
usethis::use_package("shinyWidgets")
usethis::use_package("ggplot2")
usethis::use_package("tools")
usethis::use_package("vroom")
usethis::use_package("Hmisc")
## Check if you haven't missed anything:
# This function will read all the scripts in the R/ folder and
# try to guess required dependencies
attachment::att_from_rscripts()

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("timer")
golem::add_js_handler("handler0")
golem::add_css_file("css0")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("proteoME")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action(name = "check-standard")
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html



# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
