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
golem::add_module(name = "upload", with_test = TRUE)
golem::add_module(name = "sidebar", with_test = TRUE)
golem::add_module(name = "body", with_test = TRUE)
golem::add_module(name = "body_tab1", with_test = TRUE)
golem::add_module(name = "factors", with_test = TRUE)
golem::add_module(name="plot",with_test = TRUE)
golem::add_module(name = "transform", with_test = FALSE)
golem::add_module(name = "body_sumtab", with_test = FALSE)
golem::add_module(name = "normalize", with_test = FALSE)
golem::add_module(name = "body_normalize", with_test = FALSE)
golem::add_module(name = "aggregate", with_test = FALSE)
golem::add_module(name = "body_aggregate", with_test = FALSE)
golem::add_module(name = "body_filter", with_test = FALSE)
golem::add_module(name = "filter", with_test = FALSE)
golem::add_module(name = "impute", with_test = FALSE)
golem::add_module(name = "body_impute", with_test = FALSE)
golem::add_module(name = "analysis", with_test = FALSE)
golem::add_module(name = "body_analysis", with_test = FALSE)

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("Tsummary", with_test = FALSE)
golem::add_fct("proteoAG", with_test = FALSE)
golem::add_fct("detectionMedians", with_test = FALSE)
golem::add_fct("proteoFI", with_test = FALSE)
golem::add_utils("helpers", with_test = TRUE)

#Is the app in dev-mode?
# Setting the option to FALSE
options( "golem.app.prod" = FALSE)
# Function runs as expected
golem::cat_dev("In dev\n")

## Add packages ----
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
usethis::use_package("DT")
usethis::use_package("shinyjs")
usethis::use_package("magrittr")
usethis::use_package("dplyr")
usethis::use_package("data.table")
usethis::use_package("DescTools")
usethis::use_package("viridis")
usethis::use_package("shinyscreenshot")
usethis::use_package("naniar")
usethis::use_package("grid")
usethis::use_package("missForest")
usethis::use_package("stringr")
usethis::use_package("ChemoSpec")
usethis::use_package("amap")
usethis::use_package("shinyFeedback")
usethis::use_package("rstatix")
usethis::use_package("ggrepel")

#Packages from GitHub:
usethis::use_dev_package("preprocessCore",type = "Imports")
usethis::use_dev_package("impute",type = "Imports",remote = "gangwug/impute")
usethis::use_dev_package("ComplexHeatmap",type = "Imports")
usethis::use_dev_package("MBQN",type = "Imports")

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
usethis::use_data_raw(name = "data_example", open = TRUE)
usethis::use_data_raw(name = "ann_run_example", open = TRUE)
usethis::use_data_raw(name = "ann_sample_example", open = TRUE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("proteoME")
devtools::build_vignettes()

utils::browseVignettes("proteoME") #check created Vignettes

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

#or you can check it by
rcmdcheck::rcmdcheck()

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

# Checkers ----

#Check the complexity

#remotes::install_github("hrbrmstr/cloc")
library(cloc)
cloc_git("https://github.com/TomChupan/proteoME")

#install.packages("cyclocomp")
library(cyclocomp)
cyclocomp_package_dir(getwd())


# You're now set! ----

# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
