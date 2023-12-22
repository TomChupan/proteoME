#' @title Example data with protein abundances
#'
#' @description This dataset contains sample data with protein abundances to be used in a
#' proteoME Shiny app.
#'
#' @format ## data_example
#' A data frame with 18 rows and 7 columns:
#' \describe{
#' \itemize{
#'  \item{Accession}{An UniProt accession number – a unique identifier assigned
#'  to a protein entry in the UniProt Knowledgebase (UniProtKB).
#'  See \url{https://www.uniprot.org/} }
#'  \item{Abundances_F#}{Columns with protein abundances for each run
#'  (several runs may come from one patient – repetitions)}
#'  \itemize{
#'  \item Abundances can be raw, imputed or normalized (or both imputed and
#'   normalized)
#'   }
#'  }
#'
#' }
#' @source <https://raw.githubusercontent.com/TomChupan/data/main/data_example.csv>
#'
"data_example"

#' @title Example data with run annotations
#'
#' @description This dataset contains run annotations for *data_example* dataset
#' to be used in a proteoME Shiny app.
#'
#' @format ## ann_run_example
#' A data frame with 6 rows and 3 columns:
#' \describe{
#'  \item{runID}{A column with run IDs in a form of F# (where # is a number). Each ID
#'  has to be unique.}
#'  \item{sampleID}{A column with sample IDs (one for each patient). It consists of
#'  a letter indicating a treatment group and a number indicating the patient
#'  number within the group. E.g. ID A2 stands for the second patient within a
#'  treatment group A. The same sample ID can be assigned to several run IDs.}
#'  \item{rep}{A column with repetition number. E.g. Run F4 is the second
#'  repetition for a patient A2.}
#'
#' }
#' @source <https://raw.githubusercontent.com/TomChupan/data/main/data_example_run.csv>
#'
"ann_run_example"

#' @title Example data with sample annotations
#'
#' @description This dataset contains sample annotations for *data_example* dataset
#' to be used in a proteoME Shiny app.
#'
#' @format ## ann_sample_example
#' A data frame with 3 rows and 5 columns:
#' \describe{
#'  \item{sampleID}{A column with **unique** sample IDs (one for each patient). It consists of
#'  a letter indicating a treatment group and a number indicating the patient
#'  number within the group. E.g. ID A2 stands for the second patient within a
#'  treatment group A.}
#'  \item{treatment}{A column with a treatment group specification (here only
#'  a letter).}
#'  \item{sex}{Male (M) or female (F).}
#'  \item{age}{Age in years.}
#'  \item{weight_kg}{Weight in kg.}
#'
#'
#' }
#' @source <https://raw.githubusercontent.com/TomChupan/data/main/data_example_sample.csv>
#'
"ann_sample_example"
