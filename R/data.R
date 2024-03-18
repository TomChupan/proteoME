#' @title Example data with protein abundances
#'
#' @description This dataset contains sample data with protein abundances to be used in a
#' proteoME Shiny app.
#'
#' @format
#' A data frame with 2018 rows and 101 columns:
#' \describe{
#' \itemize{
#'  \item{Accession: }{An UniProt accession number – a unique identifier assigned
#'  to a protein entry in the UniProt Knowledgebase (UniProtKB).
#'  See \url{https://www.uniprot.org/} }
#'  \item{F#: }{Columns with protein abundances for each run
#'  (several runs may come from one patient – replicates)}
#'  \itemize{
#'  \item Abundances can be raw, imputed or normalized (or both imputed and
#'   normalized)
#'   }
#'  }
#'
#' }
#' @source <https://raw.githubusercontent.com/TomChupan/data/main/data_example3.csv>
#'
"data_example"

#' @title Example data with run annotations
#'
#' @description This dataset contains run annotations for *data_example* dataset
#' to be used in a proteoME Shiny app.
#'
#' @format
#' A data frame with 100 rows and 6 columns:
#' \describe{
#'  \item{runID}{A column with run IDs in a form of F# (where # is a number). Each ID
#'  has to be unique.}
#'  \item{sampleID}{A column with sample IDs (one for each patient). It consists of
#'  a letter indicating a treatment group and a number indicating the patient
#'  number within the group. E.g. ID A2 stands for the second patient within a
#'  treatment group A. The same sample ID can be assigned to several run IDs.}
#'  \item{rep}{A column with replicate number. E.g. Run F4 is the second
#'  replicate for a patient A2.}
#'  \item{batchNo}{Batch number.}
#'  \item{batchOrder}{Order of the run within the batch.}
#'  \item{dateTime}{Date and time of the measurement in a \%m/\%d/\%Y \%R format.}
#'
#' }
#' @source <https://raw.githubusercontent.com/TomChupan/data/main/data_example_run3.csv>
#'
"ann_run_example"

#' @title Example data with sample annotations
#'
#' @description This dataset contains sample annotations for *data_example* dataset
#' to be used in a proteoME Shiny app.
#'
#' @format
#' A data frame with 50 rows and 5 columns:
#' \describe{
#'  \item{sampleID: }{A column with **unique** sample IDs (one for each patient). It consists of
#'  a letter indicating a treatment group and a number indicating the patient
#'  number within the group. E.g. ID A2 stands for the second patient within a
#'  treatment group A.}
#'  \item{treatment: }{A column with a treatment group specification.}
#'  \item{sex: }{Male (M) or female (F).}
#'  \item{age: }{Age in years.}
#'  \item{weight_kg: }{Weight in kg.}
#'
#'
#' }
#' @source <https://raw.githubusercontent.com/TomChupan/data/main/data_example_sample2.csv>
#'
"ann_sample_example"
