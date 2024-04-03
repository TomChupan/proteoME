#' proteoAG
#'
#' @description A function for aggregating the abundances data from the 'run level' to
#' the 'sample level'.
#'
#' @param data_run_pivotlonger Data with run abundances in a 'pivot longer' format.
#' The way you should get this format from the original format (see ?proteoME::data_example)
#' is shown in the examples. Stick with the presented column names!
#'
#' @param method Aggregation method. You can choose between 'mean' and 'median'.
#'
#' @param percent A number between 1 and 100 that represents the minimum required percentage
#' of quantified runs within a sample for aggregatation to take place for that
#' sample (returns NA if not met).
#'
#'
#' @examples
#' # Data to pivot longer format
#' library(dplyr)
#' pivot_longer=data_example %>%
#' tidyr::pivot_longer(!Accession,names_to = "runID",values_to = "abundances")
#' d$index=1:nrow(d)
#' d=merge(d,ann_run_example[,c(1:2)],by="runID")
#' d=merge(d,ann_sample_example[,c(1:2)],by="sampleID")
#' #for the same order in other outputs (tables, charts...)
#' d=d[order(d$index),]
#' d$runID=factor(d$runID,levels=names(data_example)[-1])
#' d$sampleID=factor(d$sampleID,levels=ann_sample_example$sampleID)
#'
#' proteoAG(data,method="mean",percent=66)
#'
#' @return A data frame with the aggregated abundances on the sample level.
#'
#' @rdname proteoAG
#' @export
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>%

proteoAG=function(data_run_pivotlonger,method=c("mean","median"),percent=50){
  method=match.arg(method)
  d_ag=aggregate(abundances ~ Accession + sampleID,
                 data = data_run_pivotlonger,
                 FUN = function(x){
                   if(sum(!is.na(x)) >= percent/100 * length(x)) {
                     return(ifelse(method=="mean",mean(x, na.rm = TRUE),
                                   median(x, na.rm = TRUE))
                     )
                   } else {
                     return(NA)
                   }
                 },
                 na.action = na.pass)
  d_ag=d_ag %>%
    tidyr::pivot_wider(names_from = "sampleID",values_from = "abundances",
                       names_sort = F)

  return(d_ag)
}
