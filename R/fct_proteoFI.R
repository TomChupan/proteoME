#' proteoFI
#'
#' @description A function for filtering the abundances data (its rows with proteins)
#' based on NA's.
#'
#' @param data_aggreg Data frame with the aggregated abundances on the sample level. First column
#' contains Accession numbers, other columns (named with sampleID's) contains protein abundances of
#' every sample/column.
#'
#' @param ann_sample Data frame with sample annotations in the same form as
#' ann_sample_example (see ?proteoME::ann_sample_example).
#'
#' @param n A number between 1 and 100 that represents the minimum required percentage
#' of samples with a quantified protein. Every protein with relative quantification
#' below this value (depending on the selected method - see 'method') will be removed.
#'
#' @param method It can be one of these 3 values: 1) "all": protein is kept in the dataset
#' when it is quantified in at least 'percent' % of all samples. 2) "onegroup":
#' protein is kept in the dataset when it is quantified in at least 'percent' %
#' of samples within at least one treatment group. 3) "eachgroup":
#' protein is kept in the dataset when it is quantified in at least 'percent' %
#' of samples within each treatment group.
#'
#'
#' @return A data frame with the filtered abundances.
#'
#' @rdname proteoFI
#' @export
#'

proteoFI=function(data_aggreg, ann_sample, n = 30,
                  method=c("all","onegroup","eachgroup")){
  method=match.arg(method)
  if(method=="all"){
    d_agF=data_aggreg[rowMeans(!is.na(data_aggreg[,-1])) >= n/100,]
    return(d_agF)
  }else{
    groups=levels(as.factor(ann_sample$treatment))
    n_levels=length(groups)
    l=list()
    for(j in 1:n_levels){
      id=ann_sample$sampleID[ann_sample$treatment==groups[j]]
      l[[j]]=which(rowMeans(!is.na(data_aggreg[,id])) >= n/100)
    }
    if(method=="onegroup"){
      rows=sort(Reduce(union,l))
    }else{
      rows=sort(Reduce(intersect,l))
    }
    d_agF=data_aggreg[rows,]
    return(d_agF)
  }
}
