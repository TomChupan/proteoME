#' detectionMedians
#'
#' @description An internal function for creating a data frame to add extra info
#' to the heatmap with detections.
#'
#' @param detected_df A data frame (rows=proteins, columns=samples) with the information
#' about number of protein detections within the sample. First column is filled with
#' protein accession numbers. So when there is '2' in the first row and the second column
#' (first non-accession column), it means that the first protein was detected in 2 replicates
#' of the first sample.
#'
#' @param sample_ann A data frame with the sample annotations (in the same format as
#' ann_sample_example) - we need to merge sampleID with a treatment group.
#'
#' @return A required data frame (see description)
#'
#' @noRd

detectionMedians=function(detected_df,sample_ann){
  detmax=max(detected_df[,-1])
  groups=levels(as.factor(sample_ann$treatment))
  n_levels=length(groups)
  det_tab=data.frame(detected=as.character(0:detmax),overall=c(rep(NA,detmax+1)))
  #overall median
  det_tab$overall=sapply(0:detmax, function(value) {
    median(apply(detected_df[, -1], 1, function(x) sum(x == value)))
  })
  #medians by treatment groups
  for(j in 1:n_levels){
    id=sample_ann$sampleID[sample_ann$treatment==groups[j]]
    det_tab[,2+j]=sapply(0:detmax, function(value) {
      median(apply(detected_df[, id], 1, function(x) sum(x == value)))
    })
    names(det_tab)[2+j]=groups[j]
  }
  return(det_tab)
}
