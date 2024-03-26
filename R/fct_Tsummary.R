#' Tsummary
#'
#' @description An individual summary function to be used in a 'Transformation' tab
#' in the proteoME app. The input is a vector (or a column from a data frame).
#'
#' @return A data frame with selected summary statistics.
#'
#' @noRd
#' @importFrom DescTools Skew
Tsummary=function(x){
  s=data.frame(min=min(x,na.rm = T),q25=quantile(x,0.25,na.rm = T),
               median=median(x,na.rm = T),mean=mean(x,na.rm = T),
               q75=quantile(x,0.75,na.rm = T),max=max(x,na.rm = T),
               range=range(x,na.rm = T)[2]-range(x,na.rm = T)[1],
               skewness=DescTools::Skew(x,na.rm = T),row.names = "")
  s
}
