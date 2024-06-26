% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_proteoAG.R
\name{proteoAG}
\alias{proteoAG}
\title{proteoAG}
\usage{
proteoAG(data_run_pivotlonger, method = c("mean", "median"), percent = 50)
}
\arguments{
\item{data_run_pivotlonger}{Data with run abundances in a 'pivot longer' format.
The way you should get this format from the original format (see ?proteoME::data_example)
is shown in the examples. Stick with the presented column names!}

\item{method}{Aggregation method. You can choose between 'mean' and 'median'.}

\item{percent}{A number between 1 and 100 that represents the minimum required percentage
of quantified runs within a sample for aggregation to take place for that
sample (returns NA if not met).}
}
\value{
A data frame with the aggregated abundances on the sample level.
}
\description{
A function for aggregating the abundances data from the 'run level' to
the 'sample level'.
}
\examples{
# Data to pivot longer format
library(dplyr)
d=data_example \%>\%
tidyr::pivot_longer(!Accession,names_to = "runID",values_to = "abundances")
d$index=1:nrow(d)
d=merge(d,ann_run_example[,c(1:2)],by="runID")
d=merge(d,ann_sample_example[,c(1:2)],by="sampleID")
#for the same order in other outputs (tables, charts...)
d=d[order(d$index),]
d$runID=factor(d$runID,levels=names(data_example)[-1])
d$sampleID=factor(d$sampleID,levels=ann_sample_example$sampleID)

proteoAG(d,method="mean",percent=66)

}
