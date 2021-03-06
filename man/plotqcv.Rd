% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotqcv.R
\name{plotqcv}
\alias{plotqcv}
\title{Plot for Quantifying Construct Validity}
\usage{
plotqcv(actr, predr, labels)
}
\arguments{
\item{actr}{vector of actual validity correlations}

\item{predr}{vector of predicted validity correlations (in same order as actr)}

\item{labels}{vector of text values that are the labels of the QCV criterion variables (in order of actr and predr)}
}
\description{
Produces a plot of actual and predicted correlations
}
\details{
Important: When entering values for the three arguments, order them identically. For example, if
the "actr" values are ordered in terms of c(criterion A, criterion B, criterion C), then the values in "predr" and the labels in "labels"
should be placed in that same order.
}
\examples{
actrIM  <- c(.46, .13, -.24, -.03, .12, .03, .39, .06, .51, .08, .24, .66)
predrIM <- c(.58, .24, -.04, .06, -.04, .18, .36, .08, .64, .56, .36, .56)
labelsIM <- c("Dep","Mach","Dis","Res","SE","Ext","Agr","Comp","PSC","SM","Anx","NTB")
plotqcv(actr=actrIM, predr=predrIM, labels=labelsIM)
}
