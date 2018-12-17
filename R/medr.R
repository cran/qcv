#' Median r
#'
#' Computes the median intercorrelation among a set of variables
#' @param df data frame consisting only of variables for which a median intercorrelation is desired
#' @importFrom stats cor median
#' @details
#' The data frame should contain only the variables for which a median intercorrelation is desired. For Westen and Rosenthal's (2003) QCV
#' procedure, the data frame should contain only participants' scores on the criterion variables.
#'
#' This prcoedure handles missing data via the "pairwise.complete.obs" option.
#' @export
#' @examples
#' data(motdat)
#' motdatc <- motdat[,2:13] #To retain only the criterion variables (dropping the focal test score)
#' medr(motdatc)

medr <- function(df) {
  critcorrs <- cor(df, use="pairwise.complete.obs")
  critcorrs[lower.tri(critcorrs, diag=TRUE)] <- NA
  medr <- median(critcorrs, na.rm=TRUE)
  qcvout <- list("criterion intercorrelations"=critcorrs, "median intercorrelation (medr)"=medr)
  return(qcvout)
}
