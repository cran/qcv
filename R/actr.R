#' Actual correlations
#'
#' A very simple function, actr() computes the actual correlations between a focal test and a set of criterion variables
#' @param df data frame consisting of focal test and a set of criterion variables
#' @importFrom stats cor
#' @details
#' The df should contain only scores on the focal test and on the criterion variables. Any additional columns should be deleted.
#' Focal test scores should be in the first column. If you need to reorder columns, you can do so by column number - eg, df <- df[c(1,3,2)] -
#' or by column name - eg, df <- df[c("test", "crit1", "crit2")]
#'
#' This function uses the "pairwise.complete.obs" option, which handles missing data by pairwise deletion.
#' @export
#' @examples
#' data(motdat)
#' actr(motdat)

actr <- function(df) {
  actrout <- cor(df[,1], df[,-1], use="pairwise.complete.obs")
  return(actrout)
}

