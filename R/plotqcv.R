#' Plot for Quantifying Construct Validity
#'
#' Produces a plot of actual and predicted correlations
#' @param actr vector of actual validity correlations
#' @param predr vector of predicted validity correlations (in same order as actr)
#' @param labels vector of text values that are the labels of the QCV criterion variables (in order of actr and predr)
#' @importFrom graphics axis legend lines lines plot
#' @details
#' Important: When entering values for the three arguments, order them identically. For example, if
#' the "actr" values are ordered in terms of c(criterion A, criterion B, criterion C), then the values in "predr" and the labels in "labels"
#' should be placed in that same order.
#' @export
#' @examples
#' actrIM  <- c(.46, .13, -.24, -.03, .12, .03, .39, .06, .51, .08, .24, .66)
#' predrIM <- c(.58, .24, -.04, .06, -.04, .18, .36, .08, .64, .56, .36, .56)
#' labelsIM <- c("Dep","Mach","Dis","Res","SE","Ext","Agr","Comp","PSC","SM","Anx","NTB")
#' plotqcv(actr=actrIM, predr=predrIM, labels=labelsIM)


plotqcv <- function(actr, predr, labels) {
  NumberLabels <- seq(1, length(labels),1)
  plot(actr, type="o", ylim =c(-1,1),  xlab = "Criterion Variables", ylab= "Correlation", xaxt="n", lty=1, pch=20)
  axis(1, at=NumberLabels, labels = labels)
  lines (predr, type = "o", lty=2, pch=20)
  legend(x=0, y = 1.5, xpd=TRUE, horiz=TRUE, legend = c("Actual correlations", "Predicted Correlations"), lty=c(1,2), bty="n")
}

