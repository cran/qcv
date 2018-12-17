#' Printing Quantifying Construct Validity
#'
#' Prints key results from the qcv() function
#' @param x object of class "qcv" (i.e., output from the qcv() function
#' @param ... More arguments to pass to the print function.
#' @export
#' @examples
#' actrIM  <- c(.46, .13, -.24, -.03, .12, .03, .39, .06, .51, .08, .24, .66)
#' predrIM <- c(.58, .24, -.04, .06, -.04, .18, .36, .08, .64, .56, .36, .56)
#' imqcvout <- qcv(n=90, actr=actrIM, predr=predrIM, medr=.075)
#' print(imqcvout)

print.qcv <- function (x, ...)
{
  if (class(x) != "qcv") {
    stop("x must be of class 'qcv'")
  }
  cat(paste("************  QCV output  *******************************", "\n"))
  cat(paste("*********************************************************", "\n"))
  cat("\n")
  cat(paste("ralerting-CV:", round(x$ralerting, 4)),"\n")
  cat(paste("rcontrast-CV:", round(x$rcontrast, 4 ), "        95% CI =",round(x$rcontrastCIlower, 4), " to ", round(x$rcontrastCIhigher, 4)),"\n")
  cat(paste("zcontrast:   ", round(x$zcontrast, 4)),"\n")
  cat(paste("p:           ", x$p),"\n")
  cat(paste("N:           ", x$N),"\n")
  cat(paste("k:           ", x$k),"\n")
  cat(paste("szr:         ", round(x$szr, 4)),"\n")
  cat(paste("medr:        ", round(x$medr, 4)),"\n")
  cat(paste("rbarsq:      ", round(x$rbarsq,4)),"\n")
  cat(paste("t:           ", round(x$t, 4)),"\n")
  cat(paste("remark:      ", round(x$rem, 4)),"\n")
  if(x$padj == "Y"){
  cat("NOTE: due to an extremely large zcontrast value, the p value is a lower-bound estimate of the true p,\n thus the t, the rcontrast-CV value, and the rcontrast-CV CI values - are approximate")
  }
}
