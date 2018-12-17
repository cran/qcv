#' Quantifying Construct Validity
#'
#' Computes key indices related to the Quantifyting Construct Validity (QCV) procedure (Westen & Rosenthal, 2003; see also Furr & Heuckeroth, in prep.)
#' @param n sample size
#' @param actr vector of actual validity correlations
#' @param predr vector of predicted validity correlations (in same order as actr)
#' @param medr median intercorrelation among criterion variables
#' @importFrom stats pnorm qt sd
#' @details
#' This function applies procedures outlined by Westen and Rosenthal (2003; see also Furr & Heuckeroth, in prep).
#'
#' Important: When entering values for the "actr" and "predr" arguments, order them identically. For example, if
#' the "actr" values are ordered in terms of c(criterion A, criterion B, criterion C), then the values in "predr" should
#' be placed in that same order. Otherwise all results will be incorrect.
#'
#' Note that extreme zcontrast values (e.g., z > 38) create problems. The rcontrast-CV effect size is computed
#' by converting z to p to t to r (see Westen & Rosenthal, Appendix A & B). However, R (and most other packages)
#' doesn't have precision to convert an extremely large z (e.g., Z = 38) to a p value. For such z values, the p is converted to exactly
#' zero. This makes it impossible to obtain t and rcontrast values. The qcv() function handles this by:
#' a) identifying cases where p is initially exactly zero, and b) re-setting p to the smallest value possible, given the
#' machine on which R us running (usually  2.225074e-308). It then proceeds to compute a t and rcontrast from that adjusted p value. It also prints a note
#' stating that the adjustment has been made and that the p, t, and rcontrast values are lower-bound approximations.
#' @references Westen, D., & Rosenthal, R. (2003). Quantifying construct validity: Two simple measures. Journal of Personality and Social Psychology, 84, 608-618.
#' @references Furr, R. M., & Heukeroth, S. (In prep).Advancing the Interpretation and Computation of the "Quantifying Construct Validity" Procedure
#' @return
#' \describe{
#'   \item{ralerting-CV}{The ralerting-CV effect size}
#'   \item{rcontrast-CV}{The rcontrast-CV effect size}
#'   \item{zcontrast}{The zcontrast value}
#'   \item{p}{p values associated with Zcontrast}
#'      \item{N}{Sample size}
#'         \item{k}{Number of criterion variables}
#'   \item{szr}{Standard deviation of (z-transformed) actual correlations}
#'   \item{medr}{Median intercorrelation between the critertion variables}
#'   \item{rbarsq}{Mean of the squared actual correlations (between focal test and criterion variables) }
#'   \item{t}{t-value associated with p value}
#'   \item{rem}{"Remarkablness" of size of contrast}
#' }
#' @return Additional values are returned in a list, but not printed
#' @export
#' @examples
#' actrIM  <- c(.46, .13, -.24, -.03, .12, .03, .39, .06, .51, .08, .24, .66)
#' predrIM <- c(.58, .24, -.04, .06, -.04, .18, .36, .08, .64, .56, .36, .56)
#' qcv(n=90, actr=actrIM, predr=predrIM, medr=.075)

qcv <- function(n, actr, predr, medr) {

  zactr <- .5*log((1+actr)/(1-actr))  # z-transform the actual correlations (Zr)
  rbarsq <- mean(actr^2)              # mean of squared actual correlations
  lambda <- predr-mean(predr)         # convert predicted correlations into contrast weights (lambdas)
  ralerting <- cor(zactr, lambda)     # ralerting-CV

  # To compute rcontrast -------------------------------------------------------------
  sumlambdasq <- sum(lambda^2)        # sum of squared lambdas
  sumLzr <- sum(lambda*zactr)         # sum of cross-producs of Zr and lambda values
  f1 <- (1-medr)/(2*(1-rbarsq))
  if(f1 <= 1) f<-f1 else f<-1         # per W&R (2003 p 617)
  h <- (1-f*rbarsq)/(1-rbarsq)
  zcontrast <- sumLzr*sqrt( (n-3) / (sumlambdasq*(1-medr)*h)   )
  # consistent with Eq2 in F&H
  # zcontrast <- sumLzr*sqrt( (N-3) / (sumlambdasq*(1-medr)* ( (1-rbarsq*( (1-medr)/(2*(1-rbarsq)) )) /(1-rbarsq)   )))
  p <- pnorm(-abs(zcontrast))                 # convert z to p
  ifelse(p == 0, padj <- "Y", padj <- "N")    # is the 'extreme z' solution (see next line) applied"
  if(p < .Machine$double.xmin){
    p <- .Machine$double.xmin                 # clumsy solution to 'extreme z' problem (ie z > about 37.5 produces p = 0)
  }
  t <- qt(p, df=n-2, lower.tail=FALSE)        # convrt p to t
  rcontrast <- sqrt(t*t/(t*t+n-2))            # convert to to rcontrast=CV
  rcontrast <- sign(ralerting)*rcontrast      # make sure the sign of rcontrast-CV is correct

  # CI for rcontrast -----------------------------------------------------------------
  zrcontrast <- .5*log((1+rcontrast)/(1-rcontrast))
  zrclow <- zrcontrast - 1.96/sqrt(n-3)
  zrchigh <- zrcontrast + 1.96/sqrt(n-3)
  rcontrastCIlower  <- (exp(2*zrclow)-1)/(exp(2*zrclow)+1)
  rcontrastCIhigher <- (exp(2*zrchigh)-1)/(exp(2*zrchigh)+1)

  # additional values of interest (eg as related to Furr & Heuckeroth) ---------------
  k <- length(actr)
  g <- (1-medr)/(1-rbarsq)
  szr <- sd(zactr)*(sqrt((k-1)/k))                  # to get 'population' SD of the Zr values
  rem <- sumLzr / sqrt(sumlambdasq*(1-medr)*h)
  rem <- sqrt(k)*szr*ralerting / sqrt(g*(1-g*rbarsq/2))
  qcvout <- list("ralerting"=ralerting, "rcontrast"=rcontrast, "rcontrastCIlower"=rcontrastCIlower,  "rcontrastCIhigher"=rcontrastCIhigher,
                 "zcontrast"=zcontrast, "p"=p, "padj"=padj, "N"=n, "k"=k, "szr"=szr, "t"=t, "rbarsq"=rbarsq, "medr"=medr, "f"=f, "h"= h, "g"=g, "remarkableness"=rem)
  class(qcvout) <- c("qcv")
  return(qcvout)
}


