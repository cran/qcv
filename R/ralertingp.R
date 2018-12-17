#' Inferential test for ralerting-CV
#'
#' Computes a p value for the ralerting-CV effect size
#' @param actr vector of actual validity correlations
#' @param predr vector of predicted validity correlations (in same order as actr)
#' @param iter the number of randomization samples to be generated
#' @export
#' @details
#' An inferential test for ralerting-CV, not presented in the original Westen and Rosenthal (2003).
#' The inferential test is based upon randomization procedures.
#' That is, given a set of predicted correlations and a set of actual correlations,
#' ralertingp() randomly pairs values from the two sets, computes an ralerting-CV
#' value, and repeats this process many times (as determined by the user).
#' The actual ralerting-CV value is then compared to the distribution of
#' ralerting-CV values derived from the randomization process.
#' The proportion of values from that distribution that are greater than the
#' actual ralerting-CV value is then taken as a p value.
#'
#' Important: When entering values for the "actr" and "predr" arguments, order them identically. For example, if
#' the "actr" values are ordered in terms of c(criterion A, criterion B, criterion C), then the values in "predr" should
#' be placed in that same order. Otherwise results will be incorrect.
#'
#' For relatively small values of k (the number of criterion variables), it is recommended to increase iter (the number of randomization samples.
#' For low-k situations, a small number of randomization samples can produce unstable p values.
#' A larger number of randomization samples would produce more stable results.
#'
#' It is possible that the randomization process produces no values that are greater than the actual ralerting-CV value. In such
#' cases, ralertingp() reports the p value as < 1/iter.
#'
#' @examples
#' actrIM  <- c(.46, .13, -.24, -.03, .12, .03, .39, .06, .51, .08, .24, .66)
#' predrIM <- c(.58, .24, -.04, .06, -.04, .18, .36, .08, .64, .56, .36, .56)
#' ralertingp(actr=actrIM, predr=predrIM, iter=1000)

ralertingp <- function(actr, predr, iter) {
  lambda <- predr-mean(predr)         # convert predicted correlations into contrast weights (lambdas)
  zactr <- .5*log((1+actr)/(1-actr))  # z-transform the actual correlations (Zr)
  ralerting <- cor(zactr, lambda)     # ralerting-CV
  ralertingrand <- function() {
    Lrand <- sample(lambda)           # randomize the predicted (and contrast-weighted) correlations
    ralertingr <- cor(zactr, Lrand)   # compute ralerting based on randomized predictions
    ralertingr
  }
  ralertingrandout <- replicate(n=iter,ralertingrand())
  compare <- ralertingrandout < ralerting
  ptable <- prop.table(table(compare))
  temp <- 1-ptable[2]
  ralertingp <- temp[[1]]
  if(is.na(ralertingp) == TRUE){
    ralertingp <- paste("<",1/iter)
  }
  cat(paste("ralerting-CV:", round(ralerting, 4)),"\n")
  cat(paste0("randomization p value for ralerting-CV: ", ralertingp))
}

