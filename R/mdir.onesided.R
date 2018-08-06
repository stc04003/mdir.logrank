#' Two-sample multiple-direction log rank test for stochastic ordered alternatives
#'
#' The mdir.onesided function calculates the multiple-direction logrank
#' statistic for (one-sided) stochastic ordered alternatives and
#' its p-value  based on a wild bootstrap approach
#'
#' @param data A data.frame, list or environment containing the variables \code{time},
#'   \code{event} (with values 0 for censored and 1 for uncensored) and \code{group}.
#' @param rg A list containing the exponents \code{c(r, g)} of the directions
#'   \eqn{w(x) = x^r (1-x)^g}. Both exponents r,g need to be natural numbers including 0.
#'  Default is \code{list(c(0, 0), c(0, 4), c(4, 0))} corresponding to the choice of the
#'  proportional, early and late direction/weight.
#' @param group1 The name or the coding for the first group in the data set
#' @param wild The wild bootstrap approach used for estimating the p-value. The Rademacher
#'   (\code{rade}, default), the normal distribution (\code{norm}) or the centred
#'   Poisson distribution (\code{pois}) approach can be selected.
#' @param iter The number of iteration used for calculating the wild bootstrap p-value.
#'   The default option is 10000.
#' @param dig_p The p-values are rounded to \code{dig_p} digits, the default is 3.
#' @param dig_stat The test statistic is rounded to \code{dig_stat} digits, the default is 3.
#'
#' @details The function provides the multiple-direction logrank statistic for
#'   the two sample one-sided testing problem of stochastic ordering within right-censored survival data.
#'   The null hypothesis \eqn{H:F1=F2} is tested against the one-sided alternative \eqn{K:F1 \ge F2,
#'   F1 \e F2}. The first group corresponding to \eqn{F1} can be specified
#'   by the argument \code{group1}. An arbitrary amount of directions/weights of the form
#'   \eqn{w(x) = x^r * (1-x)^g} for natural numbers r,g (including 0) can be chosen in the list
#'   \code{rg}. The multiple-direction onsided logrank test needs linearly independent directions.
#'   A check for this is implemented. If the directions chosen by the user are
#'   linearly dependent then a subset consisting of linearly independent directions
#'   is selected automatically.
#'
#'   The \code{mdir.logrank} function returns the test statistic and the p-value
#'   based on a wild bootstrap procedure \code{wild}.
#'
#'
#' @return An  \code{mdirone} object containing the following components:
#' \item{Descriptive}{The directions used and whether the directions specified by the user were
#'    linearly independent.}
#'  \item{p.value}{The p-value of the one-sided multiple-direction logrank test using the
#'    the using the permutation approach (Perm.).}
#'  \item{wild}{The wild bootstrap approach which was used}
#'  \item{stat}{Value of the one-sided multiple-direction logrank statistic.}
#'  \item{rg}{A list containing the exponents of the direction considered in the statistical analysis.}
#'  \item{group1}{The name of the first group.}
#'  \item{indep}{logical. Were the directions specified by the user linearly independent?}
#'  \item{iter}{The number of iterations used for calculating the wild bootstrap p-value.}
#' @examples
#' library(coin)
#' data(GTSG)
#' out <- mdir.logrank(data = GTSG, group1 = "Chemotherapy+Radiation")
#'
#' ## Detailed information:
#' summary(out)
#'
#' @references Ditzhaus, M., Pauly, M. (2018). ????
#'
#' @seealso [mdir.logrank()] (two-sided test)
#'
#' @importFrom stats runif
#' @importFrom utils read.table
#'
#' @export

mdir.onesided <- function(data, rg = list( c(0, 0), c(0, 4), c(4, 0) ), group1, wild = "rade", iter = 10000, dig_p = 3, dig_stat = 3 ){
  if( sum(c("time","group","event") %in% names(data)) != 3){
    stop("The data does not contain all three variables group, event, time.")
  }
  data <- data.frame( time = data$time, status = data$event, group = data$group)

  # breaking ties
  dist <- runif(length(data$time))*10^-5
  data$time <- sum( c(data$time, dist) )

  data <- data[ order(data$time), ]
  data$group <- unlist( lapply( data$group, function(x){ paste0(x)}))
  group1_p <- paste0(group1)
  lev_group <- levels( as.factor(data$group))
  if ( length(lev_group) != 2){
    stop("There are either more or less than 2 groups")
  }
  if ( !(group1_p %in% data$group) ){
    stop("The first group specified by group1 is not a possible group. ")
  }

  group1_lev <- ifelse( group1_p == lev_group[1], 1, 2 )
  data$group <- ifelse( data$group == lev_group[group1_lev], 0, 1)
   # Checking wheter the directions were chosen appropriately
  rg_un <- unlist( rg )
  if ( all( is.numeric( rg_un )) == FALSE ){
      stop("The exponents r,g contained in rg need to be numeric")
  }
  if ( all( rg_un == floor( rg_un)) == FALSE ){
        stop("The exponents r,g contained in rg need to be natural numbers including 0")
  }
  for (i in 1:length(rg)){
    if (length( rg[[i]] ) != 2){
      stop("rg must be NULL or a list containing vectors of the form c(r, g), where r and g are natural numbers (including 0)!")
    }
    if ( any( rg[[i]] < 0)){
          stop("The exponents specified in the list rg need to be nonnegative!")
      }
  }
  out <- coeff.check(cross = FALSE, rg = rg) # eleminating linearly dependent directions
  indep <- out$indep
  rg <- out$rg

  if ( !(wild %in% c("rade", "norm", "pois"))){
    stop("wild is not specified correctly.")
  }

  ## end of direction check --------------------------------------------------------
  w <- list()
  w.funct <- function( x ){
    x <- unlist(x)
    w <- function(y){ y^x[1] * (1-y)^x[2] }
  }
  match.fun( w.funct )
  w <- lapply( rg, w.funct )
  n <- length(data$group)
  stat <- teststat_max( data, w, G = 1)
  if ( wild == "rade"){
    stat_boot <- replicate(iter, teststat_max(data = data, w = w,  G = sample( c(-1,1), n, replace = TRUE) ))
  }
  if ( wild == "norm"){
    stat_boot <- replicate(iter, teststat_max(data = data, w = w, G = stats::rnorm(n) ))
  }
  if ( wild == "pois"){
    stat_boot <- replicate(iter, teststat_max(data = data, w = w, G = stats::rpois(n,1)-1))
  }
  p_value <- round( mean(  stat_boot > stat ), digits = dig_p)

  # Output --------------------------------------------------------------------------
  dir <- list( rg = rg, indep = indep)
  output <- list( stat = round(stat, digits = dig_stat), wild = wild, group1 = group1, p_value = p_value, rg = rg, indep = indep, iter = iter)
  class(output) <- "mdirone"
  return(output)
}

