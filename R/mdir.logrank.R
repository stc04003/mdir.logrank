#' Two-sample multiple-direction log rank test
#'
#' The mdir.logrank function calculates the p-values of the multiple-direction logrank test based
#' on the \eqn{\chi^2}-approximation and the permutation approach
#'
#' @param data A data.frame, list or environment containing the variables \code{time},
#'   \code{event} (with values 0 for censored and 1 for uncensored) and \time{group}.
#' @param cross logical. Should the weight correspondng to crossing hazards be included?
#'  The default is \code{TRUE}.
#' @param rg A list (or \code{NULL}) containing the exponents \code{c(r, g)} of the directions
#'   \eqn{w(x) = x^r (1-x)^g}. Both exponents need to be natural numbers including 0.
#'  Default is \code{list( c(0, 0) )} corresponding to proportional hazards.
#' @param nperm The number of permutations used for calculating the permuted p-value.
#'   The default option is 10000.
#' @param dig_p The p-values are rounded to dig_p digits, the default is \code{dig_p=3}.
#' @param dig_stat The test statistic is rounded to dig_stat digits, the default is \code{dig_stat=3}.
#'
#' @details The package provides the multiple-direction logrank statistic for
#'   the two sample testing problem withing right-censored survival data. Directions
#'   of the form w(x) = 1 - 2x (\code{cross = TRUE}) and w(x) = x^r * (1-x)^g for natural numbers
#'   r,g (including 0) can be specified.
#'   The multiple-direction logrank test needs linearly independent directions.
#'   A check for this is implement. If the directions chosen by the user are
#'   linearly independent then a subset consisting of linearly independent directions
#'   is selected automatically.
#'
#'   The \code{mdir.logrank} function returns the test statistic as well as two
#'   corresponding p-values: the first is based on a \eqn{chi^2} approximation and
#'   the second one is based on a permutation procedure.
#'
#' @return A \code{mdir.logrank} object containing the following components:
#' \item{Descriptive}{The directions used and whether the directions specified by the user were
#'    were linearly independent}
#'  \item{p.values}{The p-values of the multiple-direction logrank test using the
#'    \eqn{\chi^2}-approximation (Approx.) as well as the one using the permutation approach (Perm.)}
#'  \item{stat}{Value of the multiple-direction logrank statistic}
#'  \item{rg}{A list containg the exponents of the direction considered in the statistical analysis
#'  \item{cross}{logical. Was the crossing direction considered in the statistical analysis}
#'  \item{indep}{logical. Were the directions specified by the user linearly independent?}
#'  \item{nperm}{The number of permutations used for calculating the permuted p-value.
#' @examples
#' library(coin)
#' data(GTSG)
#' out <- mdir.logrank(data = GTSG)
#'
#' ## Detailed informations:
#' summary(out)
#'
#' @references Ditzhaus, M., Friedrich, S. (2018). Titel und so (Theory)
#'
#' Ditzhaus, M., Friedrich, S. (2018). Titel und so (practical paper)
#'
#' @importFrom stats pchisq
#' @importFrom utils read.table
#'
#' @export

mdir.logrank <- function(data, cross = TRUE, rg = list( c(0,0) ), nperm = 10000, dig_p = 3, dig_stat = 3 ){
  data <- data.frame( time = data$time, status = data$event, group = data$group)
  data <- data[ order(data$time), ]
  data$group <- unlist( lapply( data$group, function(x){ paste0( "group ", x)}))
  lev_group <- levels( as.factor(data$group))
  if ( length(lev_group) != 2){
    stop("There are either more or less than 2 groups")
  }
  data$group <- ifelse( data$group == lev_group[1], 0, 1)
   # Checking wheter the directions were chosen appropriately
  if( is.logical(cross) == FALSE){
    stop("cross needs to be logical")
  }
  rg_un <- unlist( rg )
  if ( is.null(rg) == FALSE){
      if ( all( is.numeric( rg_un )) == FALSE ){
        stop("The exponents r,g containing in rg need to be numeric")
      }
      if ( all( rg_un == floor( rg_un)) == FALSE ){
        stop("The exponents r,g containing in rg need to be natural numbers including 0")
      }
      for (i in 1:length(rg)){
        if (length( rg[[i]] ) != 2){
          stop("rg must be NULL or a list containing vectors of the form c(r, g), where r and g are natural numbers (including 0)!")
        }
        if ( any( rg[[i]] < 0)){
          stop("The exponents specified in the list rg need to be nonnegative!")
        }
      }
  }else{
    if( cross != TRUE){
      stop('Specify at least one hazard direction by setting cross = TRUE or defining a nonempty list rg ')
    }
  }
  if ( is.null(rg) == FALSE){
    out <- coeff.check(cross = cross, rg = rg) # eleminating linearly dependent directions
    cross <- out$cross
    indep <- out$indep
    rg <- out$rg
  }else{
    indep <- TRUE
  }


  ## end of direction check --------------------------------------------------------
  w <- list()
  w.funct <- function( x ){
    x <- unlist(x)
    w <- function(y){ y^x[1] * (1-y)^x[2] }
  }
  match.fun( w.funct )
  w <- lapply( rg, w.funct )
  if ( cross == TRUE ){
    w <- c( w, function(x){ 1-2*x })
  }
  n <- length(data$group)
  stat <- teststatistic( data, w, perm = 1:n)
  stat_perm <- replicate( nperm, teststatistic(data, w, perm = sample(n)) )
  p_value_chi2 <- round( stats::pchisq( stat, df = length(w), lower.tail = FALSE), digits = dig_p)
  p_value_perm <- round( mean(  stat_perm > stat ), digits = dig_p)
  p_value <- list( Approx = p_value_chi2, Perm = p_value_perm)

  # Output --------------------------------------------------------------------------
  p_value <- list( Approx = p_value_chi2, Perm = p_value_perm)
  dir <- list( rg = rg, cross = cross, indep = indep)
  output <- list( stat = round(stat, digits = dig_stat), p_value = p_value, rg = rg, cross = cross, indep = indep, nperm = nperm)
  class(output) <- "mdirLR"
  return(output)
}

