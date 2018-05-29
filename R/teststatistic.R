#' @export
# function for calculation of the test statistic (code chunks taken from http://www.math.uni-duesseldorf.de/~stoch/Software.htm)

# LogRankStatistic

logRankStatistic<- function(w, data, ...){

  # status: censored=0, uncensored=1
  # group: 1.group (0) or 2.group (1)
  # time is not needed since it is a rank test

  status <- data$status
  group <- data$group
  n <- length(status)
  n0 <- sum(group == 0)

  KME <- (1 - c(1, cumprod(1- status/((n+1)-1:n))))[1:n]
  wFn <- mapply(w,KME)

  # calculate observed events in group == 0
  n.events.1 <- status - group
  n.events.1 <- replace(n.events.1, n.events.1==-1, 0)
  n.risk.1 <- c(n-n0, (n-n0) - cumsum(group))[1:n]
  n.risk <- (n+1)-1:n
  n.risk.0 <- n.risk - n.risk.1

  expected <- n.risk.0/n.risk*status
  OME1 <- (n.events.1 - expected)
  # multiply with weights:
  OMEw <- wFn * OME1

  # calcualtion of variance
  V <- status*(n.risk.0/n.risk)*(1-n.risk.0/n.risk)*(n.risk-status)/(n.risk-1)
  V[length(V)] <- 0
  Vw <- sum(wFn^2 * V)

  # test statistic
  Tn <- sum(OMEw)
  if (Vw == 0) stat <- 0
  else stat <- Tn^2/Vw

  output <- list(Tn = Tn, stat = stat, V = V, KME = KME)

  return(output)
}

# Empirical Covariance Matrix

ECM <- function(V, KME, wv,...){
  # ECM: Empirical Covariance Matrix: same code as for denominator above, only with two weight functions

  wFn <- list()
  for(i in 1:length(wv)){
  wFn[[i]] <- mapply(wv[[i]],KME)
  }

  x3 <- matrix(NA, length(wv), length(wv))
  for(i in 1:length(wv)){
    for(j in 1:length(wv)){
      x3[i, j] = x3[j, i] <- sum(wFn[[i]]* wFn[[j]]* V)
    }
  }
  return(x3)
}

teststatistic <- function(data, w, perm = 1:n, ...){
  data$group <- data$group[ perm ] # Permutation approach, for the teststatistic set perm = 1:n
  LRS <- lapply(w, FUN = logRankStatistic, data)
  Tn <- unlist(lapply(LRS, "[[", "Tn")) # vector Tn
  # extract Variances and F_hat for calculation of covariance matrix
  V <- lapply(LRS, "[[", "V")[[1]]
  KME <- lapply(LRS, "[[", "KME")[[1]]
  sigma_hat <- ECM(V, KME, w) # Sigma (covariance matrix)

  statistic <- t(Tn) %*% MASS::ginv(sigma_hat) %*% Tn     # Tn Sigma^-1 Tn
  return( c(statistic) )
}
