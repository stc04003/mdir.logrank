# function for calculation of the test statistic

# LogRankStatistic

logRankStatistic<- function(w, data, G = 1, ...){

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
  OMEw <- G * wFn * OME1

  # calcualtion of variance
  V <- G^2*status*(n.risk.0/n.risk)*(1-n.risk.0/n.risk)*(n.risk-status)/(n.risk-1)
  V[length(V)] <- 0
  Vw <- sum(wFn^2 * V)

  # test statistic
  Tn <- sum(OMEw)

  output <- list(Tn = Tn, V = V, KME = KME)

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


# Statistic for the two-sided testing problem
teststatistic <- function(data, w, perm, ...){
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

# Statistic for the one-sided testing problem
teststat <- function(x, pre_statistic, Tn ){
  statistic <- ifelse( any(pre_statistic[x] < 0), 0, t(Tn[x]) %*% pre_statistic[x])    # Tn Sigma^-1 Tn or 0
  return( c(statistic) )
}

pset <- function(n){
  l <- vector(mode="list",length=2^n) ; l[[1]]=numeric()
  counter <- 1
  for(x in 1:n){
    for(subset in 1:counter){
      counter=counter+1
      l[[counter]] <- c(l[[subset]], x)
    }
  }
  return(l[2:2^n])
}

teststat_max <- function(data, w,  G = 1, ...){
  LRS <- lapply(w, FUN = logRankStatistic, data = data, G = G)
  Tn <- unlist(lapply(LRS, "[[", "Tn")) # vector Tn
  # extract Variances and F_hat for calculation of covariance matrix
  V <- lapply(LRS, "[[", "V")[[1]]
  KME <- lapply(LRS, "[[", "KME")[[1]]
  sigma_hat <- ECM(V, KME, w) # Sigma (covariance matrix)
  pre_statistic <- MASS::ginv(sigma_hat) %*% Tn     #  Sigma^-1 Tn
  powerset <- pset( length(w) )
  stat <- max( unlist( lapply(powerset, teststat, pre_statistic = pre_statistic, Tn = Tn) ))
}

# Boost for the Rademacher approach. Here, the bootstrap variance estimator
# is independent of the multiplier G's since G_i^2=1.
# Hence, it coincides with the (unconditional) variance estimator

logRankStatistic_rade <- function(w, data, G, ...){

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
  OMEw <- G * wFn * OME1

  # test statistic
  Tn <- sum(OMEw)

  output <- list(Tn = Tn)

  return(output)
}



teststat_max_cov <- function(data, w,  G = 1, ...){
  LRS <- lapply(w, FUN = logRankStatistic, data = data, G = G)
  Tn <- unlist(lapply(LRS, "[[", "Tn")) # vector Tn
  # extract Variances and F_hat for calculation of covariance matrix
  V <- lapply(LRS, "[[", "V")[[1]]
  KME <- lapply(LRS, "[[", "KME")[[1]]
  sigma_hat <- ECM(V, KME, w) # Sigma (covariance matrix)
  Sigma_inv <- MASS::ginv(sigma_hat)
  pre_statistic <- Sigma_inv %*% Tn     #  Sigma^-1 Tn
  powerset <- pset( length(w) )
  stat <- max( unlist( lapply(powerset, teststat, pre_statistic = pre_statistic, Tn = Tn) ))
  out <- list( stat = stat, Sigma_inv = Sigma_inv)
}

teststat_max_rade <- function(data, w,  G = 1, Sigma_inv, ...){
  LRS <- lapply(w, FUN = logRankStatistic_rade, data = data, G = G)
  Tn <- unlist(lapply(LRS, "[[", "Tn")) # vector Tn
  pre_statistic <- Sigma_inv %*% Tn     #  Sigma^-1 Tn
  powerset <- pset( length(w) )
  stat <- max( unlist( lapply(powerset, teststat, pre_statistic = pre_statistic, Tn = Tn) ))
  out <- list( stat = stat, Sigma_inv = Sigma_inv)
}


