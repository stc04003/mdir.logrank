#' @export
print.mdirLR <- function(x, ...) {
  cat("test statistic: ", "\n", "     ", x$stat, sep = "")
  df <- length(x$rg)+ sum(x$cross)
  cat("\n", "p-value (chi-squared Approx. with ",
      df, " df): ",
      "\n", "     ", x$p_value$Approx, sep = "")
  cat("\n", "p-value (", x$nperm, " permutations): ",
      "\n", "     ", x$p_value$Perm, sep = "")
}

#' @export
summary.mdirLR <- function (object, ...) {
  x <- object
  if ( length(x$rg) == 0 ){
    cat("The chosen weights are linearly independent.", "\n",
        "The test is based on the crossing weight.", "\n","\n")
  }else{
        rg_rep <- paste0("c(", x$rg[[1]][1], ", ", x$rg[[1]][2], ")" )
        if ( length(x$rg) > 1 ){
          for (i in 2:length(x$rg)){
            rg_rep <- paste0( rg_rep, ", c(", x$rg[[i]][1], ", ", x$rg[[i]][2], ")" )
          }
        }
        cat("The chosen weights are", if ( x$indep == FALSE){ " not"},
            " linearly independent.", "\n", "The test is based on ",
            if ( x$cross == TRUE){ "the crossing weight and "},
            length(x$rg), " ","weight", if (length(x$rg) > 1){"s"}, " with exponents ", "\n",
            "     ", "c(r,g)=  ", rg_rep, ".", "\n","\n", sep="")
  }
  print(x)
}
