## Single Sample Randomization Test for Location
one_sample.rand.test <- function(data, mu0=0, rep=50000, test="ts", sig=0.95) {
  cat("        One Sample Randomization Test for Location\n\n")
  cat(paste("data:", deparse(substitute(data)),"\n"))
  cat(paste("mu0:", mu0,"\n"))
  cat(paste("Number of Repetitions:", rep,"\n"))
  
  D <- data - mu0
  meanD <- mean(D)
  
  stddev <- sd(D)
  
  upper <- 0
  lower <- 0
  n <- length(D)
  
  cat(paste("Sample Size:", n))
  cat("\n\n")
  cat(paste("Mean Difference:", meanD,"\n"))
  cat(paste("Std. Dev.:", stddev, "\n"))
  
  # Begin sign randomizations
  meanDvec <- 1:rep
  for (i in 1:rep) {
    sgnvec <- sign(runif(n)-.5) # random vector with 1 or -1 values
    Dvec <- sgnvec*D
    
    # Calculate the mean difference for the ith randomization vector
    meanDvec[i] <- mean(Dvec)
    if(meanDvec[i] >= meanD) upper = upper + 1
    if(meanDvec[i] <= meanD) lower = lower + 1
  }
  
  cat("\n")
  # Calculate p-values:
  pval <- 0
  
  pval_lower <- lower / rep
  pval_upper <- upper / rep
  
  ## for "less than" one-sided H1
  if (test == "lt") {
    cat(paste("One-Sided p-value (lower):", pval_lower,"\n\n"))
    pval <- pval_lower
  }
  else if (test == "gt") { ## for "greater than" one-sided H1
    cat(paste("One-Sided p-value (upper):", pval_upper,"\n\n"))
    pval <- pval_upper
  }
  else { # for two-sided H1
    sgnD <- sign(meanD)
    if(sgnD < 0) pval_two_sided = 2 * pval_lower
    if(sgnD > 0) pval_two_sided = 2 * pval_upper
    cat(paste("Two-Sided p-value:", pval_two_sided,"\n\n"))
    pval <- pval_two_sided
  }
  
  if (pval < (1 - sig)) {
    cat(paste("Reject H0:",meanD,"=",mu0,"\n"))
  } else {
    cat(paste("Fail to Reject H0:",meanD,"=",mu0,"\n\n"))
  }
}