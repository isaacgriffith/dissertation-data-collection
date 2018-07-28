# MIT License
# 
# Copyright (c) 2018 Isaac Griffith
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

power_func <- function(r, f, sample.rep, power.rep) {
  power.results <- data.frame(matrix(ncol = 4, nrow = sample.rep))
  
  ## Sample size loop.
  for (k in 1:sample.rep) {
    cat("", "\n")
    cat(paste("Sample Size =", k*length(unique(r@frame[,ncol(r@frame)]))), "\n")
    
    ## Storage vector
    pvalue <- numeric(power.rep)
    
    ## Progress Bar
    pb <- txtProgressBar(max = power.rep, style=3)
    
    ## Power Replications
    for (j in 1:power.rep) {
      Sys.sleep(0.001); setTxtProgressBar(pb, j) # Update progress bar
      ## Simulate response data, put in vector
      simdv <- matrix(unlist(simulate(f, nsim = k)), ncol = 1)
      ## Simulate predictor data, renumbering subject ID.
      mm <- NULL; mm1 <- NULL; c <- 0
      ## Concatenate sample size
      for (i in 1:k) {
        mm1 <- f@frame
        mm1[,ncol(mm1)] <- as.integer(mm1[,ncol(mm1)] + c)
        c <- max(mm1[,ncol(mm1)])
        mm <- rbind(mm, mm1)
      }
      ## Run anova().
      mm[,1] <- simdv
      s.full <- lmer(formula(f), mm, REML = F)
      s.reduced <- lmer(formula(r), mm, REML = F)
      pvalue[j] <- anova(s.reduced, s.full)[2,7]
      
      ## Progress bar cleanup
      Sys.sleep(.002)
      close(pb)
    }
    
    ## Bootstrap sample size
    power.results[k,1] <- max(mm[,ncol(mm)])
    
    ## Power calculations for different alpha.
    power.results[k,2] <- mean(pvalue <= .01)
    power.results[k,3] <- mean(pvalue <= .05)
    power.results[k,4] <- mean(pvalue <= .15)
    
    ## Clean up for console screen
    cat("", "\n")
  }
  
  cat("", "\n")
  cat("Finished", "\n")
  
  ## Return power results
  colnames(power.results) <- c ("N","alpha.01","alpha.05","alpha.15")
  return(power.results)
}
