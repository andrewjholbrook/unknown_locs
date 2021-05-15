setwd("~/unknown_locs")
source("code/simulateHawkes.R")
library(hpHawkes)
set.seed(666)

for(i in 1:1000) {
  digits <- 1
  output <- generate_sample(mean_N_bg_points=200)
  output <- output[order(output[,3]),]
  X <- round(output[,1:2],digits = digits)
  times <- output[,3] 
  
  
  out <- hpHawkes::simulation_sampler(n_iter=30000,
                                      locations=X,
                                      times=times,
                                      gpu=2,
                                      sampleLocations = TRUE,
                                      windowWidth = 10^(-digits),
                                      burnIn = 1000,
                                      thinPeriod = 1,
                                      params = c(2,1,1,5,0.5,0.5))
  
  samps <- out$samples
  se_spat_length <- 1/samps[1,]
  spat_length_covered_95 <- (0.5 < quantile(se_spat_length,probs = 0.975) &
                            0.5 > quantile(se_spat_length,probs = 0.025) )
  spat_length_covered_80 <- (0.5 < quantile(se_spat_length,probs = 0.9) &
                               0.5 > quantile(se_spat_length,probs = 0.1) )
  spat_length_covered_50 <- (0.5 < quantile(se_spat_length,probs = 0.75) &
                               0.5 > quantile(se_spat_length,probs = 0.25) )
  
  cat("sampledLocations", digits, spat_length_covered_95,
      spat_length_covered_80, spat_length_covered_50, "\n",
      file = "output/simulation.txt", append = TRUE, sep=" ")
  
  locsArray <- simplify2array(out$locations)
  locsQuant <- apply(locsArray,MARGIN = c(1,2), FUN=function(x) quantile(x,probs=c(0.025,0.975))) 
  locsCovered <- output[,1:2] < locsQuant[2,,] & output[,1:2] > locsQuant[1,,]
  propCovered <- sum(locsCovered)/length(locsCovered)
  
  cat(digits, propCovered, "\n",
      file = "output/simulationLocsCoverage.txt", append = TRUE, sep=" ")
  
  out <- hpHawkes::simulation_sampler(n_iter=15000,
                                       locations=X,
                                       times=times,
                                       gpu=2,
                                       sampleLocations = FALSE,
                                       burnIn = 1000,
                                       thinPeriod = 1,
                                       params = c(2,1,1,5,0.5,0.5))
  
  samps <- out$samples
  se_spat_length <- 1/samps[1,]
  spat_length_covered_95 <- (0.5 < quantile(se_spat_length,probs = 0.975) &
                               0.5 > quantile(se_spat_length,probs = 0.025) )
  spat_length_covered_80 <- (0.5 < quantile(se_spat_length,probs = 0.9) &
                               0.5 > quantile(se_spat_length,probs = 0.1) )
  spat_length_covered_50 <- (0.5 < quantile(se_spat_length,probs = 0.75) &
                               0.5 > quantile(se_spat_length,probs = 0.25) )
  
  cat("fixedLocations", digits, spat_length_covered_95,
      spat_length_covered_80, spat_length_covered_50, "\n",
      file = "output/simulation.txt", append = TRUE, sep=" ")
  
  ##############################################################################
  X <- round(2*output[,1:2])/2
  digits <- 0.5
  
  out <- hpHawkes::simulation_sampler(n_iter=30000,
                                      locations=X,
                                      times=times,
                                      gpu=2,
                                      sampleLocations = TRUE,
                                      windowWidth = 0.5,
                                      burnIn = 1000,
                                      thinPeriod = 1,
                                      params = c(2,1,1,5,0.5,0.5))
  
  samps <- out$samples
  se_spat_length <- 1/samps[1,]
  spat_length_covered_95 <- (0.5 < quantile(se_spat_length,probs = 0.975) &
                               0.5 > quantile(se_spat_length,probs = 0.025) )
  spat_length_covered_80 <- (0.5 < quantile(se_spat_length,probs = 0.9) &
                               0.5 > quantile(se_spat_length,probs = 0.1) )
  spat_length_covered_50 <- (0.5 < quantile(se_spat_length,probs = 0.75) &
                               0.5 > quantile(se_spat_length,probs = 0.25) )
  
  cat("sampledLocations", digits, spat_length_covered_95,
      spat_length_covered_80, spat_length_covered_50, "\n",
      file = "output/simulation.txt", append = TRUE, sep=" ")
  
  locsArray <- simplify2array(out$locations)
  locsQuant <- apply(locsArray,MARGIN = c(1,2), FUN=function(x) quantile(x,probs=c(0.0125,0.9875))) #bonferroni correction
  locsCovered <- output[,1:2] < locsQuant[2,,] & output[,1:2] > locsQuant[1,,]
  propCovered <- sum(locsCovered)/length(locsCovered)
  
  cat(digits, propCovered, "\n",
      file = "output/simulationLocsCoverage.txt", append = TRUE, sep=" ")
  
  out <- hpHawkes::simulation_sampler(n_iter=15000,
                                      locations=X,
                                      times=times,
                                      gpu=2,
                                      sampleLocations = FALSE,
                                      burnIn = 1000,
                                      thinPeriod = 1,
                                      params = c(2,1,1,5,0.5,0.5))
  
  samps <- out$samples
  se_spat_length <- 1/samps[1,]
  spat_length_covered_95 <- (0.5 < quantile(se_spat_length,probs = 0.975) &
                               0.5 > quantile(se_spat_length,probs = 0.025) )
  spat_length_covered_80 <- (0.5 < quantile(se_spat_length,probs = 0.9) &
                               0.5 > quantile(se_spat_length,probs = 0.1) )
  spat_length_covered_50 <- (0.5 < quantile(se_spat_length,probs = 0.75) &
                               0.5 > quantile(se_spat_length,probs = 0.25) )
  
  cat("fixedLocations", digits, spat_length_covered_95,
      spat_length_covered_80, spat_length_covered_50, "\n",
      file = "output/simulation.txt", append = TRUE, sep=" ")
  
  
  ##############################################################################
  digits <- 0
  X <- round(output[,1:2])

  
  out <- hpHawkes::simulation_sampler(n_iter=30000,
                                      locations=X,
                                      times=times,
                                      gpu=2,
                                      sampleLocations = TRUE,
                                      windowWidth = 1,
                                      burnIn = 1000,
                                      thinPeriod = 1,
                                      params = c(2,1,1,5,0.5,0.5))
  
  samps <- out$samples
  se_spat_length <- 1/samps[1,]
  spat_length_covered_95 <- (0.5 < quantile(se_spat_length,probs = 0.975) &
                               0.5 > quantile(se_spat_length,probs = 0.025) )
  spat_length_covered_80 <- (0.5 < quantile(se_spat_length,probs = 0.9) &
                               0.5 > quantile(se_spat_length,probs = 0.1) )
  spat_length_covered_50 <- (0.5 < quantile(se_spat_length,probs = 0.75) &
                               0.5 > quantile(se_spat_length,probs = 0.25) )
  
  cat("sampledLocations", digits, spat_length_covered_95,
      spat_length_covered_80, spat_length_covered_50, "\n",
      file = "output/simulation.txt", append = TRUE, sep=" ")
  
  locsArray <- simplify2array(out$locations)
  locsQuant <- apply(locsArray,MARGIN = c(1,2), FUN=function(x) quantile(x,probs=c(0.0125,0.9875))) #bonferroni correction
  locsCovered <- output[,1:2] < locsQuant[2,,] & output[,1:2] > locsQuant[1,,]
  propCovered <- sum(locsCovered)/length(locsCovered)
  
  cat(digits, propCovered, "\n",
      file = "output/simulationLocsCoverage.txt", append = TRUE, sep=" ")
  
  out <- hpHawkes::simulation_sampler(n_iter=15000,
                                      locations=X,
                                      times=times,
                                      gpu=2,
                                      sampleLocations = FALSE,
                                      burnIn = 1000,
                                      thinPeriod = 1,
                                      params = c(2,1,1,5,0.5,0.5))
  
  samps <- out$samples
  se_spat_length <- 1/samps[1,]
  spat_length_covered_95 <- (0.5 < quantile(se_spat_length,probs = 0.975) &
                               0.5 > quantile(se_spat_length,probs = 0.025) )
  spat_length_covered_80 <- (0.5 < quantile(se_spat_length,probs = 0.9) &
                               0.5 > quantile(se_spat_length,probs = 0.1) )
  spat_length_covered_50 <- (0.5 < quantile(se_spat_length,probs = 0.75) &
                               0.5 > quantile(se_spat_length,probs = 0.25) )
  
  cat("fixedLocations", digits, spat_length_covered_95,
      spat_length_covered_80, spat_length_covered_50, "\n",
      file = "output/simulation.txt", append = TRUE, sep=" ")
  
}
  
 