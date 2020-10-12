setwd("~/unknown_locs/")

library(readr)
library(xtable)

params_unbiased <- read_table2("output/params.txt", col_names = FALSE)
params_unbiased <- params_unbiased[,-7]
params_unbiased[,1:4] <- 1/params_unbiased[,1:4]
params_unbiased[,5] <- params_unbiased[,5] / (params_unbiased[,5]+params_unbiased[,6])
params_unbiased <- params_unbiased[,-6]
pu <- coda::as.mcmc(params_unbiased)

params_naive <- read_table2("output/wildfire_naive_params.txt", col_names = FALSE)
params_naive <- params_naive[,-7]
params_naive[,1:4] <- 1/params_naive[,1:4]
params_naive[,5] <- params_naive[,5] / (params_naive[,5]+params_naive[,6])
params_naive <- params_naive[,-6]
pn <- coda::as.mcmc(params_naive)

params_naive2 <- read_table2("output/wildfire_naive_params2.txt", col_names = FALSE)
params_naive2 <- params_naive2[,-7]
params_naive2[,1:4] <- 1/params_naive2[,1:4]
params_naive2[,5] <- params_naive2[,5] / (params_naive2[,5]+params_naive2[,6])
params_naive2 <- params_naive2[,-6]
pn2 <- coda::as.mcmc(params_naive2)

df <- cbind(summary(pu)[[2]][,c(1,3,5)],summary(pn)[[2]][,c(1,3,5)],summary(pn2)[[2]][,c(1,3,5)])
xtable(df,digits=4)

