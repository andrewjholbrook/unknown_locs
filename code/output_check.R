setwd("~/unknown_locs/")

library(coda)

numLocs <- 2925
N       <- numLocs*2

classes <- sample(1:N,size=7)
clsss <- rep(FALSE,N)
clsss[classes] <- TRUE
classes <- character(length=N)
classes[clsss] <- "numeric"
classes[!clsss] <- "NULL"

locas <- read.table("output/locs.txt", colClasses = classes, 
                    header = FALSE,sep = " ")
traceplot(as.mcmc(locas))
effectiveSize(locas)

pars <- read.table("output/params.txt",header = FALSE,sep = " ")
pars <- pars[,-7]
effectiveSize(pars)
traceplot(as.mcmc(pars))
