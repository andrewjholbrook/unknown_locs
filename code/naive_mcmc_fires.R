setwd("~/unknown_locs")

library(hpHawkes)

df <- readRDS("data/wildfiresData.rds")
X <- cbind(df$x,df$y)
times <- df$time

set.seed(666)

gpu <- sampler(n_iter=500000,
               locations=X,
               times=times,
               gpu=2,
               sampleLocations = FALSE,
               burnIn = 50000,
               thinPeriod = 450,
               params = c(0.1,0.001,0.001,0.1,1,1))


samps <- gpu$samples
samps[1,] <- 1/samps[1,]
samps[2,] <- 1/samps[2,]
samps[3,] <- 1/samps[3,]
samps[4,] <- 1/samps[4,]
rownames(samps) <- c("se_spat_length","bg_spat_length","bg_temp_length","se_temp_length", "se_weight","back_weight")


png(filename = "figures/wild_fire_test.png", width = 7, height = 10, units = 'in', res = 300)
par(mfrow=c(3,2))

plot(samps[1,],type="l",ylab="Spatial lengthscale (km)",main = "Self-excitatory")
abline(h=median(samps[1,]),lwd=4,col="red")

plot(samps[2,],type="l",main="Background",ylab = "")
abline(h=median(samps[2,]),lwd=4,col="red")

plot(samps[4,],type="l",ylab="Temporal lengthscale (hrs)")
abline(h=median(samps[4,]),lwd=4,col="red")

plot(samps[3,],type="l",ylab = "")
abline(h=median(samps[3,]),lwd=4,col="red")

plot(samps[5,]/(samps[5,]+samps[6,]),type="l",ylab="Normalized weight")
abline(h=median(samps[5,]/(samps[5,]+samps[6,])),lwd=4,col="red")

plot(1-samps[5,]/(samps[5,]+samps[6,]),type="l",ylab = "")
abline(h=median(1-samps[5,]/(samps[5,]+samps[6,])),lwd=4,col="red")

dev.off()

saveRDS(gpu$samples,"output/wildfire_samps.rds")

