setwd("~/unknown_locs/")

library(readr)

displacements <- readRDS("output/meanDisplacement.rds")
displacements <- displacements$displacement
displacements <- displacements * 1000

times <- readRDS("data/2018_data.rds")
times <- times$Time

# mod <- lm(displacements ~ times) # nothing
# summary(mod)
# plot(times,displacements)

params_unbiased <- read_table2("output/params_unbiased.txt", 
                                         col_names = FALSE)
params_unbiased <- params_unbiased[,-7]
params_unbiased[,1:4] <- 1/params_unbiased[,1:4]

locs <- read_table2("output/locs_unbiased.txt", 
                    col_names = FALSE)
locs <- locs[,-dim(locs)[2]]

# thin to 100
sq <- floor(seq(from=1,to=dim(locs)[1],length.out = 100))
params_unbiased <- params_unbiased[sq,]
locs <- locs[sq,]

locsList <- list()
for(i in 1:100) {
  locsList[[i]] <- matrix(locs[i,],ncol = 2, byrow=TRUE)
}

# get probability child
post_prob_child <- matrix(100,length(times),100)
for (i in 1:100) {
  X <- matrix(as.numeric(unlist(locsList[[i]])),ncol=2)
  post_prob_child[,i] <- hpHawkes::probability_se(locations = X,
                                                  times = times,
                                                  params = as.numeric(params_unbiased[i,]),
                                                  gpu = 2,dimension=2)
}
post_probs <- rowMeans(post_prob_child,na.rm = TRUE)
post_probs[1] <- 0

mod <- lm(log(displacements)~post_probs)

