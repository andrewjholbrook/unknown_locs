setwd("~/unknown_locs/")

library(readr)

originalLocs <- readRDS("data/2018_data.rds")
originalLocs <- originalLocs[,-3]

locs_unbiased <- read_table2("output/locs_unbiased.txt", col_names = FALSE)
locs_unbiased <- locs_unbiased[,-dim(locs_unbiased)[2]]
N <- dim(locs_unbiased)[1]
sq <- round(seq(from=1,to=N,length.out = 100))
locs_unbiased <- locs_unbiased[sq,]

diffs <- list()
displacement <- matrix(0,dim(originalLocs)[1],2)
for (i in 1:100) {
  diffs[[i]] <- originalLocs[,1:2] - matrix(as.numeric(unlist(locs_unbiased[i,])),byrow = TRUE,ncol = 2)
  displacement <- displacement + diffs[[i]]
}
DispLong <- abs(displacement[,1])/100
DispLat <- abs(displacement[,2])/100
displacement <- sqrt(displacement[,1]^2 + displacement[,1]^2)/100
DispsData <- data.frame(DispLong, DispLat, displacement)
#saveRDS(DispsData,file="output/meanDisplacement.rds")

#### now use diffs to get coordinates
# read coordinates
coords <- readRDS("data/coordsOrdered.rds")
coords <- coords[,2:1]

coordsList <- list()
for (i in 1:100) {
  coordsList[[i]] <- coords - diffs[[i]]/100
}
saveRDS(coordsList,file = "output/postCoordsList.rds")

