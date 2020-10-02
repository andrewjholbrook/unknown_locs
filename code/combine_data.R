setwd('~/unknown_locs/')

library(readr)
library(hpHawkes)
library(ggplot2)

library(data.table)
library(alphahull)
library(spatstat)
library(rgeos)
library(stringr)
#library(splancs)

source("code/utils.r")

set.seed(1)

# remove dec 29 - jan 2; july 1 - july 6
REMOVE_HOLIDAYS <- FALSE


# get 2017-2019
data3 = fread("data/Shot_Spotter_Gun_Shots.csv")
#data3 <- data3[data3$TYPE=="Single_Gunshot"|data3$TYPE=="Multiple_Gunshots",]
data3$DATETIME <- stringr::str_replace(data3$DATETIME,"T", " ")
data3$DATETIME <- stringr::str_replace(data3$DATETIME,".000Z", "")
data3$timestamp = as.POSIXct(data3$DATETIME, format="%Y-%m-%d %H:%M:%OS")
data3$timestamp = format(data3$timestamp, "%m/%d/%y %H:%M:%OS")
data3 = data3[complete.cases(data3$timestamp),]
data3 = unique(data3)

# remove holidays
data3$date = as.Date(data3$DATETIME, format="%Y-%m-%d")
data3$year <- format(data3$date,"%y")
data3 <- data3[data3$year==18,]
data3 <- data3[data3$TYPE=="Multiple_Gunshots"|data3$TYPE=="Single_Gunshot",]

doy = format(data3$date,format="%m-%d")
# code holidays as:
# July 2, 3, 4, 5, 6, December 29, 30, 31, January 1, 2

holidays = c("01-01")
data3$holiday = doy %in% holidays
data3 <- data3[!data3$holiday,]


data3$time <- as.numeric(as.POSIXct(data3$DATETIME))

t <- data3$time

X = as.matrix(lat_long_to_xy(data3$LATITUDE, data3$LONGITUDE))

X[,1] =(X[,1] - min(X[,1])) / 1000  # kilometers
X[,2] =(X[,2] - min(X[,2])) / 1000 # kilometers

t = (t - min(t)) / 3600 # hours


plot(X)
df = data.frame(X=X[,1], Y=X[,2], Time=t)
df <- df[order(df$Time),]
dim(df)
df <- unique(df)

saveRDS(df,file="data/2018_data.rds")
