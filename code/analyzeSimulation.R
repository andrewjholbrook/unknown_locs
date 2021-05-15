setwd("~/unknown_locs/")

library(readr)
library(ggplot2)

df <- read_table2("output/simulation.txt", col_names = FALSE)
df <- df[,1:5]
colnames(df) <- c("Model","Decimals","Covered95", "Covered80","Covered50")
df$Model <- factor(df$Model)
df$Decimals <- factor(df$Decimals)
df$Covered95 <- as.numeric(df$Covered95)
df$Covered80 <- as.numeric(df$Covered80)
df$Covered50 <- as.numeric(df$Covered50)

library(reshape2)
df <- melt(df,measure.vars=3:5,value.name = "Covered",variable.name = "Level")
df1 <- df[df$Level=="Covered95",]
df1 <- df1[,-3]

tab1 <- table(df1)[,,2] / (table(df1)[,,2] + table(df1)[,,1])
tab1

df1 <- df[df$Level=="Covered80",]
df1 <- df1[,-3]

tab2 <- table(df1)[,,2] / (table(df1)[,,2] + table(df1)[,,1])
tab2

df1 <- df[df$Level=="Covered50",]
df1 <- df1[,-3]

tab3 <- table(df1)[,,2] / (table(df1)[,,2] + table(df1)[,,1])
tab3

