setwd("~/unknown_locs/")

library(readr)
library(ggplot2)

df <- read_table2("output/simulation.txt", col_names = FALSE)
df <- df[,1:3]
colnames(df) <- c("Model","Decimals","Covered")
df$Model <- factor(df$Model)
df$Decimals <- factor(df$Decimals)
df$Covered <- as.numeric(df$Covered)

tab <- table(df)[,,2] / (table(df)[,,2] + table(df)[,,1])
