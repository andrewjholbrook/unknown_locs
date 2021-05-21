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

library(xtable)
xtable(cbind(tab3,tab2,tab1))


locs <- read_table2("output/simulationLocsCoverage.txt", col_names = FALSE)
locs <- locs[,1:2]
colnames(locs) <- c("Decimals","Coverage")
locs$Decimals[locs$Decimals==1] <- 0.1
locs$Decimals[locs$Decimals==0] <- 1.0 
locs$Precision <- factor(locs$Decimals)

means <- aggregate(Coverage ~  Precision, locs, mean)

gg <- ggplot(locs,aes(y=Coverage,x=Precision)) +
  geom_hline( yintercept=0.95,color="purple",size=2) +
  ylab("Proportion of locations covered by 95% CI") +
  xlab("Spatial data precision") +
  #annotate(geom="label",x=-77.036386,y=38.892711,label=mean()) +
  geom_boxplot() +
  geom_text(data = means, aes(label = round(Coverage,digits = 3), y = Coverage + 0.0035)) +
  ggtitle("Coverage distributions across independent simulations") +
  theme_bw()
  
gg

ggsave("coverage.pdf",gg,device = "pdf",width=6,height = 4,path = "figures/")



