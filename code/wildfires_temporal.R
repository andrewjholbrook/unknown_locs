setwd("~/unknown_locs")

library(readr)
library(ggplot2)

dat <- readr::read_csv("data/FireHistoryPoints_2015-2019.csv")
df  <- data.frame(dat$ORIGINAL_LONGITUDE,dat$ORIGINAL_LATITUDE,dat$DISCOVERYDATETIME,dat$DISCOVERYSIZE)
remove(dat)
colnames(df) <- c("lon","lat","time","acres")
df <- df[complete.cases(df),] # all complete anyway

# remove spatial outliers
df <- df[df$lat>53,]
df <- df[df$lat<75,]
df <- df[df$lon< -120,]

df$radius <- sqrt( df$acres/247.11/pi )  # radius of circle
df <- df[,-4]

# get times
df$time <- lubridate::parse_date_time(df$time,orders = "mdyHMSp")
df$Year <- lubridate::decimal_date(df$time)

#df <- readRDS("data/wildfiresData.rds")

palet <- RColorBrewer::brewer.pal(11,"Spectral")

#times <- df$time

params_unbiased <- read_table2("output/params.txt", col_names = FALSE)
params_unbiased <- params_unbiased[,-7]
params_unbiased[,1:4] <- 1/params_unbiased[,1:4]
unbiased_quants <- quantile(as.numeric(unlist(params_unbiased[,3])),probs=c(0.025,0.5,0.975))
unbiased_quants <- unbiased_quants/365

params_unbiased <- read_table2("output/wildfire_naive_params.txt", col_names = FALSE)
params_unbiased <- params_unbiased[,-7]
params_unbiased[,1:4] <- 1/params_unbiased[,1:4]
biased_quants <- quantile(as.numeric(unlist(params_unbiased[,3])),probs=c(0.025,0.5,0.975))
biased_quants <- biased_quants/365

Model <- c("Full"=palet[2],"Naive\n(mode A)"=palet[10])

gg2 <- ggplot(data=df,aes(x=Year)) +
  geom_point(aes(y=-0.02),shape=4,color=palet[4],alpha=0.1) +
  geom_density(bw=unbiased_quants[1],aes(color="Full"),linetype=2) +
  geom_density(bw=unbiased_quants[2],aes(color="Full")) +
  geom_density(bw=unbiased_quants[3],aes(color="Full"),linetype=2) +
  geom_density(bw=biased_quants[1],aes(color="Naive\n(mode A)"),linetype=2) +
  geom_density(bw=biased_quants[2],aes(color="Naive\n(mode A)")) +
  geom_density(bw=biased_quants[3],aes(color="Naive\n(mode A)"),linetype=2) +
  labs(x = "Year",
       y = "Density",
       color = "Model") +
  scale_color_manual(name = "Model",
                     values = Model,
                     labels = c("Full","Naive\n(mode A)")) +
  ggtitle("Background smooth and 95% credible band") +
  theme_classic() 
  
gg2

#8.14 x 6.03 in
ggsave(gg2,filename = "alaskaFiresTemporal.pdf",device = "pdf",path = "figures/",dpi = "retina",
       width=8.14,height=4)

system2(command = "pdfcrop",
        args    = c("~/unknown_locs/figures/alaskaFiresTemporal.pdf",
                    "~/unknown_locs/figures/alaskaFiresTemporal.pdf")
)


