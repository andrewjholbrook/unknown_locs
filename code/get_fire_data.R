setwd("~/unknown_locs/")
source("code/utils.r")
library("ggmap")
register_google(key = "AIzaSyDzICiKTM1TA0Ux4bcBXFiwd1_1OMbizcg")


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
df$time <- as.numeric(df$time)
df$time <- df$time - min(df$time)
df$time <- df$time / ( 60*60*24 ) # seconds to days
df <- df[order(df$time),]

# get xy coords
library(data.table)
holder <- lat_long_to_xy_ak(df$lat,df$lon)
holder <- holder / 1000 # meters to km

df$x <- holder[,1]
df$y <- holder[,2]

# plot over alaska map to check outliers
library(ggmap)
bb <- c(left=-177,
        bottom=53.3,
        right=-125,
        top=70.3)

ak_stamen <- get_stamenmap(bbox = bb,
                           zoom = 7,
                           maptype="terrain")

# library(wesanderson)
# # Gradient color
# pal <- wes_palette("Zissou1", 100, type = "continuous")

# transform rads to get plottable size
df$radius[df$radius==0] <- min(df$radius[df$radius!=0])
df$`Discovery\nradius\n(log meters)` <- log(df$radius * 1000)
palet <- RColorBrewer::brewer.pal(11,"Spectral")


gg <- ggmap(ak_stamen) +
  geom_point(data=df,aes(x=lon,y=lat,size=`Discovery\nradius\n(log meters)`),
             fill=palet[2],shape=24,stroke=0.01) +
  #scale_size_continuous(range = c(0.2, 1.5),breaks = c(2,4,6,8),labels=c(2,4,6,8))+
  scale_radius(range = c(1, 4))+
  theme_void()

gg

gg <- gg + ggtitle("2,925 Wildfire ignition sites in Alaska: 2015-2019")

library(grid)
library(gridExtra)
ggsave(gg,filename = "alaskaFires.pdf",device = "pdf",path = "figures/",dpi = "retina")
#ggsave(grid.arrange(gg,gg2,ncol=2),filename = "alaskaFires.pdf",device = "pdf",path = "figures/",dpi = "retina")

system2(command = "pdfcrop",
        args    = c("~/unknown_locs/figures/alaskaFires.pdf",
                    "~/unknown_locs/figures/alaskaFires.pdf")
)

#saveRDS(df,file="data/wildfiresData.rds")
#readRDS(df,file="data/wildfiresData.rds")




