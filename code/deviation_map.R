setwd("~/unknown_locs/")

library(ggplot2)
library(maps)
library(ggmap)
library(reshape2)
register_google(key = "AIzaSyDzICiKTM1TA0Ux4bcBXFiwd1_1OMbizcg")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# 
# # load("data/dc_locations_and_self_excit_probs.Rdata")
# # colnames(df)[3] <- "Probability\nself-\nexcitatory"
# 
# classes <- c(5855,5856,
#              1181,1182,
#              6979,6980,
#              859,860,
#              5481,5482,
#              811,812,
#              7481,7482,
#              2963,2964,
#              6467,6468,
#              5707,5708,
#              4167,4168,
#              4609,4610)
# clsss <- rep(FALSE,7965)
# clsss[classes] <- TRUE
# classes <- character(length=7965)
# classes[clsss] <- "numeric"
# classes[!clsss] <- "NULL"


# need to back transform to long/lat by loading original data...
locas <- readRDS("data/coordsOrdered.rds")
displacements <- readRDS("output/meanDisplacement.rds")
locas$Displacements <- displacements[,3]*1000 # mean displacement
locas <- locas[order(locas$Displacements),]
colnames(locas)[3] <- "Mean\nposterior\ndisplacement\n(meters)"

bb <- c(left=-77.15,
        bottom=38.816,
        right=-76.9,
        top=38.969)

dc_stamen <- get_stamenmap(bbox = bb,
                           zoom = 13,
                           maptype="toner-background")
gg <- ggmap(dc_stamen)
gg

# df <- df[df$Latitude>bb[2] & df$Latitude<bb[4],]
# df <- df[df$Longitude < bb[3] & df$Longitude > bb[1],]

#where <- geocode("washington dc")
# dcMap <- qmap("washington dc", zoom=12,
#               maptype="toner-background") +
dcMap <- gg +
  geom_point(data=locas,aes(x=Longitude,y=Latitude,color=`Mean\nposterior\ndisplacement\n(meters)`),size=3) +
  scale_colour_distiller(palette="Spectral") +
  # scale_alpha_continuous(range = c(0.3,1),guide=FALSE) +
  annotate(geom="label",x=-77.036386,y=38.892711,label="Washington D.C.") +
  theme_void() +
  theme(legend.position = c(0.13, 0.6),
        legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black",
                                         fill="white"))
dcMap

ggsave(filename="dc_map_displacements.pdf",plot=dcMap,device="pdf",path="figures/",dpi="retina")

system2(command = "pdfcrop",
        args    = c("~/unknown_locs/figures/dc_map_displacements.pdf",
                    "~/unknown_locs/figures/dc_map_displacements.pdf")
)


### get biggest displacements
N <- dim(locas)[1]
 locas <- locas[3950:N,]
postCoords <- readRDS("output/postCoordsList.rds")
postCoordsMat <- postCoords[[1]]
Observations <- rownames(postCoordsMat)
for(i in 2:100){
  postCoordsMat <- rbind(postCoordsMat,postCoords[[i]])
}
postCoordsMat$Observation <- rep(Observations,100)
postCoordsMat <- postCoordsMat[postCoordsMat$Observation %in% rownames(locas),]
postCoordsMat$Observation <- factor(postCoordsMat$Observation)
df <- postCoordsMat
#df <- df[df$Observation==577|df$Observation==575,]

dcMap2 <- qmap("washington dc", zoom=12,
              maptype="toner-background") +
  geom_point(data=df,aes(x=Longitude,y=Latitude,color=Observation)) +
  #discrete_colour_distiller(palette="Spectral") +
  # scale_alpha_continuous(range = c(0.3,1),guide=FALSE) +
  annotate(geom="label",x=-77.036386,y=38.892711,label="Washington D.C.") +
  theme(legend.position = c(0.13, 0.6),
        legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
dcMap2

bb <- c(left=-77.032,
        bottom=38.96,
        right=-77.024,
        top=38.964)
palet <- RColorBrewer::brewer.pal(11,"Spectral")


df <- df[df$Latitude>38.96 & df$Latitude<38.964,]
df <- df[df$Longitude < -77.024 & df$Longitude > -77.032,]
blah <- readRDS("data/2018_data.rds")

df$Observation <- droplevels(df$Observation)

dc_stamen <- get_stamenmap(bbox = bb,
                           zoom = 18,
                           maptype="toner-background")
gg <- ggmap(dc_stamen)

gg <- gg + geom_point(data=df,aes(x=Longitude,y=Latitude,color=Observation)) +
  scale_color_manual(values=c(palet[2],palet[10]))+
  geom_point(data=locas,aes(x=Longitude,y=Latitude),inherit.aes = FALSE,color="gold2",size=7) +
  theme_void() +
 ggtitle("Inferred and observed locations #2")+
  theme(legend.position  = "none") 
  
gg

ggsave(filename="infVsObs1.png",plot=gg,device="png",path="figures/",dpi="retina")

# next figure
df <- postCoordsMat
bb <- c(left=-76.985,
        bottom=38.842,
        right=-76.9775,
        top=38.849)


df <- df[df$Latitude>bb[2] & df$Latitude<bb[4],]
df <- df[df$Longitude < bb[3] & df$Longitude > bb[1],]
blah <- readRDS("data/2018_data.rds")

df$Observation <- droplevels(df$Observation)

dc_stamen <- get_stamenmap(bbox = bb,
                           zoom = 18,
                           maptype="toner-background")
gg2 <- ggmap(dc_stamen)

gg2 <- gg2 + geom_point(data=df,aes(x=Longitude,y=Latitude,color=Observation)) +
  scale_color_manual(values=c(palet[1],palet[3],palet[9],palet[11]))+
  geom_point(data=locas,aes(x=Longitude,y=Latitude),inherit.aes = FALSE,color="gold2",size=7) +
  theme_void() +
 ggtitle("Inferred and observed locations #1")+
  theme(legend.position  = "none") 

gg2

ggsave(filename="infVsObs2.png",plot=gg2,device="png",path="figures/",dpi="retina")


# next figure
df <- postCoordsMat
bb <- c(left=-76.9335,
        bottom=38.882,
        right=-76.9267,
        top=38.886)


df <- df[df$Latitude>bb[2] & df$Latitude<bb[4],]
df <- df[df$Longitude < bb[3] & df$Longitude > bb[1],]
#blah <- readRDS("data/2018_data.rds")

df$Observation <- droplevels(df$Observation)

dc_stamen <- get_stamenmap(bbox = bb,
                           zoom = 18,
                           maptype="toner-background")
gg3 <- ggmap(dc_stamen)

gg3 <- gg3 + geom_point(data=df,aes(x=Longitude,y=Latitude,color=Observation)) +
  scale_color_manual(values=c(palet[1],palet[3],palet[9],palet[11]))+
  geom_point(data=locas,aes(x=Longitude,y=Latitude),inherit.aes = FALSE,color="gold2",size=7) +
  theme_void() +
 ggtitle("Inferred and observed locations #3")+
  theme(legend.position  = "none") 

gg3

ggsave(filename="infVsObs3.png",plot=gg3,device="png",path="figures/",dpi="retina")

 library(grid)
 library(gridExtra)
ggsave(filename="displacementFigs.pdf",plot=grid.arrange(gg,gg2,gg3,ncol=3,
       layout_matrix=cbind(c(2,2),c(1,3))),
       device="pdf",path="figures/",dpi="retina")
system2(command = "pdfcrop",
        args    = c("~/unknown_locs/figures/displacementFigs.pdf",
                    "~/unknown_locs/figures/displacementFigs.pdf")
)


# #
# ###########
# ####################
# ###########
# # dates and self excite probs
# 
# load("data/dates_and_self_excit_probs.Rdata")
# library(scales)
# library(RColorBrewer)
# library(grid)
# library(gridExtra)
# 
# gg <- ggplot(data = df2, aes(x=Date)) +
#   stat_smooth(aes(x=Date,y=Probabilities),se=FALSE,color=brewer.pal(11,"Spectral")[2]) +
#   stat_density(geom="line",aes(y=..density..*1000),adjust=2,size=1,color=brewer.pal(11,"Spectral")[10]) +
#   scale_x_date(date_breaks = "2 year",
#                labels = date_format("%Y")) +
#   xlab("Year") + ylab("") +
#   annotate(geom="text", x=df2$Date[38000],y=0.075,color=brewer.pal(11,"Spectral")[2],label="Self-excitatory probabilities") +
#   annotate(geom="segment", x=df2$Date[12000],xend =df2$Date[22000],y=0.075,yend=0.075,color=brewer.pal(11,"Spectral")[2],size=1.5) +
#   annotate(geom="text", x=df2$Date[37500],y=0.06,color=brewer.pal(11,"Spectral")[10],label="Gunshot density (x1000)") +
#   annotate(geom="segment", x=df2$Date[12000],xend =df2$Date[22000],y=0.06,yend=0.06,color=brewer.pal(11,"Spectral")[10],size=1.5) +
#   theme_classic()
# 
# gg
# 
# ggsave(plot=gg, filename = "year_se_probs",device = "pdf",path="figures/")
# 
# 
# 
# ggsave(filename = "combined_year_map.png", grid.arrange(dcMap,gg,ncol=2),
#        device = "png",path="figures/" ,width = 9,height = 4,dpi="retina")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
