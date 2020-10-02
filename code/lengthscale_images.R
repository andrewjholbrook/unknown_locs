setwd("~/unknown_locs/")

library(ggplot2)
library(gridExtra)
library(grid)
library(readr)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)

}


params_unbiased <- read_table2("output/params_unbiased.txt", col_names = FALSE)
params_unbiased <- params_unbiased[,-7]
params_unbiased[,1:4] <- 1/params_unbiased[,1:4]
params_unbiased[,5] <- params_unbiased[,5] / (params_unbiased[,5]+params_unbiased[,6])
params_unbiased <- params_unbiased[,-6]
pu <- coda::as.mcmc(params_unbiased)

params_naive <- read_table2("output/params.txt", col_names = FALSE)
params_naive <- params_naive[,-7]
params_naive[,1:4] <- 1/params_naive[,1:4]
params_naive[,5] <- params_naive[,5] / (params_naive[,5]+params_naive[,6])
params_naive <- params_naive[,-6]
pn <- coda::as.mcmc(params_naive)

df <- rbind(params_naive,params_unbiased)
df <- data.frame(df)
df$Model <- c(rep("Naive",dim(params_naive)[1]), rep("Full",dim(params_unbiased)[1]))
df$Model <- factor(df$Model)
df <- df[,-(3:5)]
colnames(df)[1:2] <- c("Self-excitatory","Background")
df <- reshape2::melt(df,value.name="Lengthscale")
colnames(df)[2] <- "Rate\ncomponent"
colnames(df)[3] <- "Meters"
df$Meters <- df$Meters * 1000 # km to m
df$`Rate\ncomponent` <- factor(df$`Rate\ncomponent`)

df2 <- df[,1:2]
df2 <- unique(df2)
df2$Upper <- c(7.695546e-02,6.624552e-02,1.107802e-01,1.033356e-01) 
df2$Lower <- c(6.783538e-02,5.569585e-02,1.021235e-01,9.396624e-02)
df2$Lower <- df2$Lower * 1000
df2$Upper <- df2$Upper * 1000
df <- merge(df,df2)
palet <- RColorBrewer::brewer.pal(11,"Spectral")

gg <- ggplot(data = df, aes(x=Meters,fill=Model,linetype=`Rate\ncomponent`)) +
  geom_density(adjust=2,alpha=0.5) +
  geom_segment(data=df,aes(x=Lower,y=0.17,xend=Upper,yend=0.17,colour=Model),size=0.8,lineend = "round") +
  xlab("Spatial lengthscale (meters)") + ylab("Posterior density") +
  #geom_text(aes(y=0.17,x=115,label="95% Credible\ninterval")) +
  scale_color_manual(values=c("Full"=palet[2],"Naive"=palet[10]))+
  scale_fill_manual(values=c("Full"=palet[2],"Naive"=palet[10]))+
  theme_classic()
gg


ggsave('compare_lengthscales.pdf',gg,
       device = 'pdf',path = 'figures/',
       width = 6,height=3)

system2(command = "pdfcrop", 
        args    = c("~/unknown_locs/figures/compare_lengthscales.pdf", 
                    "~/unknown_locs/figures/compare_lengthscales.pdf") 
)

