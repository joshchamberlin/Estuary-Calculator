#############################################################################
# Salish Sea Marine Survival Project                                        #
# Ecosystem Indicators Analysis                                             #
# Figure for QNA paper Figure 1 from C. Greene Salmon Population Trend Data #                                            
#                                                                           #
# Generated on 1/25/2017                                                    #
# K. Sobocinski                                                             #
# kathryn.sobocinski@noaa.gov                                               #
# Long Live the Kings/NOAA NWFSC                                            #
#############################################################################
library(ggplot2)
library(ggpubr)
library(plyr)
library("RColorBrewer")
library(gridExtra)

setwd("C:\\Users\\Kathryn.Sobocinski\\Documents\\SSMSP\\Data\\Salmon")
trends=read.csv("Trends.csv")
head(trends)
dim(trends) #151x21
trends1=trends[1:151, 2:11]
head(trends1)
tail(trends1)


#Break out:
#steelhead, chinook, coho= focal
#pink, chum, sockeye= other
focal=trends1[1:91, 1:10]
droplevels(focal)$Species
other=trends1[92:151, 1:10]
droplevels(other)$Species

levels(focal$Subbasin)
#Order Subbasins S-N
focal$Subbasin=factor(focal$Subbasin, 
                      levels=c("South Sound", "Central Puget Sound",
                               "Whidbey", "Hood Canal", "Juan de Fuca",
                                "South Strait of Georgia", "Central Strait of Georgia",
                                "North Strait of Georgia", "Johnstone Strait","Pacific Coast"))

#Specify stacked labels
levels(focal$Subbasin) <- gsub(" ", "\n", levels(focal$Subbasin))
#Specify colors
cbPalette <- c("#000033", "#56B4E9", "#006633")

#Generate plot and options, focal spp.
pf=ggplot(focal, aes(x=Subbasin, y=PopTrend)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    geom_jitter(aes(colour=Species, shape=Species, size=1.5), width=0.15) +
    guides(size = "none", colour = guide_legend(override.aes = list(size=3))) +
    scale_y_continuous(limits=c(-0.25, 0.25)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 5.5, color = "darkgray") +
    geom_vline(xintercept = 9.5, color = "darkgray") +
    ylab("Population Trend") +
    xlab("Sub-Basin") +
    scale_colour_manual(values=cbPalette) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), rect = element_blank(),
                      axis.line = element_line(colour = "black")) +
    theme(legend.text=element_text(size=14))+
    theme(legend.position = c(0.12, 0.90))
pf    


#Other spp.
#Order Subbasins S-N
other$Subbasin=factor(other$Subbasin, 
                      levels=c("South Sound", "Central Puget Sound",
                               "Whidbey", "Hood Canal", "Juan de Fuca",
                               "South Strait of Georgia", "Central Strait of Georgia",
                                "North Strait of Georgia", "Johnstone Strait","Pacific Coast"))

#Specify stacked labels
levels(other$Subbasin) <- gsub(" ", "\n", levels(other$Subbasin))
#Specify colors
cbPalette <- c("#E69F00", "#D55E00", "#330000")

#Generate plot and options
po=ggplot(other, aes(x=Subbasin, y=PopTrend)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=12)) +
  geom_jitter(aes(colour=Species, shape=Species, size=1.5), width=0.15) +
  guides(size = "none", colour = guide_legend(override.aes = list(size=3))) +
  scale_y_continuous(limits=c(-1.1, 1.1)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 5.5, color = "darkgray") +
  geom_vline(xintercept = 6.5, color = "darkgray") +
  ylab("Population Trend") +
  scale_colour_manual(values=cbPalette) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), rect = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.text=element_text(size=14))+
  theme(legend.position = c(0.10, 0.90))
po    

#Specify stacked labels
levels(other$Subbasin) <- gsub(" ", "\n", levels(other$Subbasin))
#Specify colors
cbPalette <- c("#E69F00", "#D55E00", "#330000")

#Some issue with the outlier points--remove any over 0.25
po1=ggplot(subset(other, PopTrend<0.25), aes(x=Subbasin, y=PopTrend)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  geom_jitter(aes(colour=Species, shape=Species, size=1.5), width=0.15) +
  guides(size = "none", colour = guide_legend(override.aes = list(size=3))) +
  scale_y_continuous(limits=c(-0.25, 0.25)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 5.5, color = "darkgray") +
  geom_vline(xintercept = 6.5, color = "darkgray") +
  ylab("Population Trend") +
  xlab("Sub-Basin") +
  scale_colour_manual(values=cbPalette) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), rect = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.text=element_text(size=14))+
  theme(legend.position = c(0.11, 0.90))
po1   

po2=po1 + theme(panel.background = element_blank(), # bg of the panel
                plot.background = element_rect(fill = "transparent", colour=NA)) # bg of the plot
pf1=pf + theme(panel.background = element_blank(), # bg of the panel
               plot.background = element_rect(fill = "transparent", colour=NA)) # bg of the plot
#To make 2panel plot
salmonpops<-grid.arrange(pf, po1 + theme(axis.title.y=element_blank()), ncol=2)
pf
po1
#2 Panel Plot with Transparent BG for presentation
salmonpops2<-grid.arrange(pf1, po2 + theme(axis.title.y=element_blank()), ncol=2)
ggsave("Trends Fig No BG.png", salmonpops2, bg = "transparent", width=12, height=6)


png('salmonpops.png', units="in", width=11.5, height=5.5, res=300)
grid.arrange(pf, po1 + theme(axis.title.y=element_blank()), ncol=2)
dev.off()

tiff('salmonpops.tiff', units="in", width=11.5, height=5.5, res=500)
grid.arrange(pf, po1 + theme(axis.title.y=element_blank()), ncol=2)
dev.off()

postscript("salmonpops.eps", width = 480, height = 480)
grid.arrange(pf, po1 + theme(axis.title.y=element_blank()), ncol=2)# Make plot
dev.off()


theme_pubr()
+theme(plot.title = element_text(hjust = 0, margin = margin(t = 0, b = 10)),
                   panel.margin = unit(0.5, "lines"),
                   strip.background = element_rect(fill=NA, color=NA),
                   strip.text.x = element_text(size=12, face="bold"),
                   panel.border = element_rect(fill=NA, colour = "black", size=1.15),
                   axis.line.x = element_line(color="black", size = 0.5),
                   axis.line.y = element_line(color="black", size = 0.5))



