####biodiversity monitoring south tyrol - limnology
#Thea Schwingshackl
#June 24

#load libraries -------------
library(dplyr)
library(tidyr)
library(vegan)
library(stringr)
library(iNEXT)
library(ggplot2)
library(tibble)
library(readxl)

  


#set working directory
setwd("C:/Users/tschwingshackl/OneDrive - Scientific Network South Tyrol/BMS")

data<-read.csv( "BMS all_diversity.csv")#edited dataset with including shannon diversity

data$dwc.habitat.1.<-tolower(data$dwc.habitat.1.)
levels(as.factor(data$dwc.habitat.1.))

names(data)
#select category/substrate, point name, taxa and abundance

substrate_rarefaction<-data[c(34, 25, 54)]%>% #select point names, taxon and sample size value # and family and land use
  dplyr::group_by(dwc.habitat.1., taxon) %>%
  dplyr::summarise(n = dplyr::n())



substrate_rare_wide<-pivot_wider(data=substrate_rarefaction, names_from=taxon, values_from = n)

#replace NA with 0
substrate_rare_wide[is.na(substrate_rare_wide)] <- 0
substrate_rare_wide[, -1] <- lapply(substrate_rare_wide[, -1], as.numeric)


names<-substrate_rare_wide$dwc.habitat.1.

substrate_rare_wide<-as.data.frame(substrate_rare_wide[-1])

substrate_rare_long<-t(substrate_rare_wide)
substrate_rare_long<-as.data.frame(substrate_rare_long)
colnames(substrate_rare_long)<-paste (names)

#totalrarefaction<-totalrarefaction[7]
iNEXT_total<-iNEXT(substrate_rare_long, q=1)#q=1 shannon index, takes a long time

#colorblind-friendly palette
color<-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#009E73", "#DCEBF9")


#png("rarefaction_substrates_all.png", width=14, height=8, unit="in", res=600)

ggiNEXT(iNEXT_total, type=1)+
  scale_color_manual(values=color)+
  scale_fill_manual(values=color)+
  theme_minimal()
dev.off()

