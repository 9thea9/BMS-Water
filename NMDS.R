####biodiversity monitoring south tyrol - limnology
#Thea Schwingshackl
#May 24



library(dplyr)
library(tidyr)
library(vegan)
library(stringr)
library(iNEXT)
library(ggplot2)
library(tibble)
library(readxl)

setwd("C:/Users/tschwingshackl/OneDrive - Scientific Network South Tyrol/BMS")
data<-read.csv("matrix for nmds.csv")


#formatting
rownames(data)<-data$X
species<-data$X
#only numeric columns 
data<-data[c(2:82)]


data_invert<-t(data)
data_invert<-as.data.frame(data_invert)
sites<-rownames(data_invert)
#NMDS 1
mds_data <- metaMDS(comm = data, distance = "bray",  trymax = 500, k=10)
#NMDS 2
mds_data2<-metaMDS(comm=data_invert, distance="bray", trymax=500, k=10)

mds_data$stress

##Plot 1
plot(mds_data$points, pch=15, #ylim=c(-0.2, 0.2), 
     col=as.factor(species))
text(mds_data$points, label=species, 
     cex=0.8)
plot(envfit(mds_data$points, data), col="black")


#Plot 2 
plot(mds_data2$points, pch=15, #ylim=c(-0.2, 0.2)
     )
text(mds_data2$points, label=sites, 
     cex=0.8)
plot(envfit(mds_data2$points, data_invert), col="black")

##Vinschgau only---------------

vinschgau_complete<-read.csv("matrix_vinschgau_complete.csv")
vinschgau<-read.csv("matrix_vinschgau.csv")
#import also other data for coloring 

vinschgau[is.na(vinschgau)] <- 0
rownames(vinschgau)<-vinschgau$point_names

#just needed for different plotting options
family<-vinschgau_wider$Family
luc<-vinschgau_wider$code
category<-vinschgau_wider$category

vinschgau<-vinschgau_wider%>%reframe(
  group_by(as.factor(point_names))
)


vinschgau_wider<-as.data.frame(vinschgau_wider)

vinschgau<-vinschgau[c(3:80)]

#maybe not even needed 
data_invert<-t(vinschgau)
data_invert<-as.data.frame(data_invert)
species<-rownames(data_invert)

species<-as.data.frame(species)
vinschgau_complete_plotting<-cross_join(species, vinschgau_complete)
#NMDS 1
mds_data <- metaMDS(comm = vinschgau, distance = "bray",  trymax = 50, k=10)

#NMDS 2
mds_data2<-metaMDS(comm=data_invert, distance="bray", trymax=500, k=10)

mds_data$stress

##Plot 1
plot(mds_data$points, pch=16, #ylim=c(-0.2, 0.2), 
     cex=2
)

text(mds_data$points, label=rownames(vinschgau), 
     cex=0.8)
plot(envfit(mds_data$points, colnames(vinschgau)), col="black")


#Plot 2 
png("NMDS vinschgau.png", width=10, height=8, unit="in", res=600)
plot(mds_data2$points, pch=16, #ylim=c(-0.2, 0.2), 
     cex=1
)
text(mds_data2$points, label=species, 
     cex=0.8)
plot(envfit(mds_data2$points, data_invert), col="black")
dev.off()
     