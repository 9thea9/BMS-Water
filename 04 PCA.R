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

####PCA--------------------------------------------------------------------------------------
###
###
####
names(data)

##
#just select the numeric columns that make sense for PCA
abiotic_pca<-data[c(7:19)] #try without temperature because might have to strong influence
#eliminate total P, Nitrite (only two sites have measurements for this)
plot(abiotic_pca)#check what makes sense 
#Attention with P, N: values are very often to low to be detected
#can make a huge difference when one site has a value
names(abiotic_pca)

abiotic_pca<-na.omit(abiotic_pca)

#perform pca
pca<-prcomp(scale(abiotic_pca))
summary(pca)



#for plotting
#bring pca results in right format
head(scores<-pca$x/pca$sdev)
head(loadings<-pca$rotation)



#transform data - that it can be used for plot
data$category<-as.factor(data$category)
levels(data$category)

scores<-na.omit(scores)

data <- data %>%
  mutate(point_names = paste(`dwc.eventID`, sep="_", year))%>%
  mutate(cat_year=paste(category, sep="_", year))
point_names<-as.factor(data$point_names)

# Map years to pch values
data$shape<-(as.factor(data$year))
levels(data$shape)<-c(21, 24, 25)
data$year<-as.factor(data$year)

data$shape<-as.numeric(as.character(data$shape))

data$category<-as.factor(data$category)
levels(data$category)
data$col<-data$category

col_vector<-c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
levels(data$col)<-col_vector
data$col<-as.character(data$col)

#plot
dev.off()
#png("overviewabiotic.png", width = 8, height = 8, units = "in", res=400)  

#plot 1 - point in different colours
plot(pca$x[,1]/pca$sdev[1], pca$x[,2]/pca$sdev[2], bg=data$col,
     xlab="PC1 [30 %]", ylab="PC2 [25 %]", main="PCA Plot - sampling points in three years BMS", 
     pch=data$shape, cex=3, lwd=0.5)#change % according to PCA summary

text(pca$x[,1]/pca$sdev[1], pca$x[,2]/pca$sdev[2], label=data$dwc.eventID, cex=0.7)
# Creleft_join()# Create a legend
legend("bottom",                  # Position of the legend
       legend = levels(data$category), # Text in the legend
       col=col_vector,        # Colors
       pch = 16,                                      # Point type
       cex = 1.1)                                     # Font size of legend text
legend("bottomright", legend=levels(data$year), pch=c(21, 24, 25), pt.lwd=2)



arrows<-loadings*2 # with extension factor
arrows(x0=-0,y0=0,x1=arrows[,1],y1=arrows[,2],col="black", angle=50, lwd=2)
text(x=arrows[,1]*1.2,y=arrows[,2]*1.2,labels=names(abiotic_pca),cex=0.9, col="black")

dev.off()
