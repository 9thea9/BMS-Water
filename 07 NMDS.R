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

names(data)

data_subset<- data[c(3, 54, 52, 34, 57, 28)]%>%
  dplyr::group_by(ID_year, taxon, category, Order) %>%
  dplyr::summarise(n = dplyr::n())
#select point ID with year, abundance and taxon

data_subset_wide<-as.data.frame(pivot_wider(data_subset, names_from = taxon, values_from = n))

category<-data_subset_wide$category
point_names<-data_subset_wide$ID_year
rownames(data_subset_wide)<-point_names


data_subset_wide[is.na(data_subset_wide)] <- 0 



numbers<-category  
col_cat<-as.factor(numbers)
levels(col_cat)<-c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

category_df<-as.data.frame(numbers)
category_df$ID_year<-data_subset_wide$ID_year



#vinschgau_wider<-as.data.frame(vinschgau_wider)

data_nmds<-data_subset_wide[c(4:246)]


data_nmds <- as.data.frame(lapply(data_nmds, as.numeric))

taxon<-colnames(data_nmds)
#maybe not even needed 
data_invert<-t(data_nmds)
data_invert<-as.data.frame(data_nmds)

names(data)
Order<-data[c(28, 54, 57)]

Order<-Order%>%group_by(taxon)%>%
  reframe(Order=unique(Order), n= mean(ab))
Order$taxon <- gsub(" ", ".", Order$taxon)

species_names<-as.data.frame(colnames(data_invert))

colnames(species_names)<-c("taxon")

test<-inner_join(species_names, Order)
Order<-as.factor(test$Order)
levels(Order)<-c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2",
                 "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896",
                 "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5", "#1f77b4", 
                 "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
                 "#bcbd22", "#17becf", "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "grey",
                 "darkgreen", "darkred", "green3", "lightgreen")

#NMDS -----------------------------------------------
mds_data<-metaMDS(comm=data_invert, distance="bray", trymax=100,k=10)

mds_data$stress
stressplot(mds_data)

IDcol<-as.factor(pointID)
levels(IDcol)<- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499")




levels(fam_col)<-colors
abundance<-as.numeric(df_species_families$n)
#Plot NMDS vinschgau 
#png("NMDS all southtyrol.png", width=12, height=10 , unit="in", res=600)
plot(mds_data$points, pch=16, 
     cex= 2, col=as.character(Order)
)

text(mds_data$points, label=taxon, 
     pos=2, cex=0.9)
plot(envfit(mds_data$points, data_invert),  cex=0.8, lwd=3, col=as.character(col_cat))
ordiellipse(mds_data, groups = pointID, 
            draw = "polygon", col=unique(as.character(IDcol)), 
            label = F, lwd=0.5, alpha=0.2)

legend("bottomright", legend=unique(pointID), col=unique(as.character(IDcol)), pch=16, pt.cex=2)
dev.off()