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

data<-read.csv("BMS sites all.csv")
col_vector<-c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

data$ID_year<-paste(data$year, data$dwc.eventID, sep=" ")
names(data)

new<-data[c(58, 51, 31)]%>% #select point names, taxon and sample size value # and family and land use
  dplyr::group_by(ID_year, taxon) %>%
  dplyr::summarise(n = dplyr::n())


new_wide<-as.data.frame(pivot_wider(new, names_from = taxon, values_from = n))
new_wide[is.na(new_wide)] <- 0 

rownames(new_wide)<-new_wide$ID_year
new_wide<-new_wide[-1]


species_richness <- new %>%  group_by(as.factor(ID_year)) %>%
  summarise(length(unique(taxon))) # Taxa richness per micro habitat 
new<-new%>%drop_na()
new_wide<-pivot_wider(data=new, names_from=taxon, values_from = n)


#replace NA with 0
new_wide[is.na(new_wide)] <- 0
new_wide[, -1] <- lapply(new_wide[, -1], as.numeric)

#safe names for later
names<-new_wide$ID_year
new_wide<-as.data.frame(new_wide)
rownames(new_wide)<-new_wide$ID_year

new_wide<-new_wide[-1]#dont use first column with names
new_wide$totalab<-rowSums(new_wide)
#now realtive abundance
totalab<-new_wide$totalab
new_wide_relative<-sapply(new_wide[, -ncol(new_wide)], function(x) x / totalab)
new_wide_relative<-data.frame(new_wide_relative)

#calculate shannon index
new_wide_relative$shannon<-diversity(new_wide_relative, index = "shannon")
new_wide_relative$ID_year<-names

diversity<-new_wide_relative[c(245, 246)] #select last two columns of the dataframe


diversity_data<-inner_join(diversity, data)

#write.csv(diversity_data, "BMS all_diversity.csv")

names(new_wide_relative)

new_wide_relative <- new_wide_relative %>% mutate (category= ID_year)%>%
  mutate(category = str_remove(category, "_.*")) %>%
  mutate(category = str_remove(category, "^[^ ]* "))

new_wide_relative$category<-as.factor(new_wide_relative$category)

species_richness<-species_richness %>% mutate (category= `as.factor(ID_year)`)%>%
  mutate(category = str_remove(category, "_.*")) %>%
  mutate(category = str_remove(category, "^[^ ]* "))

#for plotting 
new_wide_relative$col<-as.factor(new_wide_relative$category)
levels(new_wide_relative$col)<-as.factor(col_vector)
col<-as.character(new_wide_relative$col)

#png("shannon_speciesrichness_all.png", width=8, height=12, units="in", res=600)
par(mfrow=c(3,1))

barplot(
  height = new_wide_relative$shannon,
  names.arg =names,
  cex.names = 0.8, 
  ylim = c(0, 7), 
  col = col,  # Apply colors
  las = 2  # Make the names vertical (optional, for better readability if needed)
)

levels(species_richness$category)<-col_vector

barplot(
  height = species_richness$`length(unique(taxon))`,
  names.arg = species_richness$`as.factor(ID_year)`,
  cex.names = 1, 
  ylim = c(0, 50), 
  col = col,  # Apply colors
  las = 2  # Make the names vertical (optional, for better readability if needed)
)


plot(new_wide_relative$shannon, species_richness$`length(unique(taxon))`, 
     col=col, pch=16, 
     ylab="Species Richness", xlab="Shannon diversity", cex=2)
text(new_wide_relative$shannon+0.1, species_richness$`length(unique(taxon))`+1, lab=species_richness$`as.factor(ID_year)`, cex=1)
dev.off()
