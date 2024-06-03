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

###RDA---------------------------------------------------------
names(data)
chemistry_all<-data%>% group_by(ID_year, category)%>%
  summarise(pH=mean(pH.value), ORP=mean(ORP..mV.), cond=mean(conductivity..µs.cm.), 
            DO=mean(DO....), DOmg=mean(DO..mg.L.), Ntot=mean(total.Nitrogen..mg.L.),Nitrite=mean(Nitrite..mg.L.), OrthoP=mean(Orthophosphate..mg.L.),
            turb=mean(turbidity..FNU.), temp=mean(temperature...C.), Ptot=mean(total.Phosphorus..mg.L.), Nitrate=mean(Nitrate..mg.L.))
chemistry_all<-as.data.frame(chemistry_all)%>%
  drop_na()
rownames(chemistry_all)<-chemistry_all$ID_year
names(chemistry_all)
category<-chemistry_all$category
chemistry<-chemistry_all[-c(1,2)]#without names and values that dont make sense (check generel assumptions)

species_matrix<-data[c(3, 54, 34)]%>% #select point names, taxon and sample size value # and family and land use
  dplyr::group_by(ID_year, taxon) %>%
  dplyr::summarise(n = dplyr::n())

species<-pivot_wider(data=species_matrix, names_from = taxon, values_from = n)

#replace NA with 0
species[is.na(species)] <- 0
species<-as.data.frame(species)
rownames(species)<-species$ID_year
species[,-1] <- lapply(species[, -1], as.numeric)
species<-species[-1]


zwc<-scale(species)
rda<-rda(zwc~., data=chemistry)
summary(rda)
RsquareAdj(rda)#redundancy statistic

#hypothesis tests
#testing the first axis
anova(rda)

anova(rda, first=T)

anova(rda,by="axis",model="direct",perm.max=9999,step=1000)

# testing the individual terms=constraints
anova(rda,by="terms",model="direct",perm.max=9999,step=1000)  # tests terms sequentially, order matters!

anova(rda,by="margin",model="direct",perm.max=9999,step=1000) # tests each term in full model (like drop1() function)

#Plot the Redundancy analyses
plot(rda, scaling=1)

(sites<-scores(rda,choices=c(1,2),display="sites",scaling=1)) 
(lcs<-scores(rda,choices=c(1,2),display="lc",scaling=1)) # fitted/constrained site scores
(species<-scores(rda,choices=c(1,2),display="sp",scaling=1))
(constraints<-scores(rda,choices=c(1,2),display="bp",scaling=1))

col_cat<-as.factor(category)
levels(col_cat)<-c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


plot(sites,asp=1,pch=16,col= as.character(col_cat), cex=2)
arrows(x0=0,y0=0,x1=constraints[,1]*2,y1=constraints[,2]*2,lwd=1.5,col="darkgrey", code=2)
text(constraints[,1:2]*2.1,label=rownames(constraints),pos=4,cex=0.8,col="black")


legend("topright", legend=unique(category), col=unique(as.character(col_cat)), pch=16)

#---------------CCA---------------------------
apply(species,1,sum)
vare.ca<-cca(X=species, Y=chemistry)
summary(vare.ca,scaling=1)

plot(vare.ca,scaling=2, xlab="CCA1 21.01%", ylab="CCA2 18.58%", ylim=c(-0.5, 1))

# Add species names to the plot
species_names <- colnames(species) # Assuming rownames of vinschgau contain species names
site_names<-rownames(chemistry)
text(vare.ca, display = "species", scaling = 2, labels = species_names, cex = 0.8, pos = 2)
text(vare.ca, display = "sites", scaling = 2, labels = site_names, cex = 0.8, pos = 3, col = "black")
dev.off()

##cluster-------------
cluster_data<-vegdist(species, method="bray")
cluster<-hclust(cluster_data, method="ward.D")

png("Cluster all_species.png", width = 14, height=8, unit="in", res=600)
plot(cluster, main="Sites clustering based on Community", cex=0.6) # color t, he labels in different colors
rect.hclust(cluster,k=7)
dev.off()
