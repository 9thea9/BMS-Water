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


#load needed dataframe, depending on question 

data<-read.csv("BMS sites all.csv")

#create vector with names to loop through 
names<-c("pH.value", "ORP..mV.", "conductivity..µs.cm.",    "turbidity..FNU.","temperature...C.", 
         "DO....",  "DO..mg.L.", "total.Nitrogen..mg.L.", "total.Phosphorus..mg.L.",  "Orthophosphate..mg.L.", 
         "Ammonium..mg.L.", "Nitrate..mg.L.", "Nitrite..mg.L.")  
#12 colors for the six different categories present in vinschgau 
col_vector<-c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
              "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

#To check normal distribution and variance
#also possible to plot each category separately 

#png("boxplots_abiotic_variance_all.png", width=16, height=10, units="in", res=500)
par(mfrow=c(3,5))
# Loop through each column in y_values
for (i in seq_along(names)) {
  
  
  # Create a boxplot for each column
  
  boxplot(data = data, data[[names[i]]], 
          main=names[i],
          ylab=names[i], xaxt="n", xlab=" ")
  
  
}  
dev.off()#to safe the plot 


#png("boxplots_abiotic_all.png", width=16, height=10, units="in", res=500)
par(mfrow=c(3,5))
# Loop through each column in y_values
for (i in seq_along(names)) {
  
  
  # Create a boxplot for each column
  
  plot(data = data, data[[names[i]]] ~ as.factor(category), 
       main=names[i],
       ylab=names[i], col=col_vector, xaxt="n", xlab=" ")
  # Rotate x-axis labels
  axis(1,  at= 1:11, labels = unique(data$category), las = 2)
  
}        
dev.off()

#png("histogram_all.png", width=16, height=10, units="in", res=500)
par(mfrow=c(3,5))
# Loop through each column in y_values
for (i in seq_along(names)) {
  
  
  # Create a boxplot for each column
  
  hist(data[[names[i]]], 
          main=names[i],
          ylab=names[i], xaxt="n", xlab=" ")
  
  
}  
dev.off()#to safe the plot 
#if needed - do some data transformation 

###The skewness coefficient can be computed using the moments R packages:
library(moments)

#The direction of skewness is given by the sign of the skewness coefficient:
#A zero means no skewness at all (normal distribution).
#A negative value means the distribution is negatively skewed.
#A positive value means the distribution is positively skewed.

#calculate The skewness coefficient for our variables
s.coeff <-rep(NA, length(names))

for (i in seq_along(names)) {
  s.coeff[i] <- skewness(data[[names[i]]], na.rm = TRUE)
} 


###Transformation methods
#--> square-root for moderate skew:
#sqrt(x) for positively skewed data,
#sqrt(max(x+1) - x) for negatively skewed data
#--> log for greater skew:
#log10(x) for positively skewed data,
#log10(max(x+1) - x) for negatively skewed data
#--> inverse for severe skew:
#1/x for positively skewed data
#1/(max(x+1) - x) for negatively skewed data
