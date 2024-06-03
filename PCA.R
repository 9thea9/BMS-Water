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

data<-read.csv(diversity_data, "BMS all_diversity.csv")
