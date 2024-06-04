#EP su comunity
#Biodiversity Monitoring South Tyrol - Ephemeroptera, Plecoptera Taxa only

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(vegan)
library(reshape2)
library(openxlsx)

setwd("C:/Users/fvallefuoco/OneDrive - Scientific Network South Tyrol/VaF/R")
#load the data - TOT SPECIES/TAXA
data_bms<-read_excel("No_ambiguous_sp_BMS.xlsx")

# see some character cols
unique(data_bms$dwc.habitat.1.) # some capitalization issues 
data_bms$dwc.habitat.1. <- str_to_lower(data_bms$dwc.habitat.1., locale = "en")
unique(data_bms$dwc.habitat.1.)

data_bms <- data_bms %>% mutate(Taxa = paste(Order, Family, Subfamily, Genus, Species, sep = "_")) 


# add two new columns splitting the eventID variables into "category" and "subsample" information
data_bms <-cbind(colsplit(data_bms$dwc.eventID, "_", c("category", "subsample")),data_bms)



### EXTRACT REFERENCE DATA POINTS ---------------------------------------------
data_bms$code <- paste(data_bms$year, data_bms$dwc.eventID, sep = "_")
sort(unique(data_bms$code))

######stream categories with all 10 sites collected

points90 <- c("2021_31162_1", "2021_31162_10", "2021_31162_2" , "2021_31162_3" , "2021_31162_4" , "2021_31162_5" , "2021_31162_6" , "2021_31162_7" ,
                  "2021_31162_8",  "2021_31162_9","2021_32231_1" , "2021_32231_10", "2021_32231_2"  ,"2021_32231_3" , "2021_32231_4" , "2021_32231_5", 
                  "2021_32231_6",  "2021_32231_7" , "2021_32231_8",  "2021_32231_9" , "2021_32352_1" , "2021_32352_10" ,"2021_32352_2" , "2021_32352_3" ,
                  "2021_32352_4" , "2021_32352_5" , "2021_32352_6",  "2021_32352_7" , "2021_32352_8" , "2021_32352_9" ,"2022_12352_1" , "2022_12352_10",
                  "2022_12352_2" , "2022_12352_3" , "2022_12352_4" , "2022_12352_5" , "2022_12352_6" , "2022_12352_7" , "2022_12352_8" , "2022_12352_9",
                  "2022_32232_1" , "2022_32232_10", "2022_32232_2" , "2022_32232_3" , "2022_32232_4"  ,"2022_32232_5" , "2022_32232_6" ,"2022_32232_7",
                  "2022_32232_8" , "2022_32232_9", "2022_33132_1",  "2022_33132_10" ,"2022_33132_2" , "2022_33132_3", "2022_33132_4" , "2022_33132_5",
                  "2022_33132_6" , "2022_33132_7" , "2022_33132_8" , "2022_33132_9", "2023_13133_1",  "2023_13133_10" ,"2023_13133_2" , "2023_13133_3" , 
                  "2023_13133_4",  "2023_13133_5" , "2023_13133_6" ,"2023_13133_7",  "2023_13133_8",  "2023_13133_9",  "2023_32132_1",  "2023_32132_10",
                  "2023_32132_2",  "2023_32132_3" , "2023_32132_4" ,"2023_32132_5" , "2023_32132_6" , "2023_32132_7" , "2023_32132_8",  "2023_32132_9", 
                  "2023_33232_1",  "2023_33232_10", "2023_33232_2" , "2023_33232_3"  ,"2023_33232_4",  "2023_33232_5" , "2023_33232_6",  "2023_33232_7" ,
                  "2023_33232_8" , "2023_33232_9" )
length(points90)


# split data frame
bms_90 <- data_bms [data_bms$code %in% points90, ] # select rows

#save the 90 site DF

write.xlsx(bms_90, 'data_bms90.xlsx')


###----------------------
#Create a new column with 3 groups: E- P - Others
unique(bms_90$Order)

bms_90 <- bms_90%>%
                       mutate( New_order= case_when(
                         Order %in% c("Plecoptera") ~ "P", 
                         Order %in% c("Ephemeroptera") ~ "E", 
                        # Order %in% c("C", "D", "E") ~ "EP",  # New group composed of EP
                         TRUE ~ "Others"  # All other categories of Order
                          )
                       )



####Different order in different categories####

#get vector with unique categories
categories <- levels(as.factor(bms_90$category)) #9 different categories in total 
#create unique color for each order 
Species_col<-bms_90$New_order
Species_col<-as.factor(Species_col)

# Define a vector of 3 easy-to-distinguish colors
palette <- c("#1f77b4", "#F5F5DC","#9edae5")
# Replicate the palette 3 times
levels(Species_col)<- palette

# Initialize a vector to store colors for each species
Species_col <- rep(NA, length(unique(bms_90$New_order)))
unique_species <- unique(bms_90$New_order)

# Assign colors to each species
set.seed(123)  # Set a seed for reproducibility
for (i in seq_along(unique_species)) {
  Species_col[i] <- palette[i %% length(palette) + 1]
}


png("Stream category EP-Others.png", width=20, height=16, res=600, unit="in")
#Pie chart
par(mfrow=c(3,3))


# Iterate through each category
for (cat in categories) {
  # Subset data for the current category
  category_data <- bms_90[bms_90$category == cat, ]
  
  # Initialize an empty vector to store sums of abundance
  sums <- numeric(length(unique(category_data$New_order)))
  pie_labels <- unique(category_data$New_order)
  
  # Iterate through each species
  for (i in seq_along(pie_labels)) {
    single_species <- category_data[category_data$New_order == pie_labels[i], ] 
    
    # Check for NA values and subset accordingly
    single_species <- single_species[!is.na(single_species$ab), ]
    
    # Calculate the sum of abundance for the current species
    sums[i] <- sum(single_species$ab)*2
  }
  
  # Create a pie chart using the sums of abundance
  pie(sums, labels = pie_labels, main = paste("Abundance by Order in", cat),cex.main = 3,
      col=Species_col[match(pie_labels, unique_species)], cex=2.5)
}

#while (!is.null(dev.list()))  
dev.off()



####Different order in different substrates####

#get vector with unique categories
categories <- levels(as.factor(bms_90$dwc.habitat.1.)) #11 different categories in total 
#create unique color for each order 
Species_col<-bms_90$New_order
Species_col<-as.factor(Species_col)

# Define a vector of 3 easy-to-distinguish colors
palette <- c("#1f77b4", "#F5F5DC","#9edae5")
# Replicate the palette 3 times
levels(Species_col)<- palette

# Initialize a vector to store colors for each species
Species_col <- rep(NA, length(unique(bms_90$New_order)))
unique_species <- unique(bms_90$New_order)

# Assign colors to each species
set.seed(123)  # Set a seed for reproducibility
for (i in seq_along(unique_species)) {
  Species_col[i] <- palette[i %% length(palette) + 1]
}


png("Substrate EP-Others.png", width=20, height=16, res=600, unit="in")
#Pie chart
par(mfrow=c(3,4))


# Iterate through each category
for (cat in categories) {
  # Subset data for the current category
  category_data <- bms_90[bms_90$dwc.habitat.1. == cat, ]
  
  # Initialize an empty vector to store sums of abundance
  sums <- numeric(length(unique(category_data$New_order)))
  pie_labels <- unique(category_data$New_order)
  
  # Iterate through each species
  for (i in seq_along(pie_labels)) {
    single_species <- category_data[category_data$New_order == pie_labels[i], ] 
    
    # Check for NA values and subset accordingly
    single_species <- single_species[!is.na(single_species$ab), ]
    
    # Calculate the sum of abundance for the current species
    sums[i] <- sum(single_species$ab)*2
  }
  
  # Create a pie chart using the sums of abundance
  pie(sums, labels = pie_labels, main = cat, cex.main = 3,
      col=Species_col[match(pie_labels, unique_species)], cex=2.5)
}

#while (!is.null(dev.list()))  
dev.off()



