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

data<-read_excel("BMS_Water_Dataset.xlsx")#from BMS home, scientific share point

#create column with just category
data<-data%>%
  mutate(category=`dwc:eventID`)%>%
  mutate(category = str_remove(category, "_.*"))

data$year_cat<-paste(data$year, data$category)

# Create a new column 'taxon' based on conditions
data$taxon <- ifelse(
  !is.na(data$Species),                    # Condition 1: If Species is not NA
  data$Species,                            # Result 1: Paste Species
  ifelse(
    !is.na(data$Genus),                    # Condition 2: If Genus is not NA
    data$Genus,                            # Result 2: Paste Genus
    ifelse(
      !is.na(data$Subfamily),              # Condition 3: If Subfamily is not NA
      data$Subfamily,                      # Result 3: Paste Subfamily
      ifelse(
        !is.na(data$Family),               # Condition 4: If Family is not NA
        data$Family,                       # Result 4: Paste Family
        data$Order                         # Result 5: Paste Order (assuming Order is always available)
      )
    )
  )
)

data$split_taxon<-data$taxon
data$ab<-data$`dws:sampleSizeValue`

#resolve ambiguous species ------
#split taxon into two identifiere -> to separate Species names (eg sp.)
data <- separate(data, split_taxon, into = c("Id1", "Id2"), sep = " ")
data$Id2<-as.factor(data$Id2)


# Get unique combinations of dwc.eventID, year, and Genus
unique_combinations <- unique(data[, c("ID",  "Id1")])

# Initialize an empty list to store the results
result_list <- list()

# Loop over each unique combination
for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination
  subset_data <- data %>%
    filter(ID == current_combination$ID,
           Id1 == current_combination$Id1)
  
  # Check the number of rows in the subset and if there are any "sp." species and not NA values
  if (nrow(subset_data) > 1 & any(subset_data$Id2 == "sp." & !is.na(subset_data$Id2))) {
    # Calculate total abundance for the subset excluding "sp." or "juv." species
    subset_data$total_ab <- sum(subset_data$ab[subset_data$Id2 != "sp."])
  } else {
    # If there's only one row or no rows in the subset, set total_ab to NA
    subset_data$total_ab <- subset_data$ab
  }
  
  # Calculate relative abundance for the subset
  subset_data <- subset_data %>%
    mutate(rel_ab = ab / total_ab)
  
  if (nrow(subset_data) > 1 & any(subset_data$Id2 == "sp." & !is.na(subset_data$Id2))) {
    subset_data$new_ab= subset_data$ ab [subset_data$Id2== "sp." ] * subset_data$rel_ab + subset_data$ab
  } else {
    subset_data$new_ab= subset_data$ab
  }
  
  
  
  # Check the number of rows in the subset
  if (nrow(subset_data) > 1 & any(subset_data$Id2 == "sp." & !is.na(subset_data$Id2))) {
    # Remove rows containing "sp." or "(juv.)" species
    subset_data <- subset_data %>%
      filter(!(subset_data$Id2 == "sp."))
  }
  
  
  
  # Store the result in the list
  result_list[[i]] <- subset_data
}

# Combine the results into a single dataframe
result <- do.call(rbind, result_list)





#select sites, reference only once in 4 years, own dataset with reference sites --------
yearly_points <- c(#for 2021
                  "2021 12352", "2021 33132", "2021 32232", #sampled 22
                   "2021 13133", "2021 33232", "2021 32132", #sampled 23
                   "2021 13232", "2021 33131", "2021 32131", #sampled 24 
                   #for 2022
                   "2022 32352", "2022 31162", "2022 32231", #sampled 21
                   "2022 13133", "2022 33232", "2022 32132", #sampled 23
                   "2022 13232", "2022 33131", "2022 32131",#sampled 24
                   #for 2023
                  "2023 32352", "2023 31162", "2023 32231", #sampled 21
                  "2023 12352", "2023 33132", "2023 32232", #sampled 22
                  "2023 13232", "2023 33131", "2023 32131",#sampled 24
                  #for 2024
                  "2024 32352", "2024 31162", "2024 32231", #sampled 21
                  "2024 12352", "2024 33132", "2024 32232", #sampled 22
                  "2024 12352", "2024 33132", "2024 32232", #sampled 23
                   ) 

data_all <- result [!(result$year_cat %in% yearly_points), ] # select rows
data_reference<-result [(result$year_cat %in% yearly_points), ] # select rows

#safe dataframes for further analysis
write.csv(data_all, "BMS sites all.csv" )
write.csv(data_reference, "BMS sites references.csv")

#optional -> just select EP species 
EP<-data_all%>% subset(Order==c("Ephemeroptera", "Plecoptera"))
write.csv(EP, "BMS EP only.csv")
