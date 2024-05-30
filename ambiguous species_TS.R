#Biodiversity Monitoring South Tyrol
#Thea Schwingshackl, April 2024
#modified may 30 2024 


#load packages
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)


setwd("C:/Users/tschwingshackl/OneDrive - Scientific Network South Tyrol/BMS")

data<-read_excel("BMS_Water_Dataset.xlsx")%>%
  rename("ab"=`dws:sampleSizeValue`)


# Create a new column 'taxon' based on different conditions
#it takes the highest taxonomic level possible
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
        data$Order    ))))                     # Result 5: Paste Order (assuming Order is always available)
   

#split taxon into two identifiere -> to separate Species names (eg sp.)
data <- separate(data, taxon, into = c("Id1", "Id2"), sep = " ")
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




