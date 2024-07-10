# Load necessary libraries
library(dplyr)
library(tidyr)

#set working directory
setwd("C:/Users/tschwingshackl/OneDrive - Scientific Network South Tyrol/BMS")


#load needed dataframe
data<-read.csv("BMS sites all.csv")
names(data) #check



# Create a function to ensure each group has both "Ephemeroptera" and "Plecoptera"
ensure_orders <- function(data) {
  # Check if "Ephemeroptera" is present, if not add row with 0 value
  if (!"Ephemeroptera" %in% data$Order) {
    data <- data %>%
      add_row(Order = "Ephemeroptera", ab = 0, total_ab = 0) 
      
  }
  # Check if "Plecoptera" is present, if not add row with 0 value
  if (!"Plecoptera" %in% data$Order) {
    data <- data %>%
      add_row(Order = "Plecoptera", ab = 0, total_ab = 0) 
      
  }
  return(data)
}


# Apply the function to the whole dataframe
#if one group does not contain Ephemeroptera or Plecoptera, a row is added with abundance = 0 
result <- data %>%
  group_by(dwc.eventID, Subsample.ID, year, ID) %>%                # Group by site and subsample
  group_modify(~ ensure_orders(.x)) %>%        # Ensure each group has the required Orders
  ungroup()                                    # Ungroup to return a regular data frame


names(result)

#next, copy abiotic data to this new  rows 
#select values that need to be copied 
to_copy<-names(result[-c(1,4,25:30)])


# Identify rows with NA values
na_rows <- which(rowSums(is.na(result[, to_copy])) > 0)

# Group by ID and fill NA values in specified columns

filled_df <- result %>%
  group_by(ID, dwc.eventID) %>%
  mutate(across(all_of(to_copy), function(x) ifelse(is.na(x), first(na.omit(x)), x))) %>%
  ungroup()


#check if observations got lost or not 
identical(levels(data$ID), levels(filled_df$ID))

#last, just select rows with desired orders 
EP <- filled_df[filled_df$Order %in% c("Ephemeroptera", "Plecoptera"), ]


