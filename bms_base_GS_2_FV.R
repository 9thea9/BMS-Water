### EURAC BMS 
### GREGOR SEIFERT
### DATA ANALYSIS 

# install.packages("tidyverse")
library(tidyverse)


setwd("C:/Users/grego/Documents/Studium/EMMA_BZ/EURAC/")

### DATA IMPORT AND CLEANING --------------------------------------------------

data_bms <- read.table("Base_data_og/BMS_Water_Dataset.csv", sep = ";", dec = ".",  header = T, encoding = "UTF-8") # read table
data_bms <- data_bms %>% mutate(dwc.eventDate = as.Date(dwc.eventDate)) # make date usable

# see some character cols
unique(data_bms$dwc.habitat.1.) # some capitalization issues 
data_bms$dwc.habitat.1. <- str_to_lower(data_bms$dwc.habitat.1., locale = "en")
unique(data_bms$dwc.habitat.1.) # better 

# check for others 
length(unique(data_bms$Order)) == length(unique(str_to_lower(data_bms$Order, locale = "en")))
length(unique(data_bms$Family)) == length(unique(str_to_lower(data_bms$Family, locale = "en")))
length(unique(data_bms$Subfamily)) == length(unique(str_to_lower(data_bms$Subfamily, locale = "en")))
length(unique(data_bms$Genus)) == length(unique(str_to_lower(data_bms$Genus, locale = "en")))
length(unique(data_bms$Species)) == length(unique(str_to_lower(data_bms$Species, locale = "en")))
# all good 

unique(data_bms$Species) # blanks could lead to confusion
length(unique(data_bms$Order)) == length(unique(str_squish(data_bms$Order)))
length(unique(data_bms$Family)) == length(unique(str_squish(data_bms$Family))) # False
length(unique(data_bms$Subfamily)) == length(unique(str_squish(data_bms$Subfamily)))
length(unique(data_bms$Genus)) == length(unique(str_squish(data_bms$Genus)))
length(unique(data_bms$Species)) == length(unique(str_squish(data_bms$Species))) # False
# remove blanks 
data_bms$Family <- str_squish(data_bms$Family) 
data_bms$Species <- str_squish(data_bms$Species)
# test again
length(unique(data_bms$Family)) == length(unique(str_squish(data_bms$Family))) 
length(unique(data_bms$Species)) == length(unique(str_squish(data_bms$Species)))
# all good now 

# replace blank cells with NA 
for (i in 1:ncol(data_bms)) {
  data_bms[which(data_bms[,i]==""),i] <- NA
}
# head(data_bms) # voila

# add Taxa col
data_bms <- data_bms %>% mutate(Taxa = paste(Order, Family, Subfamily, Genus, Species, sep = "_")) 

##FV Thea's proposed method would be better:
# Create a new column 'taxon' based on different conditions
#it takes the highest taxonomic level possible
data_bms$taxon <- ifelse(
  !is.na(data_bms$Species),                    # Condition 1: If Species is not NA
  data_bms$Species,                            # Result 1: Paste Species
  ifelse(
    !is.na(data_bms$Genus),                    # Condition 2: If Genus is not NA
    data_bms$Genus,                            # Result 2: Paste Genus
    ifelse(
      !is.na(data_bms$Subfamily),              # Condition 3: If Subfamily is not NA
      data_bms$Subfamily,                      # Result 3: Paste Subfamily
      ifelse(
        !is.na(data_bms$Family),               # Condition 4: If Family is not NA
        data_bms$Family,                       # Result 4: Paste Family
        data_bms$Order      ))))               # Result 5: Paste Order (assuming Order is always available)


### EXTRACT REFERENCE DATA POINTS ---------------------------------------------
yearly_points <- c("12352_1", "12352_2", "32232_1", "32232_2", "33132_1", "33132_2", "13133_1", "13133_2", "13232_1", "13232_2", "31162_1", "31162_2", 
                  "32131_1", "32131_2", "32132_1", "32132_2", "32231_1", "32231_2", "32352_1", "32352_2", "33131_1", "33131_2", "33232_1", "33232_2")

# split data frame
bms_ref <- data_bms [data_bms$dwc.eventID %in% yearly_points, ] # select rows
bms_filtered <- data_bms [!(data_bms$dwc.eventID %in% yearly_points), ] # select rows

# sum of rows from multi-year and annual data should equal the input table
isTRUE(nrow(bms_ref) + nrow(bms_filtered) == nrow(data_bms))


### Averages Reference Samples ------------------------------------------------

# TODO: only keep specific combinations of species for each sampling point 
# >> averages for numerical values <- all samples 


### merge lines & condense Data 
### IDEA: 
# 1 get all species in specific sampling points 
# 2 assign averages of numeric values to condensed lines accordingly


# get all cols 
bms_ref %>% group_by(dwc.eventID, Taxa) %>% 
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = T))) # good, but missing numerical variables 
# not precessed variables: ID , Subsample.ID , dwc.eventDate , dwc.habitat.1. , dwc.habitat.2. 
  # >> ID can be recreated after

#MV# should be considered to work with long database (see "longtowirde" R-Skript with a unique taxa name/taxa ID) 

  # >> Subsample and habitat 1 are the same, would be good to keep anyways maybe, just add to group_by

#FV# n.b. Add the microhabitat category (or habitat 2) or dwc.eventID as the variables to be grouped by, it depends on what you are looking at: the mean abundance by site or by substrate?

  # >> event Date should be what, the first one? >> add to summarize
#FV# Perhaps event Date not important for further analysis mainly if you use the average

  # >> habitat 2 should be converted to numeric, solving the problem 
bms_ref_new <- bms_ref %>% mutate(dwc.habitat.2_num = suppressWarnings(as.numeric(dwc.habitat.2.))) # causing problem

#FV#  # Applying suppressWarnings function should work

#MV# for dwc.habitat.2 (current speed [cm/s]), and/or the percentage of the substrate coverage (%) "<5" needs to be changed to "5" !
bms_ref_new$dwc.habitat.2_num[is.na(bms_ref_new$dwc.habitat.2_num)] <- 5
any(is.na(bms_ref_new$dwc.habitat.2_num)) # good, works numerical now 
bms_ref_new <- bms_ref_new %>% select(-dwc.habitat.2.) %>% rename(dwc.habitat.2. = dwc.habitat.2_num) # replace old col with mutated one (could be shortened later)

# check if Date exists in df
nrow(data_bms[data_bms$dwc.eventDate == as.Date("2020-06-01"),]) # >> no, can be used as outstanding

### make data frame smaller by removing double measurements 
# averaging all numerical values of groups 
bms_ref_avg <- bms_ref_new %>% group_by(dwc.eventID, Subsample.ID, dwc.habitat.1., Order, Family, Subfamily, Genus, Species, Taxa) %>% 
  summarize(across(where(is.numeric), ~ mean(.x , na.rm = T)), dwc.eventDate = as.Date("2020-06-01")) %>% ungroup() 
# make years int style 
bms_ref_avg$year <- round(bms_ref_avg$year, 0) 


# create ID for each line
bms_ref_avg <- bms_ref_avg %>% mutate(ID = paste(year, "_", str_replace(dwc.eventID, "_", ""), "_", Subsample.ID)) # construct ID
bms_ref_avg$ID <- str_replace_all(bms_ref_avg$ID, " ", "") # remove spaces, why did they even appear? 
#MV# sometimes happens in Excel when transposing the data 

# reorder for merging back into original data & concert to df
bms_ref_avg <- bms_ref_avg %>% select(names(bms_filtered)) %>% as.data.frame()
rm(bms_ref_new, bms_ref) # temporary working dfs can be deleted


### merge data back -----------------------------------------------------------
# adding reference point-data back to table of values that are sampled every 4 years

bms_filtered <- bms_filtered %>% mutate(dwc.habitat.2_num = as.numeric(dwc.habitat.2.)) # causing problem
#FV#  # As above, applying suppressWarnings function should works
#MV# for dwc.habitat.2 "<5" can be changed to "5" and does not need to be highlighted; (for context: only numbers higher than 5 are possible here!)
bms_filtered$dwc.habitat.2_num[is.na(bms_filtered$dwc.habitat.2_num)] <- 5 # change values smaller than 5
bms_filtered <- bms_filtered %>% select(-dwc.habitat.2.) %>% rename(dwc.habitat.2. = dwc.habitat.2_num) # rename column 

# add tables back together for getting a working data frame 
bms_workhorse <- bms_filtered %>% bind_rows(bms_ref_avg)




# Calculating basic measures --------------------------------------------------

### Taxa richness -----------
bms_workhorse %>% group_by(dwc.habitat.1.) %>% summarise(length(unique(Taxa))) %>% ungroup() # Taxa richness per micro habitat 
length(unique(bms_workhorse$Taxa)) # number of species overall 



### read taxa list and calculate feeding group related base stats -------------

bms_taxalist <- read.table("Base_data_og/BMS_Water_Taxalist.csv", sep = ";", dec = ".",  header = T, encoding = "UTF-8") # read table

# replace blank cells with NA 
for (i in 1:ncol(bms_taxalist)) {
  bms_taxalist[which(bms_taxalist[,i]==""),i] <- NA
}

# test on spelling and spaces 
all(unique(bms_taxalist$Order) == unique(str_to_title(bms_taxalist$Order, locale = "en")), na.rm = T)
all(unique(bms_taxalist$Family) == unique(str_to_title(bms_taxalist$Family, locale = "en")), na.rm = T)
all(unique(bms_taxalist$Subfamily) == unique(str_to_title(bms_taxalist$Subfamily, locale = "en")), na.rm = T)
all(unique(bms_taxalist$Genus) == unique(str_to_title(bms_taxalist$Genus, locale = "en")), na.rm = T)
all(unique(bms_taxalist$Species) == unique(str_to_sentence(bms_taxalist$Species, locale = "en")), na.rm = T) # to "sentance" instead of "title" for species 

length(unique(bms_taxalist$Order)) == length(unique(str_squish(bms_taxalist$Order)))
length(unique(bms_taxalist$Family)) == length(unique(str_squish(bms_taxalist$Family))) 
length(unique(bms_taxalist$Subfamily)) == length(unique(str_squish(bms_taxalist$Subfamily)))
length(unique(bms_taxalist$Genus)) == length(unique(str_squish(bms_taxalist$Genus)))
length(unique(bms_taxalist$Species)) == length(unique(str_squish(bms_taxalist$Species))) 
# all good 

bms_taxalist <- bms_taxalist %>% mutate(Taxa = paste(Order, Family, Subfamily, Genus, Species, sep = "_")) # add Taxa col
bms_taxalist <- bms_taxalist %>% filter(!(is.na(Family))) # remove NA lines 
bms_taxalist %>% filter((is.na(Grazer))) # ?? # lets drop it
#FV # Do you mean that there are no info about Feeding habits for the Dipetera Trichoceridae? Deleting it now may cause problems for the matching phase (we can consider it for a specific traits analysis)
bms_taxalist <- bms_taxalist %>% filter(!(is.na(Grazer))) # remove NA lines 

# see how the Taxa cols match
all(bms_workhorse$Taxa %in% bms_taxalist$Taxa)
unique(bms_workhorse$Taxa[(bms_workhorse$Taxa %in% bms_taxalist$Taxa)]) # Taxa from samples that are in the "Taxalist" table 
unique(bms_workhorse$Taxa[!(bms_workhorse$Taxa %in% bms_taxalist$Taxa)]) # not in the table 

# check the other way around, just to know
all(unique(bms_taxalist$Taxa) %in% unique(bms_workhorse$Taxa)) 
unique(bms_taxalist$Taxa[!(bms_taxalist$Taxa %in% bms_workhorse$Taxa)]) # not in the data 
## >> no match, too bad.. what now? 

## >> filter dataset 
bms_workhorse <- bms_workhorse %>% filter(Taxa %in% bms_taxalist$Taxa) # filter rows: drop samples where Taxa is not in Taxalist-Table 
all(bms_workhorse$Taxa %in% bms_taxalist$Taxa) # check, all in 

# join tables to get info from Taxalist
bms_workhorse <- bms_workhorse %>% left_join(bms_taxalist, join_by(Taxa)) 

# calculate species abundance per habitat ----------- 
bms_workhorse %>% group_by(dwc.habitat.1., Taxa) %>% summarize(mean(dwc.sampleSizeUnit)) %>% ungroup() # on standardized value (?)

# calculate feeding group abundance per habitat -----------

# get feed group, can be multiple values 
Feed_1 <- bms_workhorse %>% select(Grazer:Other) %>% max.col(ties.method = "first")
Feed_2 <- bms_workhorse %>% select(Grazer:Other) %>% max.col(ties.method = "last")
bms_workhorse <- bms_workhorse %>% mutate(Feed_group = colnames(bms_workhorse)[Feed_1+53]) # add feed group 
bms_workhorse <- bms_workhorse %>% mutate(Feed_group_2 = colnames(bms_workhorse)[Feed_2+53]) # add second feed group 
bms_workhorse %>% filter(Feed_group != Feed_group_2) %>% select(54:65) # have a look 

# feeding group abundance
bms_workhorse %>% group_by(dwc.habitat.1., Feed_group) %>% summarize(mean(dwc.sampleSizeUnit)) %>% ungroup() # abundance on standardized value (?)
#FV# Try to work on relative abundance too :)

## maybe change structure: if two feed groups, make mixed? 
bms_workhorse$Feed_group_2[bms_workhorse$Feed_group != bms_workhorse$Feed_group_2] <- "Multi feeders" # adjust values if feed 1 and feed2 are different 
#FV# Not sure that the use of a Multi feeders group is ecologically correct (maybe you lose some info that way), these feeding habits association should be weighted by the abundance  
#FV# I usually used the functcomp() function but feel free to use other functions as well.  

# better way for feeding group abundance 
bms_workhorse %>% group_by(dwc.habitat.1., Feed_group_2) %>% summarize(mean(dwc.sampleSizeUnit)) %>% ungroup() # abundance on standardized value (?)




# shannon-index -----------
library(vegan)

### create table in wide format
abundance_df <- bms_workhorse %>% select(dwc.habitat.1., dws.sampleSizeValue, Taxa) %>% group_by(dwc.habitat.1., Taxa) %>%
  summarize(sum_sample_size = sum(dws.sampleSizeValue)) %>% ungroup() %>%
  pivot_wider(id_cols = dwc.habitat.1., names_from = Taxa, values_from = sum_sample_size)

# replace NA cells with 0 (diversity function can not handle NAs) 
for (i in 2:ncol(abundance_df)) {
  abundance_df[which(is.na(abundance_df[,i])),i] <- as.numeric(0)
}

# calculate shannon diversity 
abundance_df$shannon_values_habitats <- as.vector(abundance_df %>% select(-1) %>% diversity(index = "shannon")) 


# eveness-index  -----------

n_spacies <- NULL
for (i in 1:nrow(abundance_df)) {
  n_spacies[i] <- length(which(abundance_df[i,2:212] != 0))
}
n_spacies # see values 

# add calculated values to df 
abundance_df <- abundance_df %>% bind_cols(n_species = n_spacies)
abundance_df$Eveness <- abundance_df$shannon_values_habitats / log(abundance_df$n_species) # calculate Eveness 
abundance_df$Eveness # see values 



# EPT% (share of Ephemerottera, Plecoptera, Trichoptera) ----------- 

# create data frame with calculated values and columns of EPT species 
abundance_EPT <- cbind(abundance_df[,c(1,213:215)], abundance_df[,colnames(abundance_df)[grepl("lecoptera|richoptera|phemeroptera", colnames(abundance_df))]])

n_EPT <- NULL
for (i in 1:nrow(abundance_EPT)) {
  n_EPT[i] <- length(which(abundance_EPT[i,5:134] != 0))
}
n_EPT # see values 

# add calculated EPT numbers to df 
abundance_df <- abundance_df %>% bind_cols(n_EPT_species = n_EPT) 
# calculate share in % and order df 
abundance_df <- abundance_df %>% mutate(EPT_percent = n_EPT_species/n_species) %>% select (dwc.habitat.1., shannon_values_habitats, n_species, Eveness, n_EPT_species, EPT_percent, everything())
abundance_df[,1:6]

# get rid of temporal vlaues 
rm(n_EPT, n_spacies, abundance_EPT)

### Graphics ---------------------------
library(ggplot2)

# taxa richness 
ggplot(data = bms_workhorse %>% group_by(dwc.habitat.1.) %>% 
         summarise(n_taxa = length(unique(Taxa))) %>% ungroup(), 
       aes(x = dwc.habitat.1., y = n_taxa)) +
  geom_bar(stat = "identity") 
# Shannon
ggplot(data = abundance_df, aes(x = dwc.habitat.1., y = shannon_values_habitats)) +
  geom_bar(stat = "identity")
# Evenness
ggplot(data = abundance_df, aes(x = dwc.habitat.1., y = Eveness)) +
  geom_bar(stat = "identity")
# species richness
ggplot(data = abundance_df, aes(x = dwc.habitat.1., y = n_species)) +
  geom_bar(stat = "identity")
# abundance per feed group 
ggplot(data = bms_workhorse %>% group_by(dwc.habitat.1., Feed_group_2) %>% 
         summarize(abundance = mean(dwc.sampleSizeUnit)) %>% ungroup(), 
       aes(x = dwc.habitat.1., y = abundance, col = Feed_group_2)) +
  geom_point(stat = "identity") +
 # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.6, hjust=.5)) #looks pretty intense, what are the numbers behind that? 
# check
bms_workhorse %>% filter(dwc.habitat.1. == "hydrophytes") %>% filter(Feed_group_2 == "Passive.filter.feeders") %>% 
  select(dws.sampleSizeValue, Taxa, dwc.sampleSizeUnit) # well, skyrocketing values ... 

# plotting of abundance of taxa not so possible with all results (?) 

# %EPT
ggplot(data = abundance_df, aes(x = dwc.habitat.1., y = EPT_percent)) +
  geom_bar(stat = "identity") 

#FV# All this plots could be improved. Perhaps boxplot are more explanatory for biological indices and than you could text if there are significant differences between groups

#-----------------------------------
# Family work (key on Order & Species) is complicated, leads to many-to-many relationships between "workhorse" and "taxalist" 


### developing space ### 
### another problem 
bms_workhorse %>% filter(str_detect(ID, "2021_311623")) %>% nrow() # n taxa from one site (all subsamplpes)
bms_workhorse %>% filter(str_detect(ID, "2021_311623")) %>% distinct(Taxa) %>% nrow() # n unique taxa from one site (all subsamples)
# >> a lot of overlap 

# challenge: drop pseudo samples of species from micro habitats without loosing information 

# group by dwc.eventID and year + Taxa 
# get which habitat type is the most present 


# get col nums for micro habitats & put them in loop 
which(colnames(bms_workhorse) == "mobile.blocks....")
which(colnames(bms_workhorse) == "algae....")
for (i in 30:40) { # make NAs 0 for max.col function 
  bms_workhorse[which(is.na(bms_workhorse[,i])),i] <- as.numeric(0)
}
# find max col 
micro_1 <- bms_workhorse %>% select("mobile.blocks....":"algae....") %>% max.col(ties.method = "last")
micro_2 <- bms_workhorse %>% select("mobile.blocks....":"algae....") %>% max.col(ties.method = "last")
# check if they are same 
all(micro_1 == micro_2) # >> true >> one has always biggest percentage 

# also: percentages of micro habs are same within a site
# >> multiply sample-size-unit & sample-size-value with corresponding percentages of microhab in site (depending on subsample)
# >>> condense df, so per sample site each species only occures once 



