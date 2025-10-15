#'CODE ASSOCIATED WITH: CHAN ET AL. 2025, ORYX
#'
#'
#'AUTHORS: Ryan J. Almeida, Jordan C. Chan
#'
#'DATE: 2025/15/10
#'
#'REQUIRED PACKAGES: tidyverse
#'
#'DESCRIPTION: This script contains the code used to clean data for 
#'analyses outlined in Chan et al. (2025).

#load tidyverse package
library(tidyverse)

#read LEMIS data
lemis <- read.csv("lemisDataCorrected_2023-11-11.csv")

# #filter to only live herp imports with N < 10000 individuals
herps <- lemis %>%
  filter(group_ == "Reptiles" |
           group_ == "Amphibians",
         unit == "NO",
         import_export == "I",
         description == "LIV",
         purpose == "T")

#make quantity variable numeric
herps$quantity <- as.numeric(herps$quantity)

#species to remove
remove.spp <- c("Pelodiscus sinensis")
remove.genus <- c("Rana", "Xenopus", "Lithobates")

#remove excluded taxa
herps <- herps %>%
  filter(!(corrected %in% remove.spp), 
         !(correctedGenus %in% remove.genus),
         correctedGenus != "Above genus",
         corrected != "Above species")


#remove outlier singletons within genus
#for each genus
for (i in 1:length(unique(herps$correctedGenus))){
  #filter data to current genus and sort descending by quantity
  current <- filter(herps, 
                    correctedGenus == unique(herps$correctedGenus[i])) %>% 
    arrange(desc(quantity))
  
  #if quantity if biggest import is 10x second biggest
  if((current$quantity[1]/current$quantity[2]) >= 10){
    
    #exclude this entry
    id <- current$entryID[1]
    herps <- filter(herps, entryID != id)
  }
}

#create column for only genus at species
herps$genus.only <- !grepl(" ",herps$corrected)

#remove any species not at the genus level
herps <- herps %>% 
  filter(genus.only == F)

#rename ports
herps$port[herps$port == "NW" | 
             herps$port == "NY"] <- "New York"

herps$port[herps$port == "MI"] <- "Miami"
herps$port[herps$port == "LA"] <- "Los Angeles"
herps$port[herps$port == "AT"] <- "Atlanta"
herps$port[herps$port == "DF"] <- "Dallas-Fort Worth"

#lump all other ports into "remaining ports"
#define top 5 ports
top5ports <- c("Atlanta", 
               "New York", 
               "Miami", 
               "Los Angeles", 
               "Dallas-Fort Worth")

#any port NOT in the top 5 is re-labeled "Remaining ports"
herps$port[!(herps$port %in% top5ports)] <- "Remaining ports"


#write csv with herps
write.csv(herps,"herpsLEMIS.csv")
