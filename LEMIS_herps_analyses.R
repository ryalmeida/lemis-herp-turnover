#'CODE ASSOCIATED WITH: CHAN ET AL. 2025, ORYX
#'
#'
#'AUTHORS: Ryan J. Almeida, Jordan C. Chan
#'
#'DATE: 2025/15/10
#'
#'REQUIRED PACKAGES: tidyverse, vegan
#'
#'DESCRIPTION: This script contains the code used to conduct turnover,
#'species accumulation curveand residence time analyses outlined 
#'in Chan et al. (2025).

### Load packages and import data----
library(tidyverse)
library(vegan)

#read LEMIS herps data from LEMIS_herps_dataClean.R
herps <- read.csv("herpsLEMIS.csv")

### Calculate residence times and species accumulation curves------

#residence times across all ports
residence.df <- herps %>% 
  group_by(corrected) %>% 
  summarise(Year = length(unique(sYear)) + 2000) %>% 
  group_by(Year) %>% 
  summarise(N = n(),
            percent = N/length(unique(herps$corrected)))

#initalize data frame for species accumulation curves
portSAC.df <- data.frame()

#initalize data frame for port level residence times
residence.df.port <- data.frame()

#calculate SAC and residence time for each port
for (i in 1:length(unique(herps$port))){ #for each port
  
  #current port
  port_current <- unique(herps$port)[i]
  
  #print message to track ports
  print(paste("Current port:",port_current))
  
  #dataframe with just that port
  port.df <- filter(herps, port == port_current)
  
  #calculate SAC for port region
  #create site x spp matrix for all ports
  portSxS <- port.df %>% 
    group_by(corrected, sYear) %>% 
    summarise(n = sum(quantity,na.rm = T)) %>% 
    pivot_wider(names_from = corrected,
                values_from = n)
  
  #turn NA to 0
  portSxS[is.na(portSxS)] <- 0
  
  #rename rows with years
  rownames(portSxS) <- portSxS$sYear
  
  #calculate SAC for all ports
  portSAC <- specaccum(portSxS[,-1], method = method)
  print("Accumulation done")
  
  #place SAC data into df for plotting
  # obsS <- data.frame("Year" = portSAC$sites + 2000,
  #                    "percent" = portSAC$richness/max(portSAC$richness))
  obsS <- data.frame("Year" = portSAC$sites + 2000,
                     "percent" = portSAC$richness/length(unique(herps$corrected)))
  obsS$taxon <- "Species"
  obsS$port <- port_current
  
  #intialize a dataframe for all port SACs that contains the combined ports
  portSAC.df <- rbind(portSAC.df,obsS)
  
  residenceS <- port.df %>% 
    group_by(corrected) %>% 
    summarise(Year = length(unique(sYear)) + 2000) %>% 
    group_by(Year) %>% 
    summarise(N = n())
  
  #proportion of species per residence time
  residenceS$percent <- residenceS$N/length(unique(port.df$corrected))
  residenceS$port <- port_current
  residenceS$taxon <- "Species"
  
  residence.df.port <- rbind(residence.df.port, residenceS)
}


### Calculate annual turnover and new trade entrant rates----

#get species for each year at each port
ports.turnover <- herps %>% 
  group_by(sYear, corrected, port) %>% 
  summarise(n = sum(quantity, na.rm = T))

#determine number of species in all ports
allports <- herps %>% 
  group_by(sYear, corrected) %>% 
  summarise(n = sum(quantity, na.rm = T)) 

#for each port
for (j in 1:length(unique(herps$port))){
  #filter to focus on current port
  port.df <- filter(ports.turnover, port == ports[j])
  #initialize vector of years
  years <- unique(port.df$sYear)
  #if imports happened at a port for more than a year
  if (length(years) > 1){
    #for each year, starting in 2001
    for (i in (2:length(years))){
      #reference year is year before current index
      y0 <- filter(port.df, sYear == years[i-1])
      #turnover will be calculated from y0 to y1
      y1 <- filter(port.df, sYear == years[i])
      
      #determine which species have been imported up until current year
      y0.all <- filter(allports, sYear< years[i])
      
      #calculate species turnover constituents:
      #total number of species
      st <- length(unique(c(unique(y0$corrected), 
                            unique(y1$corrected))))
      
      #species lost from Y0 to Y1
      sl <- length(setdiff(unique(y0$corrected), 
                           unique(y1$corrected)))
      
      #species gained from Y0 to Y1
      sg <- length(setdiff(unique(y1$corrected), 
                           unique(y0$corrected)))
      
      
      #initialize dataframe with turnover results
      species.turnover <- data.frame(Year = years[i],
                                     port = ports[j],
                                     value = ((sg + sl)/st),
                                     type = "Turnover")
      
      #calculate proportion of new port entrants coming in that year
      new.spp <- length(setdiff(unique(y1$corrected), 
                               unique(y0$corrected)))/length(unique(y1$corrected))
      
      #calculate proportion of new trade entrants coming in that year
      new.spp.all <- length(setdiff(unique(y1$corrected), 
                                    unique(y0.all$corrected)))/length(unique(y1$corrected))
      
      #add results to dataframe
      species.propNewSp.port <- data.frame(Year = years[i],
                                           port = ports[j],
                                           value = new.spp,
                                           type = "New to Port")
      #add results to dataframe
      species.propNewSp <- data.frame(Year = years[i],
                                      port = ports[j],
                                      value = new.spp.all,
                                      type = "New to Overall Trade")
      
      
      #add to big dataframe
      turnover.port.df <- rbind(species.turnover,
                                species.propNewSp,
                                species.propNewSp.port,
                                turnover.port.df) 
    }
  }
}

