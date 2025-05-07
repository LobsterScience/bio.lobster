#######################################
# Dive Survey Analysis
# Historic Data formatting and Summary
# Victoria H. 2023
######################################

###########################
# Load Packages
###########################
require(bio.lobster)
require(bio.polygons)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(viridis)
require(dplyr)
require(sf)


#setwd("C:/Users/HowseVJ/OneDrive - divecompO-MPO/LFA 35-38 Framework Resources/Figures")

###########################
# Source Data
###########################

transect = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/opendataTransect8221.csv")
section = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/opendataSections8221.csv")
morph = read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/opendataBiological8221.csv")


##### Check LOBSTER COUNTS ##### 
#Section data : lobster count includes lobster that were encountered by not measured. 
# moph has character data for lengths.. but no column for total lobster.

sec_divecomp <- section %>%
  group_by(transect) %>%
  summarize(total_animals = sum(sectionlobstercount, na.rm = TRUE))
sec_divecomp<-as.data.frame(sec_divecomp)


tran_divecomp <- transect %>%
  group_by(transect) %>%
  summarize(total_animals = sum(totallobster, na.rm = TRUE))
tran_divecomp<-as.data.frame(tran_divecomp)

morph_divecomp <- morph %>%
  group_by(transect) %>%
  summarize(
    total_animals= sum(!is.na(carapacelength) & carapacelength!= "")
  )
morph_divecomp<-as.data.frame(morph_divecomp)

### check if The divecompS are the same
merged_divecomp <- tran_divecomp %>%
  rename(total_animals_tran= total_animals) %>%
  inner_join(sec_divecomp %>% rename(total_animals_sec = total_animals), by = "transect") %>%
  inner_join(morph_divecomp %>% rename(total_animals_morph = total_animals), by = "transect")

# check if the total_animals values are the same across all three dataframes
merged_divecomp <- merged_divecomp %>%
  mutate(same_total_animals = (total_animals_tran == total_animals_sec) & (total_animals_sec == total_animals_morph))

# Display the merged dataframe with the comparison result
print(merged_divecomp)

# Note: For total animals encountered use the lobster count from the transect level so animals counted but not measured are included #

############## Format into one data frame from relational tables, and clean up columns ##################

##Pair up Transects, Sections
diveTS = merge(transect, section, by=c("transect","date", "region", "site"), all.x=T, all.y=F, sort=F) ## merge the sections with the overall Transect data

## Add Morphological data
divecomp = merge(diveTS, morph, by=c("transect", "section","date", "region", "site"), all.x=T, all.y=F, sort=F) ## add all the morphology data to the transects and sections

##Format dates
divecomp$date<-ymd(divecomp$date)
divecomp$month = month(divecomp$date)
divecomp$year= year(divecomp$date)

##Make Sex Numeric 
divecomp$sex_num<-divecomp$sex
divecomp$sex_num[divecomp$sex_num == "male"]<-1
divecomp$sex_num[divecomp$sex_num == "female"]<-2
divecomp$sex_num[divecomp$sex_num == "berried"]<-3
divecomp$sex_num<-as.numeric(divecomp$sex_num)

##tidy up size information
divecomp$carapacemm<-divecomp$carapacelength
divecomp$carapacemm[divecomp$carapacemm == "small"]<-NA
divecomp$carapacemm[divecomp$carapacemm == "> = 40"]<-NA
divecomp$carapacemm[divecomp$carapacemm == "< 40"]<-NA
divecomp$carapacemm <- suppressWarnings(as.numeric(divecomp$carapacemm))

divecomp$carapacemm<-as.numeric(divecomp$carapacemm)

### Rename sites:
#  "  Seal Cove" and  "   Whale Cove" no spaces
divecomp$site[divecomp$site == "    Seal Cove" ] <- "Seal Cove"
divecomp$site[divecomp$site == "   Whale Cove"  ] <- "Whale Cove"




##Convert substrates to numeric to match other data sources

# substrate types 
substrate_map <- c(
  cobble = 5,
  boulder = 6,
  ledge = 3,
  gravel = 4,
  mud = 1,
  sand = 2
)

# Convert columns to numeric based substrate scale
divecomp <- divecomp %>%
  mutate(
    primarysubstrate = as.numeric(substrate_map[primarysubstrate]),
    secondarysubstrate = as.numeric(substrate_map[secondarysubstrate])
  )

##ADD LFAs to the mix

divecomp<- divecomp %>%
  mutate(LFA = case_when(
    region %in% c("Passamaquoddy Bay", "Maces Bay", "Deadman's to Seeleys Head", "West Isles", "Letang Estuary") ~ 36,
    region == "Annapolis Basin" ~ 35,
    region == "Grand Manan" ~ 38,
    TRUE ~ NA_real_
  ))




###########Loop Data through to build NewInt Column


# Sort the dataframe by transect and section
divecomp <- divecomp[order(divecomp$transect, divecomp$section), ]

# NewInt column
divecomp$NewInt <- 0

# Loop through each unique transect
for (transect_id in unique(divecomp$transect)) {
  # Filter the dataframe for the current transect
  transect_data <- divecomp[divecomp$transect == transect_id, ]
  
  # Calculate the starting meter mark for each section since the transect runs from 0 - 50m, or 0-150m etc. 
  ## this renames the interval along the transect based on section number and section length to reflect the distance along the transect in m
  start_meter <- 0
  for (i in 1:nrow(transect_data)) {
    if (i == 1 || transect_data$section[i] != transect_data$section[i-1]) {
      transect_data$NewInt[i] <- start_meter
      start_meter <- start_meter + transect_data$sectionlength[i]
    } else {
      transect_data$NewInt[i] <- transect_data$NewInt[i-1]
    }
  }

  divecomp[divecomp$transect == transect_id, "NewInt"] <- transect_data$NewInt
}


### Next Fill the new primary and secondary substrate columns with existing data or modify the  % data to populate where primary and secondary are not present
# new columns
divecomp$secPrimarySub <- NA
divecomp$secSecondarySub <- NA

# Loop through each unique transect
for (transect_id in unique(divecomp$transect)) {
  transect_data <- divecomp[divecomp$transect == transect_id, ]
  
  # Copy non-NA values from primarysubstrate to secPrimarySub
  transect_data$secPrimarySub <- ifelse(!is.na(transect_data$primarysubstrate), transect_data$primarysubstrate, NA)
  
  # Copy non-NA values from secondarysubstrate to secSecondarySub
  transect_data$secSecondarySub <- ifelse(!is.na(transect_data$secondarysubstrate), transect_data$secondarysubstrate, NA)
  
  # Update the original dataframe
  divecomp[divecomp$transect == transect_id, "secPrimarySub"] <- transect_data$secPrimarySub
  divecomp[divecomp$transect == transect_id, "secSecondarySub"] <- transect_data$secSecondarySub
}

### Next Fill the new primary and secondary substrate columns that have NAs with the corresponding information from the %substrate columns
## and assign the substrate codes to them

########### DON"T USE THIS UNTIL WE KNOW THE CODES IN PRIMARY AND SECONDARY ARE THE SAME AS THE LIST BELOW #############

# Define the corresponding numbers
substrate_map <- c(
  percentledge = 3,
  percentboulder = 6,
  percentcobble = 5,
  percentgravel = 4,
  percentsand = 2,
  percentmud = 1
)

# Loop through each unique transect
for (transect_id in unique(divecomp$transect)) {
  # Filter the dataframe for the current transect
  transect_data <- divecomp[divecomp$transect == transect_id, ]
  
  # Loop through each row in the current transect
  for (i in 1:nrow(transect_data)) {
    # Get the values of the specified columns as a numeric vector
    values <- as.numeric(transect_data[i, names(substrate_map)])
    
    # Find the highest and second highest values
    sorted_indices <- order(values, decreasing = TRUE)
    highest <- names(substrate_map)[sorted_indices[1]]
    second_highest <- names(substrate_map)[sorted_indices[2]]
    
    # Populate the section substrate columns
    if (is.na(transect_data$secPrimarySub[i])) {
      divecomp$secPrimarySub[divecomp$transect == transect_id & divecomp$section == transect_data$section[i]] <- substrate_map[highest]
    }
    if (is.na(transect_data$secSecondarySub[i])) {
      divecomp$secSecondarySub[divecomp$transect == transect_id & divecomp$section == transect_data$section[i]] <- substrate_map[second_highest]
    }
  }
}



##A loop that Creates a column that populates binary lobster count column - every row an encounter -- to match the totallobster within a section
# Assuming your dataframe is named divecomp

divecomp$lobster <- 0

# "lobster" to 1 where "carapacemm" has a value
divecomp$lobster[!is.na(divecomp$carapacemm)] <- 1

#  Replace NA in section and section lobster count column with 1 and 0
divecomp$section[is.na(divecomp$section)] <- 1
divecomp$sectionlobstercount[is.na(divecomp$sectionlobstercount)] <- 0

#Cycle through unique transects and sections
unique_transects <- unique(divecomp$transect)

for (transect in unique_transects) {
  transect_data <- divecomp[divecomp$transect == transect, ]
  unique_sections <- unique(transect_data$section)
  
  for (section in unique_sections) {
    section_data <- transect_data[transect_data$section == section, ]
    section_lobster_count <- unique(section_data$sectionlobstercount)
    
    if (length(section_lobster_count) == 1) {
      num_lobsters <- section_lobster_count
      num_records <- sum(!is.na(section_data$carapacemm))
      
      if (num_lobsters > num_records) {
        num_new_rows <- num_lobsters - num_records
        
        for (i in 1:num_new_rows) { ## individual lobster data that shouldn't be replicated
          new_row <- section_data[1, ]
          new_row$carapacelength <- NA
          new_row$sex <- NA
          new_row$shellhardness <- NA
          new_row$eggs <- NA
          new_row$sex_num <- NA
          new_row$carapacemm <- NA
          new_row$lobster <- 1
          
          divecomp <- rbind(divecomp, new_row)
        }
      }
    }
  }
  
  # Output the transect number that is completed
  print(paste("Completed transect:", transect))
}

# Sort the dataframe by transect and section 
divecomp <- divecomp[order(divecomp$transect, divecomp$section), ]



### Average Depth in meters by unique Transect - Is depth recorded and then corrected for the tide ?


p = ggLobsterMap(area="BoF",addGrids=F,
                 fill.colours = 'grey',bathy=F,return.object = T,colourLFA = F)
p = p + geom_point(data = divecomp, aes(x = startlongitude , y = startlatitude), color = "red")
p


##Export dataframe for further analysis
write.csv(divecomp,"C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/OD_DiveFormatted.csv")

