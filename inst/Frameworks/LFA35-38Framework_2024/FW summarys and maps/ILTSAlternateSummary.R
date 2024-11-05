require(lubridate)
require(devtools)
require(ggplot2)
require(dplyr)
require(devtools)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)


#lobster.db('survey.redo')
lobster.db('survey')


LS<-surveyCatch[(surveyCatch$HAULCCD_ID== 1),] ##Only Successful Tows
bofLS<-LS[(LS$LFA %in% c("L34","L35","L36","L37","L38")),] ##Pull out BOF --Data from "new" survey and 1 year of older 2013 


###Timing of survey
bofLS$HAUL_DATE <- as.Date(bofLS$HAUL_DATE, format = "%Y-%m-%d")
bofLS$MONTH <- format(bofLS$HAUL_DATE, "%m")
unique_months <- unique(bofLS[, c("YEAR", "LFA", "MONTH")])
summary_months <- aggregate(MONTH ~ YEAR + LFA, data = unique_months, FUN = function(x) paste(unique(x), collapse = ", "))
summary_months <- as.data.frame(summary_months)
str(summary_months)

write.csv(summary_months, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/Surveymonths.csv")


sumLS<-subset(bofLS, select=c(STATION, YEAR, LFA, SPECCD_ID, NUM_CAUGHT,SET_ID,TRIP_ID))


# Create a function to calculate the summary statistics
calculate_summary <- function(data) {
  # Split the data by YEAR and LFA
  split_data <- split(sumLS, list(sumLS$YEAR, sumLS$LFA))
  
  # Initialize an empty list to store results
  result_list <- list()
  
  # Loop through each subset of data
  for (subset in split_data) {
    if (nrow(subset) > 0) {
      # Calculate the summary statistics for each subset
      completed_stations <- length(unique(subset$STATION))
      completed_tows <- length(unique(subset$SET_ID))
      total_caught_2550 <- sum(subset$NUM_CAUGHT[subset$SPECCD_ID == 2550], na.rm = TRUE)
      tows_with_2550 <- length(unique(subset$SET_ID[subset$SPECCD_ID == 2550]))
      
      # Create a data frame with the results
      result <- data.frame(
        YEAR = unique(subset$YEAR),
        LFA = unique(subset$LFA),
        completed_stations = completed_stations,
        completed_tows = completed_tows,
        total_caught_2550 = total_caught_2550,
        tows_with_2550 = tows_with_2550
      )
      
      # Append the result to the list
      result_list <- append(result_list, list(result))
    }
  }
  
  # Combine all results into a single data frame
  result_df <- do.call(rbind, result_list)
  return(result_df)
}

# Apply the function to your data
sumLS_table <- calculate_summary(sumLS)
write.csv(sumLS_table, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/SurveyStations_new.csv")

###Lobster Specific Data Table

# Filter the data
lobMeas <- surveyMeasurements[surveyMeasurements$HAULCCD_ID == 1, ]
lobMeas <- lobMeas[lobMeas$LFA %in% c("L34","L35", "L36", "L37", "L38"), ]
lobMeas <- lobMeas[, c("SET_DATE", "LFA", "SPECCD_ID", "SEX", "FISH_LENGTH", "VNOTCH")]
lobMeas$YEAR <- as.numeric(format(as.Date(lobMeas$SET_DATE), "%Y"))
lobMeas <- lobMeas[lobMeas$SPECCD_ID == 2550, ]


results <- list()
unique_combinations <- unique(lobMeas[, c("YEAR", "LFA")])

# Loop through each combination of YEAR and LFA
for (i in 1:nrow(unique_combinations)) {
  year <- unique_combinations$YEAR[i]
  lfa <- unique_combinations$LFA[i]
  
  # Subset the data for YEAR and LFA
  subset_data <- lobMeas[lobMeas$YEAR == year & lobMeas$LFA == lfa, ]
  
  total_animals <- nrow(subset_data)
  commercial <- round(sum((subset_data$SEX == 1 | subset_data$SEX == 2) & (is.na(subset_data$VNOTCH) | subset_data$VNOTCH == 0) & (subset_data$FISH_LENGTH > 82)) + 
                        0.5 * sum((subset_data$SEX == 1 | subset_data$SEX == 2) & (is.na(subset_data$VNOTCH) | subset_data$VNOTCH == 0) & (subset_data$FISH_LENGTH == 82)))
  recruits <- round(sum(subset_data$FISH_LENGTH >= 70 & subset_data$FISH_LENGTH < 82) + 
                      0.5 * sum(subset_data$FISH_LENGTH == 82))
  males <- sum(subset_data$SEX == 1)
  females <- sum(subset_data$SEX == 2 | subset_data$SEX == 3)
  berried <- sum(subset_data$SEX == 3)
  
  results[[i]] <- data.frame(YEAR = year, LFA = lfa, total_animals = total_animals, commercial = commercial, recruits = recruits, males = males, females = females, berried = berried)
}

# Combine the list into  data frame
summary_lobMeas <- do.call(rbind, results)

summary_lobMeas <- as.data.frame(summary_lobMeas)

str(summary_lobMeas)



##BuMP UP

# Merge tables
statANDmeas <- merge(summary_lobMeas, sumLS_table, by = c("YEAR", "LFA"))

# Calculate factor and adjust counts
statANDmeas$factor <- statANDmeas$total_caught_2550 / statANDmeas$total_animals
statANDmeas$commercial <- round(statANDmeas$commercial * statANDmeas$factor)
statANDmeas$recruits <- round(statANDmeas$recruits * statANDmeas$factor)
statANDmeas$males <- round(statANDmeas$males * statANDmeas$factor)
statANDmeas$females <- round(statANDmeas$females * statANDmeas$factor)
statANDmeas$berried <- round(statANDmeas$berried * statANDmeas$factor)

# Remove the factor column
statANDmeas <- statANDmeas[, !names(statANDmeas) %in% "factor"]

# Check the structure of the resulting data frame
str(statANDmeas)

write.csv(statANDmeas, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/stationandmeas_new.csv")



#Survey, Length of Time Series, N stations within area of interest per year, timing of survey, gear spatial extent (km2 of domain)
