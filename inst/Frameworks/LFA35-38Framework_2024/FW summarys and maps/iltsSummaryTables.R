require(lubridate)
require(devtools)
require(ggplot2)
require(dplyr)
require(devtools)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)



lobster.db('survey.redo')
lobster.db('survey')


LS<-surveyCatch[(surveyCatch$HAULCCD_ID== 1),] ##Only Successful Tows
bofLS<-LS[(LS$LFA %in% c("L35","L36","L37","L38")),] ##Pull out BOF --Data from "new" survey and 1 year of older 2013 

##Survey summary 
# Group by year and LFA
#Total number of stations
#Number of lobster
#Average Temperature
#Depth range 

sumLS<-subset(bofLS, select=c(STATION, YEAR, LFA, SPECCD_ID, NUM_CAUGHT,SET_ID,TRIP_ID))

### YEAR / LFA / NUMBER OF TOWS / TOTAL LOBSTER CAUGHT
sumLS_table <- sumLS %>%
  group_by(YEAR, LFA) %>%
  summarise(
    completed_stations = n_distinct(STATION),  # Counting unique stations
    completed_tows = n_distinct(SET_ID),  # Counting Number of tows
    total_caught_2550 = sum(ifelse(SPECCD_ID == 2550, NUM_CAUGHT, 0), na.rm = TRUE),  # Summing NUM_CAUGHT where SPECCD_ID == 2550
    tows_with_2550 = n_distinct(SET_ID[SPECCD_ID == 2550])  # Counting Number of tows where SPECCD_ID == 2550
  )
sumLS_table<-as.data.frame(sumLS_table)

write.csv(sumLS_table, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/SurveyStations.csv")

###Lobster Specific Data Table
#Group by LFA and Year
#N lobster, N commercial, N recruit 70-82


lobMeas<-surveyMeasurements[(surveyMeasurements$HAULCCD_ID== 1),] ##Only Successful Tows
lobMeas<-lobMeas[(lobMeas$LFA %in% c("L35","L36","L37","L38")),] ##Pull out BOF --Data from "new" survey and 1 year of older 2013 

lobMeas<-subset(lobMeas, select=c(SET_DATE, LFA, SPECCD_ID, SEX, FISH_LENGTH, VNOTCH))
lobMeas$YEAR<-year(ymd(lobMeas$SET_DATE))
lobMeas<- lobMeas %>% filter(SPECCD_ID == 2550) ## only lobs

summary_lobMeas <- lobMeas %>%
  group_by(YEAR, LFA) %>%
  summarize(
    total_animals = n(),
    commercial = sum((SEX == 1 | SEX == 2) & (is.na(VNOTCH) | VNOTCH == 0) & (FISH_LENGTH > 82)) + 
      0.5 * sum((SEX == 1 | SEX == 2) & (is.na(VNOTCH) | VNOTCH == 0) & (FISH_LENGTH == 82)),
    recruits = sum(FISH_LENGTH >= 70 & FISH_LENGTH < 82) + 
      0.5 * sum(FISH_LENGTH == 82), 
    males = sum(SEX == 1),
    females = sum(SEX == 2 | SEX == 3),
    berried = sum(SEX == 3)
  )

summary_lobMeas<-as.data.frame(summary_lobMeas)

##Bump up the number sampled based on the total number caught first join the columns


statANDmeas <- summary_lobMeas %>%
  left_join(sumLS_table, by = c("YEAR", "LFA")) %>%
  mutate(
    factor = total_caught_2550 / total_animals,
    commercial = round(commercial * factor),
    recruits = round(recruits * factor),
    males = round(males * factor),
    females = round(females * factor),
    berried = round(berried * factor)
  ) %>%
  select(-factor)


write.csv(statANDmeas, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/stationandmeas.csv")

#Survey, Length of Time Series, N stations within area of interest per year, timing of survey, gear spatial extent (km2 of domain)
