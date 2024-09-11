## Dive Summary Tables


###Import Current Survey and historic survey data





### Summarize the data available

summary_df <- diveTrans %>%
  group_by(year) %>%
  summarize(
    unique_transects = n_distinct(tid),
    total_animals_encountered = sum(lobster, na.rm = TRUE),
  )


# Summarize the dataframe
summary_df <- diveTrans %>%
  summarize(
    yearsSampled = n_distinct(year),
    transects = n_distinct(tid),
    totalLobsterEncountered = sum(lobster, na.rm = TRUE),
    unique_sites = n_distinct(transect_id)
  )
summary_df<-as.data.frame(summary_df)


##########CHECK THELOBSTER COUNTS MAKE SENSE???

###Summarize dataframe
# Summarize the dataframe
summary_df <- divecomp %>%
  group_by(lfa) %>%
  summarize(
    yearsSampled = n_distinct(year),
    transects = n_distinct(transect),
    totalLobsterEncountered = sum(totallobster, na.rm = TRUE),
    unique_sites = n_distinct(site)
  )
summary_df<-as.data.frame(summary_df)


# Summarize the dataframe by individual year
summary_dfy <- divecomp %>%
  group_by(lfa,year) %>%
  summarize(
    transects = n_distinct(transect),
    totalLobsterEncountered = sum(totallobster, na.rm = TRUE),
    unique_sites = n_distinct(site)
  )
summary_dfy<-as.data.frame(summary_dfy)

##Also plot or table... 

## area sampled
## Density of lobster
## length frequenices
## Number of berried lobster annually
##

