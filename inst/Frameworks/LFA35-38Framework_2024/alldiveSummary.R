###Dive Analysis
require(dplyr)
require(devtools)
require(sf)
require(lubridate)
library(ggplot2)
require(ggspatial)
require(bio.lobster)
require(bio.utilities)

theme_set(theme_test(base_size = 14))

setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures")
histDive<-read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOFHistoric/OD_DiveFormatted.csv")


# Determine density of lobster per m2
diveTrans_summary <- histDive %>%
  group_by(transect, year, LFA) %>%
  summarise(
    area_sampled = first(transectlength) * first(transectwidth),
    lobstranstot = first(totallobster)
  )

# Summarise the data by year and LFA, including the number of transects sampled
yearly_summary <- diveTrans_summary %>%
  group_by(year, LFA) %>%
  summarise(
    total_area_sampled = sum(area_sampled),
    total_lobsters = sum(lobstranstot),
    lobster_density = (total_lobsters / total_area_sampled) * 100,
    transects_sampled = n_distinct(transect)
  )
##include berried femmes
sex_num_summary <- histDive %>%
  filter(sex_num == 3) %>%
  group_by(year, LFA) %>%
  summarise(total_sex_num_3 = n())

# Merge summaries
hist_sum <- yearly_summary %>%
  left_join(sex_num_summary, by = c("year", "LFA"))

# Convert the summary to a dataframe
hist_sum <- as.data.frame(hist_sum)
hist_sum$source<-"Previous"


#### NEW DIVE SURVEY ####
currDive<-read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOF_DiveFormatted.csv")

#determine density of lobster per m2 and plot #

currDive_summary <- currDive %>%
  group_by(transect_id, year) %>%
  summarise(
    area_sampled = first(transect_length) * first(transect_width),
    lobstranstot = sum(lobster, na.rm = TRUE)
  )

current_sum<- currDive_summary %>%
  group_by(year) %>%
  summarise(
    total_area_sampled = sum(area_sampled),
    total_lobsters = sum(lobstranstot),
    lobster_density = (total_lobsters / total_area_sampled) * 100,
    transects_sampled = n_distinct(transect_id)
  )

#include berried femmes
curr_ber <- currDive %>%
  filter(sex == 3) %>%
  group_by(year) %>%
  summarise(total_sex_num_3 = n())

# Merge summaries
curr_sum <- current_sum %>%
  left_join(curr_ber, by = c("year"))

current_sum<-as.data.frame(curr_sum)

current_sum$LFA<-38
current_sum$source<-"Current"


merged_df <- bind_rows(hist_sum, current_sum)
write.csv(merged_df, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/divesummarytab.csv")

filtered_df <- merged_df %>% filter(LFA != 35)

divedensity<-ggplot(filtered_df, aes(x = year, y = lobster_density, fill = source)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~ LFA) +
  labs(
    x = "Year",
    y = expression("Lobster Density (Number / 100m"^2*")")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "other" = "grey")) +
  theme(legend.position = "none")

ggsave("lobsdivedensity.png", plot = divedensity, width = 10, height = 6)



### Length Frequencies ###
hisLF<-histDive %>% 
  filter(!is.na(carapacemm)) %>%
  select(LFA, year, carapacemm, sex_num)
hisLF$source<-"Previous"
hisLF<-hisLF %>%
  rename(carapace= carapacemm,
         sex=sex_num)

curLF<-currDive %>% 
  filter(!is.na(carapace)) %>%
  select(year, carapace, sex)
curLF$source<-"Current"
curLF$LFA<-38

mergeLF <- bind_rows(hisLF,curLF)


### LFA 36 plot
merge36 <- mergeLF %>%
  filter(LFA==36)

# Create the multipanel histogram
diveLF_36<-ggplot(merge36, aes(x = carapace)) +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = 82.5, color = "red", linetype = "solid", size = 0.5) +
  facet_wrap(~ year) +
  labs(
    x = "Carapace Length (mm)",
    y = "Density",
  )

#ggsave("diveLF_36.png", plot = diveLF_36, width = 10, height = 6)

### LFA 38 plot
merge38 <- mergeLF %>%
  filter(LFA==38)

# Create the multipanel histogram
diveLF_38<-ggplot(merge38, aes(x = carapace)) +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = 82.5, color = "red", linetype = "solid", size = 0.5) +
  
  facet_wrap(~ year) +
  labs(
    x = "Carapace Length (mm)",
    y = "Density",
  )

#ggsave("diveLF_38.png", plot = diveLF_38, width = 10, height = 6)


## Berried females over time 
filtered_df <- filtered_df %>%
  filter(!is.na(total_sex_num_3))

ggplot(filtered_df, aes(x = year, y = total_sex_num_3)) +
  geom_bar(stat = "identity") +  # You can use geom_point() or geom_bar(stat = "identity") if you prefer
  facet_wrap(~ LFA, scales = "free_y") +
  labs(
    x = "Year",
    y = "Number of Berried Females",
  ) 
