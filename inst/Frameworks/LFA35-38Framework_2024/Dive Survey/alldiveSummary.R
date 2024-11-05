###Dive Analysis
require(dplyr)
require(devtools)
require(sf)
require(lubridate)
library(ggplot2)
require(ggspatial)
require(bio.lobster)
require(bio.utilities)
require(gridExtra)

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

##juvenile counts
# juve_num_sum<-histDive %>%
#   filter(carapacemm<40)%>%
#   group_by(year,LFA)%>%
#   summarise(total_juves=n())

##other Juvenile classification
histDive$carapacemm<-as.numeric(histDive$carapacemm)

juve_num <- histDive %>%
  mutate(juve = case_when(
    carapacemm >= 40 ~ 0,
    carapacemm < 40 ~ 1,
    carapacelength == "> 40"~ 0,
    carapacelength == "> = 40" ~ 0,
    carapacelength == "< 40" ~ 1,
    TRUE ~ NA_real_
  ))

 juve_num_sum<-juve_num %>%
  group_by(year,LFA)%>%
   summarise(total_juves=sum(juve, na.rm=TRUE))

# Merge summaries
hist_sum <- yearly_summary %>%
  left_join(sex_num_summary, by = c("year", "LFA")) %>%
  left_join(juve_num_sum, by = c("year", "LFA"))

hist_sum$total_sex_num_3[is.na(hist_sum$total_sex_num_3)] <- 0
hist_sum$total_juves[is.na(hist_sum$total_juves)] <- 0

# Convert the summary to a dataframe
hist_sum <- as.data.frame(hist_sum)
hist_sum$source<-"Previous"


#### NEW DIVE SURVEY ####
currDive<-read.csv("C:/Users/HowseVJ/Documents/bio.data/bio.lobster/data/divesurvey/BOF_DiveFormatted.csv")
currDive$LFA<-38
#determine density of lobster per m2 and plot #

currDive_summary <- currDive %>%
  group_by(transect_id, year,LFA) %>%
  summarise(
    area_sampled = first(transect_length) * first(transect_width),
    lobstranstot = sum(lobster, na.rm = TRUE)
  )

current_sum<- currDive_summary %>%
  group_by(year,LFA) %>%
  summarise(
    total_area_sampled = sum(area_sampled),
    total_lobsters = sum(lobstranstot),
    lobster_density = (total_lobsters / total_area_sampled) * 100,
    transects_sampled = n_distinct(transect_id)
  )

#include berried femmes
curr_ber<- currDive %>%
  filter(sex == 3) %>%
  group_by(year,LFA) %>%
  summarise(total_sex_num_3 = n())


##juvenile counts
curr_juv<-currDive %>%
  filter(carapace<40)%>%
  group_by(year,LFA)%>%
  summarise(total_juves=n())


# Merge summaries
curr_sum<- current_sum %>%
  left_join(curr_ber, by = c("year","LFA")) %>%
  left_join(curr_juv, by = c("year","LFA"))

curr_sum$total_sex_num_3[is.na(curr_sum$total_sex_num_3)] <- 0
curr_sum$total_juves[is.na(curr_sum$total_juves)] <- 0

# Convert the summary to a dataframe
curr_sum<-as.data.frame(curr_sum)
curr_sum$source<-"Current"

merged_df <- bind_rows(hist_sum, curr_sum)
write.csv(merged_df, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures/divesummarytab.csv")
filtered_df <- merged_df %>% filter(LFA != 35)

# 
# ### BOTH LFAS DENSITIES TOGETHER
# divedensity<-ggplot(filtered_df, aes(x = year, y = lobster_density, fill = source)) +
#   geom_bar(stat = "identity", width = 0.7) +
#   facet_wrap(~ LFA) +
#   labs(
#     x = "Year",
#     y = expression("Lobster Density (Number / 100m"^2*")")
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   scale_fill_manual(values = c("Current" = "black", "other" = "grey")) +
#   theme(legend.position = "none")
# 
# ggsave("lobsdivedensity.png", plot = divedensity, width = 10, height = 6)


###Transects Completed each year
lfa_38_data <- subset(merged_df, LFA == 38)
lfa_38_data$berrieddense<-(lfa_38_data$total_sex_num_3 / lfa_38_data$total_area_sampled) * 100


theme_set(theme_test(base_size = 12))

##### Plot for LFA 38
transects38 <- ggplot(lfa_38_data, aes(x = year, y = transects_sampled, fill = source)) +
  geom_bar(stat = "identity") +
  labs( x = " ", y = "Number of Transects") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")

# Plot for total area sampled by year
area38 <- ggplot(lfa_38_data, aes(x = year, y = total_area_sampled, fill = source)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = expression("Total Area Sampled (m"^2*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")


density38<-ggplot(lfa_38_data, aes(x = year, y = lobster_density, fill = source)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Year",y = expression("Lobster Density (Number / 100m"^2*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")

lfa_38_data <- na.omit(lfa_38_data)
berdense38<-ggplot(lfa_38_data, aes(x = year, y = berrieddense, fill = source)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Year",y = expression("Berried Density (Number / 100m"^2*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")

lfa38_diving<-grid.arrange(transects38, area38, density38,berdense38,ncol= 2)
ggsave("lfa38divesummaryplots.png", plot = lfa38_diving, width = 12, height = 10)


lfa_36_data <- subset(merged_df, LFA == 36)
lfa_36_data$juvedense<-(lfa_36_data$total_juves / lfa_36_data$total_area_sampled) * 100


###PLOTS FOR LFA ######
transects36 <- ggplot(lfa_36_data, aes(x = year, y = transects_sampled, fill = source)) +
  geom_bar(stat = "identity") +
  labs( x = " ", y = "Number of Transects") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")

# Plot for total area sampled by year
area36 <- ggplot(lfa_36_data, aes(x = year, y = total_area_sampled, fill = source)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = expression("Total Area Sampled (m"^2*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")


density36<-ggplot(lfa_36_data, aes(x = year, y = lobster_density, fill = source)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Year",y = expression("Lobster Density (Number / 100m"^2*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")

lfa_36_data <- na.omit(lfa_36_data)
juvdense36<-ggplot(lfa_36_data, aes(x = year, y = juvedense, fill = source)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Year",y = expression("Juvenile Density (Number / 100m"^2*")")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Current" = "black", "Previous" = "grey")) +
  theme(legend.position = "none")

lfa36_diving<-grid.arrange(transects36, area36, density36,juvdense36,ncol= 2)
ggsave("lfa36divesummaryplots.png", plot = lfa36_diving, width = 12, height = 10)




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
  geom_vline(xintercept = 82.5, color = "red", linetype = "solid", linewidth = 0.5) +
  facet_wrap(~ year) +
  labs(
    x = "Carapace Length (mm)",
    y = "Density",
  )

ggsave("diveLF_36.png", plot = diveLF_36, width = 10, height = 6)

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
ggsave("diveLF_38.png", plot = diveLF_38, width = 10, height = 6)

