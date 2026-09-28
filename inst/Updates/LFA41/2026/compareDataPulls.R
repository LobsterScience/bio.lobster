library(dplyr)
library(purrr)
library(ggplot2)
require(bio.lobster)
require(bio.utilities)
require(devtools)
la()

p=list()

p$lfas = c("41") # specify lfa
p$current.assessment.year =2026
p$yrs = 1947:p$current.assessment.year

outspring25=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/LFA41-NEFSCSpringCommercialB.csv")
outfall25=read.csv("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/LFA41-NEFSCFallCommercialB.csv")

fp = file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2024','indicators')
outspring24=read.csv(file.path(fp,'NEFSC.spring.restratified.commercial.csv'))
outfall24=read.csv(file.path(fp,'NEFSC.fall.restratified.commercial.csv'))


fp = file.path(project.datadirectory('bio.lobster'),'assessments','LFA41','2023','indicators')
outspring23=read.csv(file.path(fp,'NEFSC.spring.restratified.commercial.csv'))
outfall23=read.csv(file.path(fp,'NEFSC.fall.restratified.commercial.csv'))





spring <- reduce(
  list(
    outspring23 %>% select(yr, w.Yst) %>% rename(w23 = w.Yst),
    outspring24 %>% select(yr, w.Yst) %>% rename(w24 = w.Yst),
    outspring25 %>% select(yr, w.Yst) %>% rename(w25 = w.Yst)
  ),
  full_join,
  by = "yr"
)

spring %>%
  mutate(
    diff24_23 = w24 - w23,
    diff25_24 = w25 - w24
  ) %>%
  filter(
    diff24_23 != 0 |
      diff25_24 != 0
  )


plot(spring$yr, spring$w23, type = "l", col = "blue")
lines(spring$yr, spring$w24, col = "red")
lines(spring$yr, spring$w25, col = "green")

legend(
  "topleft",
  c("2023", "2024", "2025"),
  col = c("blue", "red", "green"),
  lty = 1
)


all.equal(
  outspring23 %>%
    filter(yr < max(yr)),
  outspring24 %>%
    filter(yr < max(yr))
)

tmp <- full_join(
  outspring24 %>% select(yr, w.Yst) %>% rename(w24 = w.Yst),
  outspring25 %>% select(yr, w.Yst) %>% rename(w25 = w.Yst),
  by = "yr"
)

tmp %>%
  filter(
    yr < max(yr, na.rm = TRUE),  # ignore newest year
    w24 != w25
  )


spring <- bind_rows(
  outspring23 %>% mutate(version = "2023"),
  outspring24 %>% mutate(version = "2024"),
  outspring25 %>% mutate(version = "2025")
)

fall <- bind_rows(
  outfall23 %>% mutate(version = "2023"),
  outfall24 %>% mutate(version = "2024"),
  outfall25 %>% mutate(version = "2025")
)

par(mfrow = c(1, 2))

# Spring
ggplot(spring,
       aes(x = yr, y = w.Yst/1000, colour = version)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Spring",
    y = "Stratified Total Weight (kt)",
    x = "Year"
  )

# Fall
ggplot(fall,
       aes(x = yr, y = w.Yst/1000, colour = version)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Fall",
    y = "Stratified Total Weight (kt)",
    x = "Year"
  )

## Compare 2025 to 2024 to see difference

comp <- full_join(
  outspring24 %>%
    select(yr, w24 = w.Yst),
  outspring25 %>%
    select(yr, w25 = w.Yst),
  by = "yr"
) %>%
  mutate(
    diff = w25 - w24,
    pct_diff = 100 * (w25 - w24) / w24,
    ratio = w25 / w24
  )

comp
