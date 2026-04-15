
library(dplyr)
library(ggplot2)

# -----------------------
project <- tibble::tribble(
  ~wp, ~Activity, ~start_date, ~end_date,
 "Fishing_season", "LFA27",    as.Date("2024-05-15"), as.Date("2024-07-15"),
  "Preseason", "LFA27",    as.Date("2024-02-15"), as.Date("2024-02-20"),
  "Postseason", "LFA27",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
 "Fishing_season", "LFA28-29",    as.Date("2024-04-28"), as.Date("2024-06-28"),
  "Preseason", "LFA28-29",    as.Date("2024-02-15"), as.Date("2024-02-20"),
  "Postseason", "LFA28-29",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
  
 "Fishing_season", "LFA30",    as.Date("2024-05-17"), as.Date("2024-07-18"),
  "Preseason", "LFA30",    as.Date("2024-02-15"), as.Date("2024-02-20"),
  "Postseason", "LFA30",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
 "Fishing_season", "LFA31A",   as.Date("2024-04-27"), as.Date("2024-06-28"),
  "Preseason", "LFA31A",    as.Date("2024-02-15"), as.Date("2024-02-20"),
  "Postseason", "LFA31A",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
 "Fishing_season", "LFA31B",   as.Date("2024-04-19"), as.Date("2024-06-20"),
  "Preseason", "LFA31B",    as.Date("2024-02-15"), as.Date("2024-02-20"),
  "Postseason", "LFA31B",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
 "Fishing_season", "LFA32",    as.Date("2024-04-19"), as.Date("2024-06-20"),
  "Preseason", "LFA32",    as.Date("2024-02-15"), as.Date("2024-02-20"),
  "Postseason", "LFA32",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
 "Fishing_season", "LFA33",    as.Date("2023-11-26"), as.Date("2024-05-31"),
  "Preseason", "LFA33",    as.Date("2023-10-15"), as.Date("2023-10-20"),
  "Midseason", "LFA33",    as.Date("2024-2-15"), as.Date("2024-2-17"),
  "Postseason", "LFA33",    as.Date("2024-6-15"), as.Date("2024-6-20"),
  'Science','LFA33',   as.Date("2024-2-18"), as.Date("2024-2-20"),
  
 "Fishing_season", "LFA34",    as.Date("2023-12-02"), as.Date("2024-05-31"),
  "Preseason", "LFA34",    as.Date("2023-10-15"), as.Date("2023-10-20"),
  "Midseason", "LFA34",    as.Date("2024-2-15"), as.Date("2024-2-17"),
  "Postseason", "LFA34",    as.Date("2024-6-15"), as.Date("2024-6-20"),
  'Science','LFA34',   as.Date("2024-2-18"), as.Date("2024-2-20"),
  
 "Fishing_season", "LFA35",    as.Date("2023-10-12"), as.Date("2023-12-31"),
 "Fishing_season", "LFA35",    as.Date("2024-02-28"), as.Date("2024-07-31"),
  "Preseason", "LFA35",    as.Date("2023-9-5"), as.Date("2023-9-10"),
 "Midseason", "LFA35",    as.Date("2024-2-15"), as.Date("2024-2-17"),
 
 "Fishing_season", "LFA36",    as.Date("2023-11-14"), as.Date("2024-01-15"),
 "Fishing_season", "LFA36",    as.Date("2024-03-31"), as.Date("2024-06-29"),
  "Preseason", "LFA36",    as.Date("2023-10-15"), as.Date("2023-10-20"),
  "Midseason", "LFA36",    as.Date("2024-2-15"), as.Date("2024-2-17"),
  "Postseason", "LFA36",    as.Date("2024-7-15"), as.Date("2024-7-20"),
  'Science','LFA36',   as.Date("2024-2-18"), as.Date("2024-2-20"),
  
 "Fishing_season", "LFA38",    as.Date("2023-11-13"), as.Date("2024-06-29"),
  "Preseason", "LFA38",    as.Date("2023-10-15"), as.Date("2023-10-20"),
  "Midseason", "LFA38",    as.Date("2024-2-15"), as.Date("2024-2-17"),
  "Postseason", "LFA38",    as.Date("2024-7-15"), as.Date("2024-7-20"),
  'Science','LFA38',   as.Date("2024-2-18"), as.Date("2024-2-20"),
  
 "Fishing_season", "LFA41",    as.Date("2024-1-1"), as.Date("2024-12-31"),
  "Preseason", "LFA41",    as.Date("2024-11-15"), as.Date("2024-11-20"),
  
  "Other", "MRLAC",    as.Date("2024-03-15"), as.Date("2024-03-20"),
  "Other", "MRLAC",    as.Date("2024-10-15"), as.Date("2024-10-20"),
  
  'Other','TownHall', as.Date('2024-01-15'),as.Date('2024-01-20'),
  'Other','Indigenous / FN', as.Date('2023-09-5'),as.Date('2024-12-31'),
  ) %>%
  mutate(
    start_date = as.Date(start_date),
    end_date   = as.Date(end_date)
  ) %>%
  filter(!is.na(start_date), !is.na(end_date), end_date >= start_date)


p <- project



p$start_date <- as.Date(p$start_date)
p$end_date   <- as.Date(p$end_date)

seasons <- p %>%
  filter(wp =='Fishing_season')

preseason <- p %>%
  filter(wp =='Preseason')

midseason <- p %>%
  filter(wp =='Midseason')

postseason <- p %>%
  filter(wp =='Postseason')

science <- p %>%
  filter(wp =='Science')


other <- p %>%
  filter(wp =='Other')



ggplot() +
    geom_segment(
    data = seasons,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,
        colour = 'Fishing_season'),
    linewidth = 6
  ) +
  geom_segment(
    data = preseason,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,colour='Preseason'),
    linewidth = 6
  ) +
  geom_segment(
    data = postseason,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,colour='Postseason'),
    linewidth = 6
  ) +
  geom_segment(
    data = midseason,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,colour='Midseason'),
    linewidth = 6
  ) +
  geom_segment(
    data = other,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,colour='Other'),
    linewidth = 6
  ) +
  geom_segment(
    data = science,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,colour='Science'),
    linewidth = 6
  ) +
  
  labs(x = "Date",y="",title='Meeting Dates') +
  theme_minimal(base_size = 14)+
  scale_color_manual(
    name = "",       # legend title
    values = c(
      "Preseason"  = "pink",
      "Postseason" = "green",
      "Science" = 'lightblue',
      "Other" = 'Orange',
      "Midseason" = 'red',
      "Fishing_season"='black'
    ),guide = guide_legend(
      override.aes = list(size = 6)
    )
  )+

  scale_x_date(date_breaks = "1 month", date_labels = "%b ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

