
library(dplyr)
library(ggplot2)

# -----------------------
project <- tibble::tribble(
  ~wp, ~Activity, ~start_date, ~end_date,
  "B", "Moulting", as.Date("2024-06-01"), as.Date("2024-09-30"),
  "B", "Moulting", as.Date("2023-06-01"), as.Date("2023-09-30"),
  "B", "Peak Moulting", as.Date("2023-06-15"), as.Date("2023-08-15"),
  "B", "Peak Moulting", as.Date("2024-06-15"), as.Date("2024-08-15"),
  "B", "Hardening", as.Date("2023-09-30"), as.Date("2023-10-15"),
  "B", "Hardening", as.Date("2024-09-30"), as.Date("2024-10-15"),
  
  "Survey", "ILTS",    as.Date("2023-06-01"), as.Date("2023-06-20"),
 "Survey", "ILTS",    as.Date("2024-06-01"), as.Date("2024-06-20"),
 "Survey", "ILTS",    as.Date("2023-09-01"), as.Date("2023-09-25"),
 "Survey", "ILTS",    as.Date("2024-09-01"), as.Date("2024-09-25"),
 
 "Survey", "RV_Summer",    as.Date("2023-07-01"), as.Date("2023-08-10"),
 "Survey", "RV_Summer",    as.Date("2024-07-01"), as.Date("2024-08-10"),
 "Survey", "RV_Winter",    as.Date("2024-02-15"), as.Date("2024-03-15"),
 "Survey", 'Scallop',  as.Date("2024-06-1"), as.Date("2024-10-15"),
 "Survey", 'Scallop',  as.Date("2023-06-1"), as.Date("2023-10-15"),
 
 "Fishing_season", "LFA34",    as.Date("2023-12-02"), as.Date("2024-05-31"),
 "Fishing_season", "LFA35",    as.Date("2023-10-12"), as.Date("2023-12-31"),
 "Fishing_season", "LFA35",    as.Date("2024-02-28"), as.Date("2024-07-31"),

 "Fishing_season", "LFA36",    as.Date("2023-11-14"), as.Date("2024-01-15"),
 "Fishing_season", "LFA36",    as.Date("2024-03-31"), as.Date("2024-06-29"),

 "Fishing_season", "LFA38",    as.Date("2023-11-13"), as.Date("2024-06-29"),

 "Fishing_season", "LFA41",    as.Date("2023-6-1"), as.Date("2024-10-15")
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

surveys <- p %>%
  filter(wp =='Survey')


moulting_bg <- p %>%
  filter(Activity %in% c("Hardening", "Moulting", "Peak Moulting"))

moulting_bg$Activity <- factor(
  moulting_bg$Activity,
  levels = c("Moulting", "Peak Moulting", "Hardening")
)

activities <- p %>%
  filter(!Activity %in% c("Hardening", "Moulting", "Peak Moulting"))

activity_levels <- c(
  "Scallop",
  "RV_Winter",
  "RV_Summer",
  "ILTS",     # ← move ILTS up here
  "LFA41",
  "LFA38",
  "LFA36",
  "LFA35",
  "LFA34"
)

activities$Activity <- factor(activities$Activity, levels = activity_levels)
seasons$Activity    <- factor(seasons$Activity, levels = activity_levels)
surveys$Activity    <- factor(surveys$Activity, levels = activity_levels)


ggplot() +
  geom_rect(
    data = moulting_bg,
    aes(xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = Activity),
    alpha = 0.4
  ) +
  
  geom_segment(
    data = activities,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity),
    linewidth = 6
  ) +
  
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
    data = surveys,
    aes(x = start_date,
        xend = end_date,
        y = Activity,
        yend = Activity,colour='Survey'),
    linewidth = 6
  ) +
  labs(x = "Date",y="",title='Survey Timing') +
  theme_minimal(base_size = 14)+
  scale_fill_manual(
    values = c(
      "Moulting" = "green",
      "Peak Moulting" = "darkgreen",
      "Hardening" = "lightblue"
      ),name=NULL,guide = guide_legend(
      override.aes = list(size = 6)
    )
  )+
  scale_colour_manual(
    values = c(
      "Survey"  = "pink",
      "Fishing_season"='black'
    ),name=NULL,guide = guide_legend(
      override.aes = list(size = 6)
    )
  )+
  scale_x_date(date_breaks = "1 month", date_labels = "%b ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

