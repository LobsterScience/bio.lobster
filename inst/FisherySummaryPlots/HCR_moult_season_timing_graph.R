
library(dplyr)
library(ggplot2)

# -----------------------
project <- tibble::tribble(
  ~wp, ~Activity, ~start_date, ~end_date,
  "A", "LFA27",    as.Date("2024-05-15"), as.Date("2024-07-15"),
  "A", "LFA29",    as.Date("2024-04-28"), as.Date("2024-06-28"),
  "A", "LFA30",    as.Date("2024-05-17"), as.Date("2024-07-18"),
  "A", "LFA31A",   as.Date("2024-04-27"), as.Date("2024-06-28"),
  "A", "LFA31B",   as.Date("2024-04-19"), as.Date("2024-06-20"),
  "A", "LFA32",    as.Date("2024-04-19"), as.Date("2024-06-20"),
  "A", "LFA33",    as.Date("2023-11-26"), as.Date("2024-05-31"),
  "A", "LFA34",    as.Date("2023-12-02"), as.Date("2024-05-31"),
  "A", "LFA35",    as.Date("2023-10-12"), as.Date("2023-12-31"),
  "A", "LFA35",    as.Date("2024-02-28"), as.Date("2024-07-31"),
  "A", "LFA36",    as.Date("2023-11-14"), as.Date("2024-01-15"),
  "A", "LFA36",    as.Date("2024-03-31"), as.Date("2024-06-29"),
  "A", "LFA38",    as.Date("2023-11-13"), as.Date("2024-06-29"),
  "B", "Moulting", as.Date("2024-06-01"), as.Date("2024-09-30"),
  "B", "Moulting", as.Date("2023-06-01"), as.Date("2023-09-30"),
  "B", "Peak Moulting", as.Date("2023-06-15"), as.Date("2023-08-15"),
  "B", "Peak Moulting", as.Date("2024-06-15"), as.Date("2024-08-15"),
  "B", "Hardening", as.Date("2023-09-30"), as.Date("2023-10-15"),
  "B", "Hardening", as.Date("2024-09-30"), as.Date("2024-10-15")
) %>%
  mutate(
    start_date = as.Date(start_date),
    end_date   = as.Date(end_date)
  ) %>%
  filter(!is.na(start_date), !is.na(end_date), end_date >= start_date)


p <- project



p$start_date <- as.Date(p$start_date)
p$end_date   <- as.Date(p$end_date)

moulting_bg <- p %>%
  filter(Activity %in% c("Hardening", "Moulting", "Peak Moulting"))

moulting_bg$Activity <- factor(
  moulting_bg$Activity,
  levels = c("Moulting", "Peak Moulting", "Hardening")
)

activities <- p %>%
  filter(!Activity %in% c("Hardening", "Moulting", "Peak Moulting"))

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
  
  scale_fill_manual(
    values = c(
      "Moulting" = "green",
      "Peak Moulting" = "darkgreen",
      "Hardening" = "lightblue"
    )
  ) +
  
  labs(x = "Date", y = "LFA") +
  theme_minimal()+

  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

