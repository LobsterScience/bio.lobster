### Fishing Seasons timeline 
library(ggplot2)
library(lubridate)
library(dplyr)

fishing_seasons <- data.frame(
  Area = c("Area 35", "Area 35", "Area 36", "Area 36", "Area 38", "Area 38B"),
  Start = as.Date(c("2024-10-14", "2025-02-28", "2024-11-12", "2025-03-31", "2024-11-12", "2025-06-30")),
  End = as.Date(c("2024-12-31", "2025-07-31", "2025-01-14", "2025-06-29", "2025-06-29", "2025-11-07"))
)

fishing_seasons$Start[fishing_seasons$Start == as.Date("2024-11-12")] <- as.Date("2024-11-12") + (2 - wday(as.Date("2024-11-12"), week_start = 1)) %% 7
fishing_seasons$End[fishing_seasons$End == as.Date("2025-11-07")] <- as.Date("2025-11-07") + (2 - wday(as.Date("2025-11-07"), week_start = 1)) %% 7

ggplot(fishing_seasons, aes(x = Start, xend = End, y = Area, yend = Area)) +
  geom_segment(size = 10, color = "darkgrey") +
  labs(title = "Fishing Seasons Gantt Chart", x = "Date", y = "Area") +
  theme_minimal()
