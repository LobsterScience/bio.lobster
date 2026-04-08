
library(dplyr)
library(ggplot2)

df=readRDS('temp_catchabiliy_month.rds')
df$date = as.Date(paste('2024',1:12,'01',sep="-"))
df$date2 = as.Date(paste('2024',1:12,'31',sep="-"))
df$date2[c(4,6,9)] = as.Date(paste('2024',c(4,6,9),'30',sep="-"))
df$date2[c(2)] = as.Date(paste('2024',c(2),'28',sep="-"))
df$date2[c(10,12)] = as.Date(paste('2023',c(10,12),'31',sep="-"))
df$date[c(10,12)] = as.Date(paste('2023',c(10,12),'1',sep="-"))
df$date[c(11)] = as.Date(paste('2023',c(11),'1',sep="-"))
df$date2[c(11)] = as.Date(paste('2023',c(11),'30',sep="-"))

df$date2[c(2)] = as.Date(paste('2024',c(2),'28',sep="-"))

# -----------------------
project <- tibble::tribble(
  ~wp, ~T_Catchability, ~start_date, ~end_date,
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
  "A", "LFA38",    as.Date("2023-11-13"), as.Date("2024-06-29"))  %>%
  filter(!is.na(start_date), !is.na(end_date), end_date >= start_date)

proj2 = subset(df,select=c(month_exploitation,date,date2))
proj2$wp = 'B'
names(proj2)[1:3]=c('T_Catchability','start_date','end_date')
p = bind_rows(project,proj2)


moulting_bg <- p %>%
  filter(T_Catchability %in% c("low", "moderate",'high'))

moulting_bg$T_Catchability <- factor(
  moulting_bg$T_Catchability,
  levels =  c("low", "moderate",'high')
)

activities <- p %>%
  filter(!T_Catchability %in%c("low", "moderate",'high'))

ggplot() +
  geom_rect(
    data = moulting_bg,
    aes(xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = T_Catchability),
    alpha = 0.4
  ) +
  
  geom_segment(
    data = activities,
    aes(x = start_date,
        xend = end_date,
        y = T_Catchability,
        yend = T_Catchability),
    linewidth = 6
  ) +
  
  scale_fill_manual(
    values = c(
      "low" = "papayawhip",
      "moderate" = "greenyellow",
      "high" = 'salmon')
  ) +
  
  labs(x = "Date", y = "LFA") +
  theme_minimal()+

  scale_x_date(date_breaks = "1 month", date_labels = "%b ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
