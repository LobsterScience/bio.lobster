## LFA 41 Landings 


require(RODBC)
require(devtools)
require(roxygen2)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rlang)
require(bio.survey)
require(bio.lobster)
require(dplyr)
require(ggplot2)


p=list()
p$assessment.year = 2026 ##Check Year
p$syr = 1989
p$lfa=41
p$yrs = 1947:p$assessment.year
figdir = file.path("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/Figs")
p$lfas = c("41") # specify lfa

#lobster.db('process.logs41.redo')
logs41=lobster.db("process.logs41")
#lobster.db('annual.landings.redo')
land = lobster.db('annual.landings')


theme_csas <- function(base_size = 14, base_family = "", text_col = "grey20",
                       panel_border_col = "grey70") {
  half_line <- base_size / 2
  theme_light(base_size = base_size, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = text_col),
      strip.text.y = element_text(colour = text_col),
      axis.text = element_text(colour = text_col),
      axis.title = element_text(colour = text_col),
      legend.title = element_text(colour = text_col, size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.9), colour = text_col),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = text_col, size = rel(1)),
      plot.subtitle = element_text(colour = text_col, size = rel(.85))
    )
}

#########LANDINGS
land41 <- land[, c("YR", "LFA41")]
land41<- land41[land41$YR > 2001 & land41$YR <= 2026,]
land41 <- land41[order(land41$YR), ]
land41$TAC<- 720
land41$Vessels <-1
land41$Season<-land41$YR

land41 <- land41 %>%
  rename(Landings = LFA41)

land41 <- land41 %>%
  rename(Year = YR)

land41$Season<-as.character(land41$Season)
land41$TAC<-as.character(land41$TAC)


histland<-read.csv('C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/2026/datfiles2026/historiclandings.csv')


# Keep only desired seasons from histland
histland_sub <- histland %>%
  mutate(
    start_year = as.numeric(substr(Season, 1, 4))
  ) %>%
  filter(
    (nchar(Season) == 4 & start_year >= 1981 & start_year <= 1985) |
      (grepl("-", Season) & start_year >= 1985 & start_year <= 2000)
  ) %>%
  select(-start_year)

histland_sub$Landings<-as.numeric(histland_sub$Landings)

histland_sub$Year<-1981:2001


# Merge, keeping all rows from both datasets
land_all <- bind_rows(histland_sub,land41)

land_all <- land_all %>%
  rename(TACDes = TAC)

land_all <- land_all %>%
  mutate(
    TAC = as.numeric(sub(" .*", "", TACDes))
  )

missing_years <- data.frame(
  Season = as.character(1971:1980),
  Year = 1971:1980
)

land_all <- bind_rows(land_all, missing_years) %>%
  arrange(Year)

land_all <- land_all %>%
  mutate(
    TACDes = ifelse(Year %in% 1976:1980, "408 (4X)", TACDes),
    TAC = ifelse(Year %in% 1976:1980, 408, TAC) )

land_all$Vessels[land_all$Year %in% 1971:1975] <- 6
land_all$Vessels[land_all$Year %in% 1976:1980] <- 8

land_all$notes <- NA_character_
land_all$notes[land_all$Year == 1971] <- "Fishery Opened July 1971"
land_all$notes[land_all$Year == 1979] <- "LFA 40 Brood Stock Protection Closure"
land_all$notes[land_all$Year == 2013] <- "3-Year Quota Management Cycle "

season_years <- c(1985, 1986, 2004, 2006)
interim_tac <- c(2025, 2026)

ann_interim <- data.frame(
  Year = c(2025, 2026),
  y = c(720, 720)
)

ann <- land_all %>%
  filter(!is.na(notes)) %>%
  mutate(
    label_x = Year,
    label_y = 950,
    arrow_y = ifelse(is.na(Landings), 0, Landings + 20)
  )

ann_labels <- ann %>%
  filter(!Year %in% season_years)

ann_stars <- land_all %>%
  filter(Year %in% season_years) %>%
  mutate(
    star_y = ifelse(is.na(Landings), 25, Landings + 25)
  )



########### PLOT 

jpeg("newlandings_2026.jpeg", width =11, height = 13, units = "in",res=600, bg = "white")

ggplot(
  land_all,
  aes(x = Year, y = Landings)
) +
  
  # Landings bars
  geom_col(
    aes(fill = ifelse(
      Year == max(Year, na.rm = TRUE),
      "Recent", "Other"
    )),
    colour = "black",
    width = 0.8
  ) +
  
  # TAC line
  geom_line(
    data = land_all %>%
      filter(!is.na(TAC), Year <= 2024) %>%
      arrange(Year),
    aes(x = Year, y = TAC),
    colour = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_segment(
    data = ann_labels,
    aes(
      x = Year,
      y = label_y - 25,
      xend = Year,
      yend = arrow_y
    ),
    linewidth = 0.5,
    colour = "blue",
    arrow = arrow(
      length = unit(0.15, "cm"),
      type = "closed"
    ),
    inherit.aes = FALSE)+
  geom_text(
    data = ann_labels,
    aes(
      x = Year,
      y = label_y,
      label = notes
    ),
    angle = 25,
    colour = "blue",
    hjust = 0,
    vjust = 0,
    size = 3.75,
    inherit.aes = FALSE
  )+
  scale_fill_manual(
    values = c(
      "Other" = "black",
      "Recent" = "grey70"
    )
  ) +
  # Blue asterisks = season changes
  geom_text(
    data = ann_stars,
    aes(
      x = Year,
      y = star_y,
      label = "*"
    ),
    colour = "blue",
    size = 8,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  # Red diamond = interim TAC
  geom_point(
    data = ann_interim,
    aes(x = Year, y = y),
    shape = 18,      # diamond
    size = 4,
    colour = "red",
    inherit.aes = FALSE
  ) +
  labs(
    x = "Year",
    y = "Landings (t)"
  ) +
  
  scale_x_continuous(
    breaks = seq(1970, 2026, by = 2), 
    expand=c(0,1)
  ) +
  
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1150)
  ) +
  
  theme_csas() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

dev.off()