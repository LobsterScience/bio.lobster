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
#lobster.db("observer41.redo")
Obs41 = lobster.db("observer41")


#########LANDINGS
land41 <- land[, c("YR", "LFA41")]
land41<- land41[land41$YR > 2001 & land41$YR <= 2026,]
land41 <- land41[order(land41$YR), ]



########### CSAS TEMPLATE ############

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

########### CSAS TEMPLATE ############

##Landings barplot
ggplot(land41,
  aes(x = factor(YR),y = LFA41,
    fill = ifelse(YR == max(YR, na.rm = TRUE), "Recent", "Other") )) +
  geom_col(color = "black", width = 0.8) +
  
  # Black bars, most recent year grey
  scale_fill_manual(
    values = c(
      "Other" = "black",
      "Recent" = "grey70" ) ) +
  # Dashed reference line
  geom_hline(
    yintercept = 720,
    colour = "red",
    linewidth = 1.2,
    linetype = "dashed" ) +
  
  labs(
    x = "Year",
    y = "Landings (t)") +
  
  scale_x_discrete(breaks = seq(2002, 2026, by = 2)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 900)) +
  
  theme_csas() +
  theme(
    legend.position = "none" )

##Landings Table 
landings41<-land41[, c("YR", "LFA41")]

write.csv(land41, "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 41 Update/landingstable.csv")


#########Observer Coverage #############
Obs41<-observer41
##Add SYEAR to 41 log data
L41log<-logs41
L41log$dates<-as.POSIXct(L41log$FV_FISHED_DATETIME, format='%m/%d/%Y %H:%M:%S')
L41log$dates = as.Date(L41log$dates,format='%m-%d-%Y')
L41log$MONTH = month(L41log$dates)
L41log$SYEAR= year(L41log$dates)
L41log$DAY=day(L41log$dates)

L41log$DDLON<-L41log$DDLON*-1
L41log<-L41log[L41log$SYEAR > 2010  & L41log$SYEAR < 2027, ]


##Observer Data
##Need SYear -- pull it out of date
Obs41$dates<-as.POSIXct(Obs41$STARTDATE, format='%Y-%m-%d %H:%M:%S')
Obs41$dates = as.Date(Obs41$dates,format='%m-%d-%Y')
Obs41$MONTH = month(Obs41$dates)
Obs41$SYEAR= year(Obs41$dates)
Obs41$DAY=day(Obs41$dates)

Obs41<-Obs41[Obs41$SYEAR > 2010  & Obs41$SYEAR < 2027, ]


##Number of observed Trips
Obs_trips <- aggregate(TRIPNO ~ SYEAR, data = Obs41, FUN = function(x) length(unique(x)))
names(Obs_trips)[2] <- "ObservedTrips"


##Number of Fishing Trips - from Logs
Fish_trips <- aggregate(TRIP_ID ~ SYEAR, data = L41log, FUN = function(x) length(unique(x)))
names(Fish_trips)[2] <- "TotalLogTrips"


##Number of Fishing Trips - From Slips
slip41$dates<-as.POSIXct(slip41$LANDING_DATE_TIME, format='%Y-%m-%d %H:%M:%S')
slip41$dates = as.Date(slip41$dates,format='%m-%d-%Y')
slip41$MONTH = month(slip41$dates)
slip41$SYEAR= year(slip41$dates)

slip41<-slip41[slip41$SYEAR > 2010  & slip41$SYEAR < 2027, ]

Slip_trips <- aggregate(LANDING_DATE_TIME ~ SYEAR, data = slip41, FUN = function(x) length(unique(x)))
names(Slip_trips)[2] <- "TotalSlipTrips"


Obs_Fish <- merge(Obs_trips, Fish_trips, by = "SYEAR")
ObsTable <- merge(Obs_Fish, Slip_trips, by = "SYEAR")

ObsTable$PercentObserved_logs<-(ObsTable$ObservedTrips/ObsTable$TotalSlipTrips)*100

ObsTable <- ObsTable %>%
  mutate(
    PercentObserved_logs = round(PercentObserved_logs, 2))

write.csv(ObsTable,file.path(figdir,"ObsTable26.csv"))

Obs41$obstrapID<-paste(Obs41$TRIPNO, Obs41$TRAPNO,Obs41$STRINGNO)
num_obtrap<-aggregate(obstrapID ~ SYEAR, data=Obs41, function(x) length(unique(x)))
colnames(num_obtrap)<-c("YEAR","OBSTOTALTRAPS")



loginfo = aggregate(NUM_OF_TRAPS~SYEAR,data=L41log,FUN=sum)

percentObs_traps<-merge(loginfo,num_obtrap, by.x="SYEAR",by.y='YEAR')
percentObs_traps$PerObserved<-(percentObs_traps$OBSTOTALTRAPS)/(percentObs_traps$NUM_OF_TRAPS)
percentObs_traps$PerObserved<-(percentObs_traps$PerObserved*100)

write.csv(percentObs_traps,file.path(figdir,"PercentTrapsObs.csv"))
