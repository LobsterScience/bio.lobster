

##### FRAMEWORK LANDINGS #####

setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Data Output")


require(devtools)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(ggplot2)
require(dplyr)

p=list()
assessment.year = 2024 ##Check Year
p$syr = 1989
p$yrs = p$syr:assessment.year


#Color palette
pal=c("#003f5c",
      "#2f4b7c",
      "#665191",
      "#a05195",
      "#d45087",
      "#f95d6a",
      "#ff7c43",
      "#ffa600")


###################### LOAD DATA ########################

#lobster.db('logs.redo') 
lobster.db('logs')

#lobster.db('process.logs.redo')
logs=lobster.db("process.logs")

#lobster.db('annual.landings.redo')
land =lobster.db("annual.landings")

#lobster.db('seasonal.landings.redo')
Sland = lobster.db('seasonal.landings')

#######################LFA 35#######################
a=lobster.db('process.logs')
a = subset(a,LFA==35)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS

h=lobster.db('seasonal.landings')
h
h=h[,c('SYEAR','LFA35')]
h=na.omit(h)
h$SYEAR=1976:2024

g=merge(b,h)
g$EFF = g$LFA35/g$CPUE


ggplot(g, aes(x = SYEAR)) +
  geom_bar(aes(y = LFA35, fill = ifelse(SYEAR == 2024, "2024", "Other")), 
           stat = "identity", width = 0.75) +
  # Point plot for EFF on secondary axis (excluding 2024)
  geom_point(data = g[g$SYEAR != 2024, ], aes(y = EFF * (4500 / max(g$EFF)), color = "Other"), size = 3) + ## y = EFF * (4500 / max(g$EFF) must match Max y axis for scaling
  # Line plot for EFF (excluding 2024)
  geom_line(data = g[g$SYEAR != 2024, ], aes(y = EFF * (4500 / max(g$EFF))), color = "#003f5c") +
  # Add triangle point for 2024 EFF
  geom_point(data = g[g$SYEAR == 2024, ], aes(y = EFF * (4500 / max(g$EFF)), color = "2024"), size = 4, shape = 17) +

  theme_test() +
  labs(x = "Season Year", y = "Landings (t)") +
  
  scale_y_continuous(
    limits = c(0, 4500),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . * (max(g$EFF) / 4500), name = "Effort ('000s Trap Hauls)")) + 

  scale_x_continuous(breaks = seq(2005, 2024, by = 3), expand = c(0.01, 0)) + 
  scale_fill_manual(values = c("2024" = "#fc8961", "Other" = "#003f5c")) + 
  scale_color_manual(values = c("2024" = "#fc8961", "Other" = "#003f5c")) + 
  guides(fill = "none", color = "none") +  # Remove legends
  theme(
    axis.text = element_text(size = 10),  
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 4700))  # Add room at top


subsamp<-g[g$SYEAR >2009 & g$SYEAR <2024,]
range(subsamp$LFA35)
median(subsamp$LFA35)



#######################LFA 36#######################
a=lobster.db('process.logs')
a = subset(a,LFA==36)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS

h=lobster.db('seasonal.landings')
h
h=h[,c('SYEAR','LFA36')]
h=na.omit(h)
h$SYEAR=1976:2024

g=merge(b,h)
g$EFF = g$LFA36/g$CPUE


ggplot(g, aes(x = SYEAR)) +
  geom_bar(aes(y = LFA36, fill = ifelse(SYEAR == 2024, "2024", "Other")), 
           stat = "identity", width = 0.75) +
  # Point plot for EFF on secondary axis (excluding 2024)
  geom_point(data = g[g$SYEAR != 2024, ], aes(y = EFF * (5000 / max(g$EFF)), color = "Other"), size = 3) + ## y = EFF * (4500 / max(g$EFF) must match Max y axis for scaling
  # Line plot for EFF (excluding 2024)
  geom_line(data = g[g$SYEAR != 2024, ], aes(y = EFF * (5000 / max(g$EFF))), color = "#003f5c") +
  # Add triangle point for 2024 EFF
  geom_point(data = g[g$SYEAR == 2024, ], aes(y = EFF * (5000 / max(g$EFF)), color = "2024"), size = 4, shape = 17) +
  
  theme_test() +
  labs(x = "Season Year", y = "Landings (t)") +
  
  scale_y_continuous(
    limits = c(0, 5000),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . * (max(g$EFF) /5000), name = "Effort ('000s Trap Hauls)")) + 
  
  scale_x_continuous(breaks = seq(2005, 2024, by = 3), expand = c(0.01, 0)) + 
  scale_fill_manual(values = c("2024" = "#fc8961", "Other" = "#003f5c")) + 
  scale_color_manual(values = c("2024" = "#fc8961", "Other" = "#003f5c")) + 
  guides(fill = "none", color = "none") +  # Remove legends
  theme(
    axis.text = element_text(size = 10),  
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0,5500))  # Add room at top

subsamp<-g[g$SYEAR >2009 & g$SYEAR <2024,]
range(subsamp$LFA36)
median(subsamp$LFA36)


#######################LFA 38#######################
a=lobster.db('process.logs')
a = subset(a,LFA==38)
b=aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR,data=a,FUN=sum)
b$CPUE=b$WEIGHT_KG/b$NUM_OF_TRAPS

h=lobster.db('seasonal.landings')
h
h=h[,c('SYEAR','LFA38')]
h=na.omit(h)
h$SYEAR=1976:2024

g=merge(b,h)
g$EFF = g$LFA38/g$CPUE


ggplot(g, aes(x = SYEAR)) +
  geom_bar(aes(y = LFA38, fill = ifelse(SYEAR == 2024, "2024", "Other")), 
           stat = "identity", width = 0.75) +
  # Point plot for EFF on secondary axis (excluding 2024)
  geom_point(data = g[g$SYEAR != 2024, ], aes(y = EFF * (7000 / max(g$EFF)), color = "Other"), size = 3) + ## y = EFF * (4500 / max(g$EFF) must match Max y axis for scaling
  # Line plot for EFF (excluding 2024)
  geom_line(data = g[g$SYEAR != 2024, ], aes(y = EFF * (7000 / max(g$EFF))), color = "#003f5c") +
  # Add triangle point for 2024 EFF
  geom_point(data = g[g$SYEAR == 2024, ], aes(y = EFF * (7000 / max(g$EFF)), color = "2024"), size = 4, shape = 17) +
  
  theme_test() +
  labs(x = "Season Year", y = "Landings (t)") +
  
  scale_y_continuous(
    limits = c(0, 7000),
    expand = c(0, 0),
    sec.axis = sec_axis(~ . * (max(g$EFF) / 7000), name = "Effort ('000s Trap Hauls)")) + 
  
  scale_x_continuous(breaks = seq(2005, 2024, by = 3), expand = c(0.01, 0)) + 
  scale_fill_manual(values = c("2024" = "#fc8961", "Other" = "#003f5c")) + 
  scale_color_manual(values = c("2024" = "#fc8961", "Other" = "#003f5c")) + 
  guides(fill = "none", color = "none") +  # Remove legends
  theme(
    axis.text = element_text(size = 10),  
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1))  +
  coord_cartesian(ylim = c(0,7500))  # Add room at top

subsamp<-g[g$SYEAR >2009 & g$SYEAR <2024,]
range(subsamp$LFA38)
median(subsamp$LFA38)


