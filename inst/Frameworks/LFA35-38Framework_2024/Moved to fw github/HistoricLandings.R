#### LANDINGS ####

require(devtools)
require(ggplot2)
require(roxygen2)
require(geosphere)
require(lubridate)
require(bio.utilities)
require(bio.lobster)
require(rlang)
require(glue)
require(PBSmapping)
require(dplyr)
###################DATA IMPORT######################

#Viridis Magma
pal=c("#fcfdbf", 
      "#feb078",
      "#f1605d",
      "#b73779",
      "#721f81",
      "#2c115f",
      "#000004")

theme_set(theme_test(base_size = 14))

##### 1975-Today #####
#lobster.db('seasonal.landings.redo')
h=lobster.db('seasonal.landings')
h <- h %>% filter(row_number() <= n()-1)
h$SecondYear <- as.numeric(sub(".*-", "", h$SYEAR))

h35=h[,c('SecondYear','LFA35')]
h35<-h35 %>% 
  rename(LANDINGS_MT = LFA35,
         SYEAR = SecondYear)

h36=h[,c('SecondYear','LFA36')]
h36<-h36 %>% 
  rename(LANDINGS_MT = LFA36,
         SYEAR = SecondYear)

h38=h[,c('SecondYear','LFA38')]
h38<-h38 %>% 
  rename(LANDINGS_MT = LFA38,
         SYEAR = SecondYear)

#### 1947- 1974 ######
lobster.db('annual.landings.redo')
g=lobster.db('annual.landings')
g = subset(g,YR<1976)

g35=g[,c('YR','LFA35')]
g35<-g35 %>% 
  rename(LANDINGS_MT = LFA35,
         SYEAR = YR)

g36=g[,c('YR','LFA36')]
g36<-g36 %>% 
  rename(LANDINGS_MT = LFA36,
         SYEAR = YR)

g38=g[,c('YR','LFA38')]
g38<-g38 %>% 
  rename(LANDINGS_MT = LFA38,
         SYEAR = YR)


#### 1800s-1940s######
#lobster.db('historic.landings.redo')
d = lobster.db('historic.landings')  ## WILLIAMS Data
count35 = c("KINGS","ANNAPOLIS", "COLCHESTER" , "CUMBERLAND")
count36 = c('ALBERT','SAINT JOHN','CHARLOTTE')
count38 = c('CHARLOTTE')

d35 = subset(d,COUNTY %in% count35)
d35=d35[,c('SYEAR','LANDINGS_MT')]
d35 <- d35 %>%
  group_by(SYEAR) %>%
  summarize(TotalWeight = sum(LANDINGS_MT))
d35<-as.data.frame(d35)
d35<-d35 %>% 
  rename(LANDINGS_MT = TotalWeight)

d36 = subset(d,COUNTY %in% count36)
d36=d36[,c('SYEAR','LANDINGS_MT')]
d36 <- d36 %>%
  group_by(SYEAR) %>%
  summarize(TotalWeight = sum(LANDINGS_MT))
d36<-as.data.frame(d36)
d36<-d36 %>% 
  rename(LANDINGS_MT = TotalWeight)


d38 = subset(d,COUNTY %in% count38)
d38=d38[,c('SYEAR','LANDINGS_MT')]

land35 =rbind(h35,d35,g35)
land35<- land35%>%
  arrange(SYEAR)%>%
  tidyr::drop_na() %>%
  mutate(LFA = 35)

land36 =rbind(h36,d36,g36)
land36<- land36%>%
  arrange(SYEAR)%>%
  mutate(LFA = 36)

land38 =rbind(h38,d38,g38)
land38<- land38%>%
  arrange(SYEAR)%>%
  mutate(LFA = 38)


merge_df <- bind_rows(land35, land36, land38)


histland<-ggplot(merge_df, aes(x = SYEAR, y = LANDINGS_MT, fill = ifelse(SYEAR == 2024, "2024", "Other"))) +
  geom_bar(stat="identity", width=0.7) +
  facet_wrap(~ LFA, nrow = 1) + 
  theme_test() +
  labs(x = "Season Year", y = "Landings (t)") +
  scale_y_continuous(limits = c(0, 6200), expand = c(0, 0.1)) + 
  scale_x_continuous(breaks = seq(1892, 2024, by = 12), expand = c(0.01, 0)) + 
  scale_fill_manual(values = c("2024" = "#f1605d", "Other" = "#000004")) + 
  guides(fill = "none")

ggsave(filename = "histlandings.png", plot = histland, path = "C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures", width = 8, height = 6, units = "in", dpi = 300)

