library(tidyr)
library(dplyr)
library(ggplot2)
library(bio.lobster)
library(devtools)
la()

# Compute the year when each column reaches half of its max
da = lobster.db('seasonal.landings')
da$year = as.numeric(substring(da$SYEAR,6,9))
da$SYEAR<- NULL
da$LFAtotal = rowSums(da[,1:6],na.rm=T)
df <- da 


# Convert to long format for easier processing
df_long <- df %>%
  pivot_longer(cols = -year, names_to = "landing_type", values_to = "value") %>%
  group_by(landing_type) %>%
  mutate(half_max = max(value) / 2)

# Find the year closest to half-max
df_half_max <- df_long %>%
  filter(abs(value - half_max) == min(abs(value - half_max))) %>%
  select(landing_type, year) %>%
  rename(year_half_max = year)

# Find the year of maximum landing
df_max <- df_long %>%
  filter(value == max(value)) %>%
  select(landing_type, year) %>%
  rename(year_max = year)

# Combine results
df_summary <- left_join(df_half_max, df_max, by = "landing_type")

##us landings
x = lobster.db('uslandings_by_state')

##overall scaled
x <- x %>%
  group_by(Year,Stock) %>% 
  summarise(across(c(Metric.Tons),sum,na.rm=T)) %>%
  group_by(Stock) %>%
  mutate(scaled_value = (Metric.Tons-min(Metric.Tons))/(max(Metric.Tons)-min(Metric.Tons)))

# Plot
ggplot(x, aes(x = Year, y = scaled_value,colour=Stock)) +
  geom_line() +
  labs(y = "Scaled Landings")

###scaled to the max year in SNE == 1997

x_1997 <- x %>%
  filter(Year == 1997) %>%
  select(Stock, Metric.Tons) %>%
  rename(value_1997 = Metric.Tons)

x_scale_1997 <- x %>%
                left_join(x_1997,by='Stock') %>%
                mutate(scaled97 = Metric.Tons / value_1997)

ggplot(x_scale_1997, aes(x = Year, y = scaled97,colour=Stock)) +
  geom_line() +
  labs(y = "Landings Scaled to 1997")

#####add in Canadian GOM/Bof

a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')
d = lobster.db('historic.landings')
l34 = c("YARMOUTH","DIGBY")
l35 = c("KINGS","ANNAPOLIS", "COLCHESTER" , "CUMBERLAND")
l36 = c('ALBERT','SAINT JOHN','CHARLOTTE')
l38 = c('CHARLOTTE')

d$LFA = ifelse(d$COUNTY %in% l34, 'LFA34',NA)
d = subset(d, !is.na(LFA))
d = aggregate(LANDINGS_MT~SYEAR+LFA,data=d,FUN=sum)
d = subset(d, SYEAR<1947)

names(d) = c('YR','LFA','LAND')

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR<=2024)
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")
o =list()
LFA = c('LFA34','LFA35','LFA36','LFA38')
for(i in LFA){
  aa = a[,c('YR',i)]
  bb = b[,c('YR',i)]
  aa = rbind(aa,bb)
  aa = aa[order(aa$YR),]
  names(aa)[2] = 'Land'
  aa$LFA = i
  o[[i]] = aa
}
o = bind_rows(o)

x = aggregate(Land~YR,data=o,FUN=sum)

x$scaled97 = x$Land/x$Land[which(x$YR==1997)]


#combined can and us data
xUS = subset(x_scale_1997,select=c(Year,Stock, scaled97))
xCan = subset(x,select=c(YR,scaled97))
xCan$Stock = 'CanGoM'
xCan$Year = as.numeric(xCan$YR)
xCan$YR <- NULL

b = bind_rows(xUS,xCan)
b$Stock = ifelse(b$Stock=='GOM','USGoM',b$Stock)

sca_land = ggplot(b, aes(x = Year, y = scaled97,colour=Stock)) +
  geom_line(size=1.2) +
  labs(y = "Landings Scaled to 1997")+
  theme_test(base_size=14)


indall = readRDS(file.path(project.datadirectory('Assessment_LFA35_38'),'outputs','SURVEYS','IndicesFromFullComboModelFeb14_2000+_allareas.rds'))
g5 = ggplot(subset(indall),aes(x=year,y=est/1000,ymin=lwr/1000,ymax=upr/1000))+geom_point()+geom_line()+geom_ribbon(alpha=.25)+theme_test(base_size = 14)+labs(x='Year',y='Commercial Abundance (x000) ')


###################################################
#mapping objects
require(bio.lobster)
require(devtools)
require(sf)
require(ggplot2)
require(dplyr)
require(tidyr)
require(stars)
require(raster)
la()
sf_use_s2(FALSE) #needed for cropping


#other mapping objects
po = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFAPolysSF.rds')))
co = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','CoastSF.rds')))
us = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','US_FishingAreas.rds')))
us = subset(us,Stock %ni% 'CAN')
us = st_transform(us,crs=4326)

co = suppressWarnings(suppressMessages(st_crop(co,xmin=-74,ymin=40,xmax=-57,ymax=48)))

po = suppressWarnings(suppressMessages(st_crop(po,xmin=-74,ymin=40,xmax=-60,ymax=48)))

#bathymetry
#working with the bathymetry data
ras = terra::rast(file.path(git.repo,'bio.lobster.data','mapping_data','bathymetryRaster.tif'))

ras1 = ras
ras1[ras1<3 | ras1>600] <- NA

pou = st_transform(po,crs=32620)
cou = st_transform(co,crs=32620)
usu = st_transform(us,crs=32620)
zras1 = crop(ras1,extent(-153000,550000,4300000,5300000))
ff = as.data.frame(zras1,xy=TRUE)

cf = coord_sf(xlim=c(-149000,500000),ylim=c(4500000,5100000))

ma = ggplot()+
  geom_raster(data=ff,aes(x=x,y=y,fill=bathymetryRaster))+
  scale_fill_viridis_c()+
  geom_sf(data=subset(pou,LFA %in% c(34,35,36,37,38,40,41)), fill='gray',alpha=.4,colour='orange',linewidth=1.1)+
  geom_sf(data=cou, fill='wheat')+
  geom_sf(data=subset(usu,Stock %in% c('GOM','GB')), fill='gray',alpha=.4,colour='purple',linewidth=1.1)+
  geom_sf(data=subset(usu,Stock=='SNE'), fill='gray',alpha=.4,colour='green',linewidth=1.1)+
  
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  cf
cowplot::plot_grid( plot1_catchef,plot1_cpue, plot1_biomass,plot1_relF, ncol = 2, labels = "AUTO", align = "hv")

cowplot::plot_grid(
  ma,sca_land,g5,tm, ncol=2, labels = "AUTO")
