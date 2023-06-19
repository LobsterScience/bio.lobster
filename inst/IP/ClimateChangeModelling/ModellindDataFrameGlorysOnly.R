
require(sdmTMB)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
require(dplyr)
require(ggplot2)
require(INLA)
options(stringAsFactors=F)
require(PBSmapping)
require(SpatialHub)
require(sf)
la()
fd=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling')
dir.create(fd,showWarnings=F)
setwd(fd)

aT = compileAbundPresAbs(redo=F,size=F)

survey <- aT %>%   
  st_as_sf(coords=c('LONGITUDE',"LATITUDE"),crs=4326) %>% st_transform(32620)

st_geometry(survey) = st_geometry(survey)/1000
st_crs(survey) = 32620

#survey = subset(survey, YEAR %in% 1991:2022)
survey$W = ceiling(yday(survey$DATE)/366*25)

ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

				 ss = st_nearest_feature(survey,ba)
       	 ds = st_distance(survey,ba[ss,],by_element=T)
       	 st_geometry(ba) = NULL
       	 survey$z = ba$z[ss]
       	 survey$z_dist = as.numeric(ds)
       	 survey = subset(survey,z_dist<1)

s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k = k[-grep('2020_',k)]
#k = k[-grep('199',k)]
survey = subset(survey,YEAR>1992)

ol = list()
m=0
for(i in 1:length(k)){
			h = readRDS(k[i])
			h$Date = as.Date(h$Date)
			y = unique(year(h$Date))
			#h$W  = ceiling(yday(h$Date)/366*25)
			#h = aggregate(bottomT~W+X+Y,data=h,FUN=median)
			h = h %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
			st_geometry(h) = st_geometry(h)/1000
			st_crs(h) = 32620

			x1 = subset(survey,YEAR == y)
			x1$Date = as.Date(x1$DATE)
			uW = unique(x1$Date)
					for(j in 1:length(uW)){
									m=m+1
									nn = subset(x1,Date==uW[j])
						hh = subset(h,Date==uW[j])
						ou = st_nearest_feature(nn,hh)
       	 ds = st_distance(nn,hh[ou,],by_element=T)
       	 st_geometry(hh) = NULL
       	 nn$GlT = hh$bottomT[ou]
       	 nn$GlD = as.numeric(ds)
			ol[[m]] = nn       	
			}
		}

crs_utm20 <- 32620
da = dplyr::bind_rows(ol)	
da = st_as_sf(da)
da = subset(da,GlD<6) #remove ~5% outliers 
i = which(da$TEMP>30)
da$TEMP[i]=NA
i = which(da$TEMP==0)
da$TEMP[i]=NA
i = which(da$TEMP== -99)
da$TEMP[i]=NA

i = which(da$GlT<3 & da$TEMP>12)
da$TEMP[i]=NA
da$Tdiff = da$GlT - da$TEMP
hist(da$Tdiff)
table(subset(da,abs(Tdiff)>3)$SOURCE)
table(subset(da,(Tdiff)>3)$SOURCE)
###>70% of temp data was within 3C, and of those sets that were under predicted (-Tdiff), >95% had lobster suggesting, if anything PA, temperature relationships are biased low

#redo jan 31 2022
saveRDS(da,file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))


#prediction grids from glorys
s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
#k = k[-grep('2020_',k)]

ol = list()
m=0
for(i in 1:length(k)){
			h = readRDS(k[i])
			h$Date = as.Date(h$Date)
			h$m = month(h$Date)
			h$yr = unique(year(h$Date))
			h$Q = ifelse(h$m %in% c(10,11,12),1,ifelse(h$m %in% c(1,2,3),2,ifelse(h$m %in% c(4,5,6),3,4)))
			h = aggregate(bottomT~Q+X+Y+yr,data=h,FUN=mean)
			h = h %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
			st_geometry(h) = st_geometry(h)/1000
			st_crs(h) = 32620
			m=m+1
		ol[[m]] = h       	
			}
		

crs_utm20 <- 32620
da = dplyr::bind_rows(ol)	
da = st_as_sf(da)


ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

				 ss = st_nearest_feature(da,ba)
       	 ds = st_distance(da,ba[ss,],by_element=T)
       	 st_geometry(ba) = NULL
       	 da$z = ba$z[ss]
       	 da$z_dist = as.numeric(ds)
#by quarter     	
saveRDS(da,'GlorysPredictSurface.rds')


#bymonth

s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
#k = k[-grep('2020_',k)]

ol = list()
m=0
for(i in 1:length(k)){
			h = readRDS(k[i])
			h$Date = as.Date(h$Date)
			h$m = month(h$Date)
			h$yr = unique(year(h$Date))
			h = aggregate(bottomT~m+X+Y+yr,data=h,FUN=mean)
			h = h %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
			st_geometry(h) = st_geometry(h)/1000
			st_crs(h) = 32620
			m=m+1
		ol[[m]] = h       	
			}
		

crs_utm20 <- 32620
da = dplyr::bind_rows(ol)	
da = st_as_sf(da)


ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

				 ss = st_nearest_feature(da,ba)
       	 ds = st_distance(da,ba[ss,],by_element=T)
       	 st_geometry(ba) = NULL
       	 da$z = ba$z[ss]
       	 da$z_dist = as.numeric(ds)
#by month     	
saveRDS(da,'GlorysPredictSurfaceMonth.rds')
