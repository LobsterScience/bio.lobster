##RV survey overviews

require(bio.lobster)
require(devtools)
require(bio.utilities)
require(sf)
require(ggplot2)
require(tidyr)
theme_set(theme_test(base_size = 14))
fd=file.path(project.datadirectory('Update_LFA41'),'outputs','SURVEYS')
setwd(fd)

p = ggLobsterMap('41',addGrids = F)


##RV Survey
x = RV_sets41()
x = subset(x,month(x$DATE) %in% c(6,7,8))
xs = st_as_sf(x,coords = c('LONGITUDE','LATITUDE'),crs=4326)
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

xs = st_join(xs,rL,join=st_within)
xs = subset(xs,LFA %in% c(34,35,36,37,38))


xle = xs %>% pivot_longer(starts_with('P'))
xle$Length = as.numeric(substr(xle$name,3,8))
#xle= na.zero(xle,cols='value')
xxa = aggregate(value~Length+YEAR,data=xle,FUN=mean)
ggplot(subset(xxa,YEAR>1998),aes(x=Length,y=value))+geom_bar(stat='identity')+facet_wrap(~YEAR,scales = 'free_y')+xlab('Carapace Length')+ylab('Density') +theme_test(base_size = 14)+geom_vline(xintercept = 82.5,color='red')

br = round(seq(min(xs$YEAR),max(xs$YEAR),length=5))
lab = c(paste(br[1],br[2],sep="-"),paste(br[2]+1,br[3],sep="-"),paste(br[3]+1,br[4],sep="-"),paste(br[4]+1,br[5],sep="-"))
xs$rYR=cut(xs$YEAR,breaks=br,labels=lab,include.lowest = T,include.highest=F)
#to check
aggregate(YEAR~rYR,data=xs,FUN=function(x)c(min(x),max(x)))

ggLobsterMap('34-38',addGrids = F,addPoints = T,pts=subset(xs))+theme_test(base_size=14)

# ################################################################################################################################################


###NEFSC

n = NEFSC_sets()
n = subset(n,month(DATE)>8 & LONGITUDE> -68 & LATITUDE>42)
ns = st_as_sf(n,coords=c('LONGITUDE','LATITUDE'),crs=4326)
ggplot(ns)+geom_sf()+geom_sf(data=ns_coast,fill='black')+geom_sf(data=z,colour='red')+geom_sf(data=ys,colour='blue')



xle = n %>% pivot_longer(starts_with('P'))
xle$Length = as.numeric(substr(xle$name,3,8))
#xle= na.zero(xle,cols='value')
xxa = aggregate(value~Length+YEAR,data=xle,FUN=mean)
ggplot(subset(xxa, YEAR %in% floor(seq(min(xxa$YEAR),max(xxa$YEAR),length=12))),aes(x=Length,y=value))+geom_bar(stat='identity')+facet_wrap(~YEAR,scales='free_y')+xlab('Carapace Length')+ylab('Density') +theme_test(base_size = 14)+geom_vline(xintercept = 82.5,color='red')

br = round(seq(min(ns$YEAR),max(ns$YEAR),length=9))
lab = c(paste(br[1],br[2],sep="-"),paste(br[2]+1,br[3],sep="-"),paste(br[3]+1,br[4],sep="-"),paste(br[4]+1,br[5],sep="-"),paste(br[5]+1,br[6],sep="-"),paste(br[6]+1,br[7],sep="-"),paste(br[7]+1,br[8],sep="-"),paste(br[8]+1,br[9],sep="-"))
ns$rYR=cut(ns$YEAR,breaks=br,labels=lab,include.lowest = T,include.highest=F)
#to check
aggregate(YEAR~rYR,data=ns,FUN=function(x)c(min(x),max(x)))

ggLobsterMap('custom',xlim=c(-68,-63.5),ylim=c(42,46),addGrids = F,addPoints = T,pts=subset(ns),fw='~rYR')+theme_test(base_size=14)


#######################

#commercial weight proportion

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=T,species=2550,biomass = T,extend_ts = F)
y = subset(y, tot>0 & month(y$SET_DATE)<8 & LFA %in% c('L34','L35','L36','L37','L38'),select=c(TRIP_ID,SET_NO,SET_LONG,SET_LAT,SA_CORRECTED_PRORATED_N,tot))

x = RV_sets()
x = subset(x,month(x$DATE) %in% c(6,7,8))
xs = st_as_sf(x,coords = c('LONGITUDE','LATITUDE'),crs=4326)
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

xs = st_join(xs,rL,join=st_within)
xs$X=st_coordinates(xs)[,1]
xs$Y=st_coordinates(xs)[,2]
st_geometry(xs) <- NULL
xs = subset(xs,LFA %in% c(34,35,36,37,38,41,40,33) &!is.na(Legal_wt)& X< -64,select=c(mission,setno,X,Y,Legal_wt,WEIGHT_KG))

names(y)=names(xs)
y$mission = as.character(y$mission)
y$setno = as.character(y$setno)

xy = bind_rows(y,xs)
saveRDS(xy,'proportions_wt_82_300.rds')


#commercial numb proportion

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=T,species=2550,biomass = F,extend_ts = F)
y = subset(y, tot>0 & month(y$SET_DATE)<8 & LFA %in% c('L34','L35','L36','L37','L38'),select=c(TRIP_ID,SET_NO,SET_LONG,SET_LAT,SA_CORRECTED_PRORATED_N,tot))

x = RV_sets()
x = subset(x,month(x$DATE) %in% c(6,7,8))

xs = st_as_sf(x,coords = c('LONGITUDE','LATITUDE'),crs=4326)
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

xs = st_join(xs,rL,join=st_within)
xs$X=st_coordinates(xs)[,1]
xs$Y=st_coordinates(xs)[,2]
st_geometry(xs) <- NULL
xs = subset(xs,LFA %in% c(34,35,36,37,38,41,40,33) &!is.na(Legal_wt)& X< -64,select=c(mission,setno,X,Y,Legal,Lobster))

names(y)=names(xs)
y$mission = as.character(y$mission)
y$setno = as.character(y$setno)

xy = bind_rows(y,xs)
saveRDS(xy,'proportions82_300.rds')


#recruit numb proportion

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(70,82),aggregate=T,species=2550,biomass = F,extend_ts = F)
y = subset(y, tot>0 & month(y$SET_DATE)<8 & LFA %in% c('L34','L35','L36','L37','L38'),select=c(TRIP_ID,SET_NO,SET_LONG,SET_LAT,SA_CORRECTED_PRORATED_N,tot))

x = RV_sets()
x = subset(x,month(x$DATE) %in% c(6,7,8))

xle = x %>% pivot_longer(starts_with('P'))
xle$Length = as.numeric(substr(xle$name,3,8))
#xle= na.zero(xle,cols='value')
xxa = aggregate(value~mission+setno+Lobster+LATITUDE+LONGITUDE,data=subset(xle,Length %in% 70:82),FUN=sum)

xs = st_as_sf(xxa,coords = c('LONGITUDE','LATITUDE'),crs=4326)
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

xs = st_join(xs,rL,join=st_within)
xs$X=st_coordinates(xs)[,1]
xs$Y=st_coordinates(xs)[,2]
st_geometry(xs) <- NULL
xs = subset(xs,LFA %in% c(34,35,36,37,38,41,40,33) & X< -64,select=c(mission,setno,X,Y,value,Lobster))
names(xs)[5] = 'Recruits'
names(y)=names(xs)
y$mission = as.character(y$mission)
y$setno = as.character(y$setno)

xy = bind_rows(y,xs)
saveRDS(xy,'proportions70_82.rds')


######################################
#recruit numb proportion

y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(70,300),aggregate=T,species=2550,biomass = F,extend_ts = F)
y = subset(y, tot>0 & month(y$SET_DATE)<8 & LFA %in% c('L34','L35','L36','L37','L38'),select=c(TRIP_ID,SET_NO,SET_LONG,SET_LAT,SA_CORRECTED_PRORATED_N,tot))

x = RV_sets()
x = subset(x,month(x$DATE) %in% c(6,7,8))

xle = x %>% pivot_longer(starts_with('P'))
xle$Length = as.numeric(substr(xle$name,3,8))
#xle= na.zero(xle,cols='value')
xxa = aggregate(value~mission+setno+Lobster+LATITUDE+LONGITUDE,data=subset(xle,Length %in% 70:300),FUN=sum)

xs = st_as_sf(xxa,coords = c('LONGITUDE','LATITUDE'),crs=4326)
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326

xs = st_join(xs,rL,join=st_within)
xs$X=st_coordinates(xs)[,1]
xs$Y=st_coordinates(xs)[,2]
st_geometry(xs) <- NULL
xs = subset(xs,LFA %in% c(34,35,36,37,38,41,40,33) & X< -64,select=c(mission,setno,X,Y,value,Lobster))
names(xs)[5] = 'RecruitsComm'
names(y)=names(xs)
y$mission = as.character(y$mission)
y$setno = as.character(y$setno)

xy = bind_rows(y,xs)
saveRDS(xy,'proportions70_300.rds')

####################################################################
#####

fd=file.path(project.datadirectory('Framework_LFA35_38'),'outputs','SURVEYS')
setwd(fd)

survey = readRDS( file='summer_fall_survey_data_all_N_Sept24.rds')
survey = st_as_sf(survey)

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs


sf_use_s2(FALSE) #needed for cropping
survey <- suppressWarnings(suppressMessages(
  st_crop(survey,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


# Project our survey data coordinates:


ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))


ns_coast <- st_transform(ns_coast, crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)/1000
st_crs(ns_coast) <- crs_utm20

survey <- survey %>%   
  st_as_sf()

surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 
survey = subset(survey,!is.na(N))

ggplot(survey,aes(colour=Survey))+geom_sf()+geom_sf(data=ns_coast,fill='black')


