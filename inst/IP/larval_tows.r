require(bio.lobster)
require("lubridate")
require(sf)
require(ggplot2)
# read data
#larval<-read.csv("C:/Users/Brad/Dropbox/Lobster/Data/Larval.csv")
larval<-read.csv(file.path(project.datadirectory("bio.lobster"),"data","Larval.csv"))

# add Year column
larval$Year<-year(larval$Date)

# missing values mean zero I assume
larval$Stage.I[is.na(larval$Stage.I)]<-0
larval$Stage.II[is.na(larval$Stage.II)]<-0
larval$Stage.III[is.na(larval$Stage.III)]<-0
larval$Stage.IV[is.na(larval$Stage.IV)]<-0


## Calculate Relative Volume

	larval$Volume<-larval$Meter.End-larval$Meter.Start

	# Assume meter rolls over at one million
	larval$Volume2<-larval$Volume
	larval$Volume2[larval$Volume<0&!is.na(larval$Volume)]<-larval$Meter.End[larval$Volume<0&!is.na(larval$Volume)]+10^6-larval$Meter.Start[larval$Volume<0&!is.na(larval$Volume)]
	
	# Remove outliers (assumed to be data errors)
	hist(larval$Volume2[larval$Volume2<100000],breaks=40)
	#abline(v=quantile(larval$Volume2,c(0.025,0.975),na.rm=T),col='red')
	abline(v=median(larval$Volume2,na.rm=T)+c(-20000,20000),col='blue') # this is a bit arbitrary but the point is to exclude points we think might be errors
	larval$Volume2[larval$Volume2<(median(larval$Volume2,na.rm=T)-20000)|larval$Volume2>(median(larval$Volume2,na.rm=T)+20000)]<-NA

	# Create standard volume column
	larval$Std.Volume<-larval$Volume2/median(larval$Volume2,na.rm=T)

	hist(larval$Std.Volume)
	# we see from this histogram that the volume often varies +/- 20% and sometimes by as much as  +/- 50% so it is 
	# introducing significant variablity to the larval counts which should be standardized by dividing by this column

	larval$Std.Stage.I<-larval$Stage.I/larval$Std.Volume
	larval$Std.Stage.II<-larval$Stage.II/larval$Std.Volume
	larval$Std.Stage.III<-larval$Stage.III/larval$Std.Volume
	larval$Std.Stage.IV<-larval$Stage.IV/larval$Std.Volume

	sites<-unique(larval$Site)
	
	s1<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.I,Year,mean))})
	s2<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.II,Year,mean))})
	s3<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.III,Year,mean))})
	s4<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.IV,Year,mean))})

	s1.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.I,Year,sd))})
	s2.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.II,Year,sd))})
	s3.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.III,Year,sd))})
	s4.se<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Stage.IV,Year,sd))})

	ss1<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.I,Year,mean,na.rm=T))})
	ss2<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.II,Year,mean,na.rm=T))})
	ss3<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.III,Year,mean,na.rm=T))})
	ss4<-sapply(1:length(sites),function(i){with(subset(larval,Site==sites[i]),tapply(Std.Stage.IV,Year,mean,na.rm=T))})


	larval$X = convert.dd.dddd(larval$Long*-1)
	larval$Y = convert.dd.dddd(larval$Lat)
	
	larval = subset(larval, !is.na(X))
	
	lar = st_as_sf(larval, coords=c('X','Y'),crs=4326)
	
	north_box = data.frame(xmin= -60.6,xmax=-60.3,ymin=46.85,ymax=46.97)
	south_box = data.frame(xmin= -60,xmax=-59.75,ymin=45.95,ymax=46.2)
	ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
	st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
	ns_coast <- suppressWarnings(suppressMessages(
	  st_crop(ns_coast,
	          c(xmin = -63, ymin = 44.7, xmax = -58.5, ymax = 47.1))))
	
	ggplot(ns_coast)+geom_sf(fill='wheat')+ 
	  geom_rect(data = north_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "transparent", color = "red")+
	geom_rect(data = south_box, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "transparent", color = "blue")
	
	
	lar$YEAR = year(lar$Date)
	
	
	ggplot(subset(lar,Std.Stage.I==0 ))+geom_sf(pch=3,size=.5)+
	  geom_sf(data=ns_coast,fill='wheat')+
	  geom_sf(data=subset(lar,Std.Stage.I>0 & Site=='False Bay'),aes(size=Std.Stage.I),pch=21,fill = alpha("red", 0.6))+
	  facet_wrap(~YEAR)+
	  coord_sf(xlim = c(south_box$xmin,south_box$xmax),
	           ylim = c(south_box$ymin,south_box$ymax),
	           expand = TRUE)
	
	
	ggplot(subset(lar,Std.Stage.I==0 ))+geom_sf(pch=3,size=.5)+
	  geom_sf(data=ns_coast,fill='wheat')+
	  geom_sf(data=subset(lar,Std.Stage.I>0 & Site=='Dingwall'),aes(size=Std.Stage.I),pch=21,fill = alpha("red", 0.6))+
	  facet_wrap(~YEAR)+
	  coord_sf(xlim = c(north_box$xmin,north_box$xmax),
	           ylim = c(north_box$ymin,north_box$ymax),
	           expand = TRUE)
	
	
#gcifa larvae
	gclar = read.csv(file.path(project.datadirectory('bio.lobster'),'data','GCIFA_larval_tow_2016-2023.csv'))
  
	gclar$Y = gsub(" ","",gclar$Latitude)
	gclar$X = gsub(" ","",gclar$Longitude)
	gclar$X = convert.dd.dddd(as.numeric(gclar$X)*-1)
	gclar$Y = convert.dd.dddd(as.numeric(gclar$Y))
	gclar = subset(gclar,select=c('X2016','X2017','X2018','X2019','X2020','X2021','X2022','X2023','X','Y'))
	names(gclar)[9] = 'Lon'
	
	glc = tidyr::pivot_longer(gclar,cols=starts_with('X'),names_to='Year',values_to='Larval_catch')
	glc$Year = as.numeric(substr(glc$Year,2,5))
  names(glc)[1] = 'X'
	
  glc = st_as_sf(glc,coords = c('X','Y'),crs=4326)
  
  ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
  st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
  
  ns_coast1 <- suppressWarnings(suppressMessages(
    st_crop(ns_coast,
            c(xmin = -64.5, ymin = 42.1, xmax = -59.8, ymax = 46.4))))
  
  box_data=data.frame(xmin = -61.5, ymin = 45.1, xmax = -60.8, ymax = 45.4)
  
  ggplot(ns_coast1)+geom_sf(fill='wheat')+
    geom_rect(data = box_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "transparent", color = "red")
  
  
  ns_coast <- suppressWarnings(suppressMessages(
    st_crop(ns_coast,
            c(xmin = -61.5, ymin = 45.1, xmax = -60.8, ymax = 45.4))))
  
  
  ggplot(subset(glc,Larval_catch>0))+
    geom_sf(aes(size=Larval_catch),pch=21,fill = alpha("red", 0.6))+
    geom_sf(data=subset(glc,Larval_catch==0),pch=3,size=.5)+
    facet_wrap(~Year)+
    geom_sf(data=ns_coast,fill='wheat')
  