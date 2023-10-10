#making daily temps from HAD MPI and Can
require(bio.lobster)
require(sf)
require(progress)
fd=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling')
dir.create(fd,showWarnings=F)
setwd(fd)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620

d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
dd = d[grep('Had',d)]
dd = dd[c(16:22,34:36,40:42,45:47)]
outs=list()
m=0
for(i in 1:length(dd)){
  h = read.table(dd[i],header=T)
  h$Date = as.Date(as.character(h$DateYYYYMMDD), format='%Y%m%d')
  h$id = paste(h$Latitude,h$Longitude,sep="_")
  hid = unique(h$id)
  for(j in 1:length(hid)){
    m=m+1
    hh = subset(h,id==hid[[j]])
    v=seq(min(hh$Date),max(hh$Date),'1 day')
    interpD = approx(hh$Date,hh$BottomTemp,xout=v)
    outs[[m]] = data.frame(Date=interpD$x,Longitude=unique(hh$Longitude),Latitude=unique(hh$Latitude),BottomTemp=interpD$y,Depth_m=unique(hh$Depth_m))
      }
  }
  
 outsc= dplyr::bind_rows(outs)

 outsc$yr = lubridate::year(outsc$Date)
 outsc$doy = lubridate::yday(outsc$Date)

 outscl = split(outsc,f=outsc$yr)


 for(i in 1:length(outscl)){
    if(i==1){
    data = outscl[[i]]
    data = st_as_sf(data,coords=c("Longitude",'Latitude'),crs=4326)
    next()
    }
    o = outscl[[i]]
    o$geometry <- NULL
    data$new=NA
    names(data)[ncol(data)]=paste('X',unique(o$yr),sep=".")
    doyy = unique(o$doy)
  
   junk = list()
    for(j in 1:length(doyy)){
         oo = subset(o,doy==doyy[j])
        oos = st_as_sf(oo,coords=c('Longitude','Latitude'),crs=4326)
        dataS = subset(data,doy==doyy[j])
        dist = st_nearest_feature(dataS,oos)
    dataS[,ncol(dataS)] = oo[dist,'BottomTemp']
    junk[[j]] = dataS
    }
    data = dplyr::bind_rows(junk)    
 }

 saveRDS(data,file='HadDailyTemps.rds')

#######################################################################
##Can

d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
dd = d[grep('Can',d)]
dd = dd[c(16:22,34:36,40:42,45:47)]
outs=list()
m=0
for(i in 1:length(dd)){
  h = read.table(dd[i],header=T)
  h$Date = as.Date(as.character(h$DateYYYYMMDD), format='%Y%m%d')
  h$id = paste(h$Latitude,h$Longitude,sep="_")
  hid = unique(h$id)
  for(j in 1:length(hid)){
    m=m+1
    hh = subset(h,id==hid[[j]])
    v=seq(min(hh$Date),max(hh$Date),'1 day')
    interpD = approx(hh$Date,hh$BottomTemp,xout=v)
    outs[[m]] = data.frame(Date=interpD$x,Longitude=unique(hh$Longitude),Latitude=unique(hh$Latitude),BottomTemp=interpD$y,Depth_m=unique(hh$Depth_m))
      }
  }
  
 outsc= dplyr::bind_rows(outs)

 outsc$yr = lubridate::year(outsc$Date)
 outsc$doy = lubridate::yday(outsc$Date)

 outscl = split(outsc,f=outsc$yr)


 for(i in 1:length(outscl)){
    if(i==1){
    data = outscl[[i]]
    data = st_as_sf(data,coords=c("Longitude",'Latitude'),crs=4326)
    next()
    }
    o = outscl[[i]]
    o$geometry <- NULL
    data$new=NA
    names(data)[ncol(data)]=paste('X',unique(o$yr),sep=".")
    doyy = unique(o$doy)
  
   junk = list()
    for(j in 1:length(doyy)){
         oo = subset(o,doy==doyy[j])
        oos = st_as_sf(oo,coords=c('Longitude','Latitude'),crs=4326)
        dataS = subset(data,doy==doyy[j])
        dist = st_nearest_feature(dataS,oos)
    dataS[,ncol(dataS)] = oo[dist,'BottomTemp']
    junk[[j]] = dataS
    }
    data = dplyr::bind_rows(junk)    
 }

 saveRDS(data,file='CanDailyTemps.rds')

 ############################################################################
 #MPI

dd = d[grep('MPI',d)]
dd = dd[c(16:22,34:36,40:42,45:47)]
outs=list()
m=0
for(i in 1:length(dd)){
  h = read.table(dd[i],header=T)
  h$Date = as.Date(as.character(h$DateYYYYMMDD), format='%Y%m%d')
  h$id = paste(h$Latitude,h$Longitude,sep="_")
  hid = unique(h$id)
  for(j in 1:length(hid)){
    m=m+1
    hh = subset(h,id==hid[[j]])
    v=seq(min(hh$Date),max(hh$Date),'1 day')
    interpD = approx(hh$Date,hh$BottomTemp,xout=v)
    outs[[m]] = data.frame(Date=interpD$x,Longitude=unique(hh$Longitude),Latitude=unique(hh$Latitude),BottomTemp=interpD$y,Depth_m=unique(hh$Depth_m))
      }
  }
  
 outsc= dplyr::bind_rows(outs)

 outsc$yr = lubridate::year(outsc$Date)
 outsc$doy = lubridate::yday(outsc$Date)

 outscl = split(outsc,f=outsc$yr)


 for(i in 1:length(outscl)){
    if(i==1){
    data = outscl[[i]]
    data = st_as_sf(data,coords=c("Longitude",'Latitude'),crs=4326)
    next()
    }
    o = outscl[[i]]
    o$geometry <- NULL
    data$new=NA
    names(data)[ncol(data)]=paste('X',unique(o$yr),sep=".")
    doyy = unique(o$doy)
  
   junk = list()
    for(j in 1:length(doyy)){
         oo = subset(o,doy==doyy[j])
        oos = st_as_sf(oo,coords=c('Longitude','Latitude'),crs=4326)
        dataS = subset(data,doy==doyy[j])
        dist = st_nearest_feature(dataS,oos)
    dataS[,ncol(dataS)] = oo[dist,'BottomTemp']
    junk[[j]] = dataS
    }
    data = dplyr::bind_rows(junk)    
 }

 saveRDS(data,file='MPIDailyTemps.rds')
