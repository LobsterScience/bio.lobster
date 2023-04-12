# getting prediction surfaces from temp
#run GLORYStemperatureData. R first
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(sf)
require(forecast)
fd=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling')
dir.create(fd,showWarnings=F)
setwd(fd)

#GlorysCliamtologies 1993-2019
s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k = k[-grep('2022',k)]

for(i in 1:length(k)){
  a=Sys.time()
      h = readRDS(k[i])
      print(k[i])
      if(i==1){
       hdim =dim(h)
        h$Date = as.Date(h$Date)
        h$doy = lubridate::yday(h$Date)
        h1 = aggregate(bottomT~doy+X+Y,data=h,FUN=mean)
        names(h1)[4]=paste("BT",unique(year(h$Date)),sep=".")
        out = h1
        h1 = st_as_sf(h1,coords =c('X',"Y"),crs=4326)
        h1 = h1 %>% st_transform(32620)
        st_geometry(h1) = st_geometry(h1)/1000
        st_crs(h1) = 32620
          rm(h)
      } else {
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Date = as.Date(h$Date)
        h$doy = lubridate::yday(h$Date)
        h2 = aggregate(bottomT~doy+X+Y,data=h,FUN=mean)
        names(h2)[4]= y
     
        o = list()
        for(j in 1:365){
        h4 = subset(h2,doy==j)
        h5 = subset(h1,doy==j)
        h3 = st_as_sf(h4,coords =c('X',"Y"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        st_geometry(h3) = st_geometry(h3)/1000
        st_crs(h3) = 32620
        ou = st_nearest_feature(h5,h3)
        o[[j]] = h4[ou,]
        } 
        h2 = as.data.frame(do.call(rbind,o))
        if((dim(out)[1] != dim(h2)[1])) browser()
        out = merge(out,h2,all.x=T)
        rm(h3)
       }
       print(Sys.time()-a)
}

g = grep('BT',names(out))

  combineClims = function(x,cols,ind){
      x1 = do.call(cbind,x[,cols])
      apply(x1[,seq(ind,ncol(x1),by=7)],1,mean)
    }

#current Climatology
gcur = c(grep('200',names(out)),grep('201',names(out)))
out$Clim0.01  = combineClims(out,g,1)
out$Clim0.025 = combineClims(out,g,2)
out$Clim0.25  = combineClims(out,g,3)
out$Clim0.5   = combineClims(out,g,4)
out$Clim0.75  = combineClims(out,g,5)
out$Clim0.975 = combineClims(out,g,6)
out$Clim0.99  = combineClims(out,g,7)


out1 = out %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
      st_geometry(out1) = st_geometry(out1)/1000
      st_crs(out1) = 32620


#adding bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

         ss = st_nearest_feature(out1,ba)
         summary(st_distance(out1,ba[ss,],by_element=T)) #any weird dists?
         st_geometry(ba) = NULL
         out1$z = ba$z[ss]

saveRDS(list(out,out1),file='GlorysClimatologies1993-2021byDOY.rds')


##updating for just the fc years
s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k = k[grep('fc',k)]

    out1=readRDS(file='GlorysClimatologies1993-2021byDOY.rds')[[2]]
    out12 = subset(out1,select=c(-BT.2021,-BT.2020))
    
for(i in 1:length(k)){
  a=Sys.time()
      h = readRDS(k[i])
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Date = as.Date(h$Date)
        h$doy = lubridate::yday(h$Date)
        h2 = aggregate(bottomT~doy+X+Y,data=h,FUN=mean)
        h2 = subset(h2,doy<366)
        names(h2)[4]= y
     
        o = list()
        for(j in 1:365){
        h4 = subset(h2,doy==j)
        h5 = subset(out12,doy==j)
        xx = dim(h5)[2]
        h3 = st_as_sf(h4,coords =c('X',"Y"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        st_geometry(h3) = st_geometry(h3)/1000
        st_crs(h3) = 32620
        ou = st_nearest_feature(h5,h3)
        o[[j]] = cbind(h5,h4[ou,y])
         } 
        h2 = bind_rows(o)
        out12 = st_as_sf(h2)
        names(out12)[ncol(out12)-1]= y
     
        rm(h3)
       }
       print(Sys.time()-a)
}

   saveRDS(out12, file='GlorysClimatologies1993-2022byDOY.rds')

########################################################################################
#Glorys Climatologies 2000-2019
s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k = k[-grep('fc_',k)]
k = k[-grep('199',k)]
k = k[-grep('2020',k)]

for(i in 1:length(k)){
      h = readRDS(k[i])
      print(k[i])
      if(i==1){
       hdim =dim(h)
        h$Date = as.Date(h$Date)
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h1 = aggregate(bottomT~Q+X+Y,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h1)[4]=paste("BT",unique(year(h$Date)),sep=".")
        out = h1
        h1 = st_as_sf(h1,coords =c('X',"Y"),crs=4326)
        h1 = h1 %>% st_transform(32620)
        st_geometry(h1) = st_geometry(h1)/1000
        st_crs(h1) = 32620
          rm(h)
      } else {
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Date = as.Date(h$Date)
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h2 = aggregate(bottomT~Q+X+Y,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h2)[4]= y
        o = list()
        for(j in 1:4){
        h4 = subset(h2,Q==j)
        h5 = subset(h1,Q==j)
        h3 = st_as_sf(h4,coords =c('X',"Y"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        st_geometry(h3) = st_geometry(h3)/1000
        st_crs(h3) = 32620
        ou = st_nearest_feature(h5,h3)
        o[[j]] = h4[ou,]
        }
        h2 = as.data.frame(do.call(rbind,o))
        out = merge(out,h2)
        rm(h3)
       }
}

g = grep('BT',names(out))

  combineClims = function(x,cols,ind){
      x1 = do.call(cbind,x[,cols])
      apply(x1[,seq(ind,ncol(x1),by=7)],1,mean)
    }

#current Climatology
gcur = c(grep('200',names(out)),grep('201',names(out)))
out$Clim0.01  = combineClims(out,g,1)
out$Clim0.025 = combineClims(out,g,2)
out$Clim0.25  = combineClims(out,g,3)
out$Clim0.5   = combineClims(out,g,4)
out$Clim0.75  = combineClims(out,g,5)
out$Clim0.975 = combineClims(out,g,6)
out$Clim0.99  = combineClims(out,g,7)


out1 = out %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
      st_geometry(out1) = st_geometry(out1)/1000
      st_crs(out1) = 32620


#adding bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

         ss = st_nearest_feature(out1,ba)
         summary(st_distance(out1,ba[ss,],by_element=T)) #any weird dists?
         st_geometry(ba) = NULL
         out1$z = ba$z[ss]
         
ggplot(out1) +
  geom_sf(aes(fill=Clim0.5,color=Clim0.5)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('GlorysClimatologies2000-2019byQ.png') 

saveRDS(list(out,out1),file='GlorysClimatologies2000-2019byQ.rds')

###########################################################################
##HAD
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
dd = d[grep('Had',d)]
##Can
crs_utm20 <- 32620
ou = list()

for(i in 1:length(dd)){
  h = read.table(dd[i],header=T)
  print(dd[i])
  h$Date = as.Date(as.character(h$DateYYYYMMDD), format='%Y%m%d')
      if(i==1){
       hdim =dim(h)
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h1 = aggregate(BottomTemp~Q+Longitude+Latitude,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h1)[4]=paste("BT",unique(year(h$Date)),sep=".")
        out = h1
        h1 = st_as_sf(h1,coords =c('Longitude',"Latitude"),crs=4326)
        h1 = h1 %>% st_transform(32620)
        st_geometry(h1) = st_geometry(h1)/1000
        st_crs(h1) = 32620
          rm(h)
      } else {
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h2 = aggregate(BottomTemp~Q+Longitude+Latitude,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h2)[4]= y
        o = list()
        for(j in 1:4){
        h4 = subset(h2,Q==j)
        h5 = subset(h1,Q==j)
        h3 = st_as_sf(h4,coords =c('Longitude',"Latitude"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        st_geometry(h3) = st_geometry(h3)/1000
        st_crs(h3) = 32620
        ou = st_nearest_feature(h5,h3)
        o[[j]] = h4[ou,]
        }
        h2 = as.data.frame(do.call(rbind,o))
        out = merge(out,h2)
        rm(h3)
       }
}


#current Climatology
gcur = c(grep('200',names(out)),grep('201',names(out)))
out$CurClim0.01  = combineClims(out,gcur,1)
out$CurClim0.025 = combineClims(out,gcur,2)
out$CurClim0.25  = combineClims(out,gcur,3)
out$CurClim0.5   = combineClims(out,gcur,4)
out$CurClim0.75  = combineClims(out,gcur,5)
out$CurClim0.975 = combineClims(out,gcur,6)
out$CurClim0.99  = combineClims(out,gcur,7)

#30sClim
gcur = c(grep('203',names(out)))

out$ThirClim0.01  = combineClims(out,gcur,1)
out$ThirClim0.025 = combineClims(out,gcur,2)
out$ThirClim0.25  = combineClims(out,gcur,3)
out$ThirClim0.5   = combineClims(out,gcur,4)
out$ThirClim0.75  = combineClims(out,gcur,5)
out$ThirClim0.975 = combineClims(out,gcur,6)
out$ThirClim0.99  = combineClims(out,gcur,7)

#50sClim
gcur = c(grep('205',names(out)))

out$FiftClim0.01  = combineClims(out,gcur,1)
out$FiftClim0.025 = combineClims(out,gcur,2)
out$FiftClim0.25  = combineClims(out,gcur,3)
out$FiftClim0.5   = combineClims(out,gcur,4)
out$FiftClim0.75  = combineClims(out,gcur,5)
out$FiftClim0.975 = combineClims(out,gcur,6)
out$FiftClim0.99  = combineClims(out,gcur,7)

#90sClim
gcur = c(grep('209',names(out)))
out$NineClim0.01  = combineClims(out,gcur,1)
out$NineClim0.025 = combineClims(out,gcur,2)
out$NineClim0.25  = combineClims(out,gcur,3)
out$NineClim0.5   = combineClims(out,gcur,4)
out$NineClim0.75  = combineClims(out,gcur,5)
out$NineClim0.975 = combineClims(out,gcur,6)
out$NineClim0.99  = combineClims(out,gcur,7)


##diffs from Climate

  out$Diff30 = out$ThirClim0.5 - out$CurClim0.5
  out$Diff50 = out$FiftClim0.5 - out$CurClim0.5
  out$Diff90 = out$NineClim0.5 - out$CurClim0.5

out2 = out %>% st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% st_transform(32620)
      st_geometry(out2) = st_geometry(out2)/1000
      st_crs(out2) = 32620


#adding bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

         ss = st_nearest_feature(out2,ba)
         summary(st_distance(out2,ba[ss,],by_element=T)) #any weird dists?
         st_geometry(ba) = NULL
         out2$z = ba$z[ss]
         
ggplot(out2) +
  geom_sf(aes(fill=Diff30,color=Diff30)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('HadClimatologies2000-2019byQ.png') 

saveRDS(list(out,out2),file='HadClimatologiesDiffsbyQ.rds')


###############################################################
###########################################################################
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
##Can
dd = d[grep('Can',d)]
##Can
crs_utm20 <- 32620
ou = list()

for(i in 1:length(dd)){
  h = read.table(dd[i],header=T)
  print(dd[i])
  h$Date = as.Date(as.character(h$DateYYYYMMDD), format='%Y%m%d')
      if(i==1){
       hdim =dim(h)
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h1 = aggregate(BottomTemp~Q+Longitude+Latitude,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h1)[4]=paste("BT",unique(year(h$Date)),sep=".")
        out = h1
        h1 = st_as_sf(h1,coords =c('Longitude',"Latitude"),crs=4326)
        h1 = h1 %>% st_transform(32620)
        st_geometry(h1) = st_geometry(h1)/1000
        st_crs(h1) = 32620
          rm(h)
      } else {
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h2 = aggregate(BottomTemp~Q+Longitude+Latitude,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h2)[4]= y
        o = list()
        for(j in 1:4){
        h4 = subset(h2,Q==j)
        h5 = subset(h1,Q==j)
        h3 = st_as_sf(h4,coords =c('Longitude',"Latitude"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        st_geometry(h3) = st_geometry(h3)/1000
        st_crs(h3) = 32620
        ou = st_nearest_feature(h5,h3)
        o[[j]] = h4[ou,]
        }
        h2 = as.data.frame(do.call(rbind,o))
        out = merge(out,h2)
        rm(h3)
       }
}

  
#current Climatology

#current Climatology
gcur = c(grep('200',names(out)),grep('201',names(out)))
out$CurClim0.01  = combineClims(out,gcur,1)
out$CurClim0.025 = combineClims(out,gcur,2)
out$CurClim0.25  = combineClims(out,gcur,3)
out$CurClim0.5   = combineClims(out,gcur,4)
out$CurClim0.75  = combineClims(out,gcur,5)
out$CurClim0.975 = combineClims(out,gcur,6)
out$CurClim0.99  = combineClims(out,gcur,7)

#30sClim
gcur = c(grep('203',names(out)))

out$ThirClim0.01  = combineClims(out,gcur,1)
out$ThirClim0.025 = combineClims(out,gcur,2)
out$ThirClim0.25  = combineClims(out,gcur,3)
out$ThirClim0.5   = combineClims(out,gcur,4)
out$ThirClim0.75  = combineClims(out,gcur,5)
out$ThirClim0.975 = combineClims(out,gcur,6)
out$ThirClim0.99  = combineClims(out,gcur,7)

#50sClim
gcur = c(grep('205',names(out)))

out$FiftClim0.01  = combineClims(out,gcur,1)
out$FiftClim0.025 = combineClims(out,gcur,2)
out$FiftClim0.25  = combineClims(out,gcur,3)
out$FiftClim0.5   = combineClims(out,gcur,4)
out$FiftClim0.75  = combineClims(out,gcur,5)
out$FiftClim0.975 = combineClims(out,gcur,6)
out$FiftClim0.99  = combineClims(out,gcur,7)

#90sClim
gcur = c(grep('209',names(out)))
out$NineClim0.01  = combineClims(out,gcur,1)
out$NineClim0.025 = combineClims(out,gcur,2)
out$NineClim0.25  = combineClims(out,gcur,3)
out$NineClim0.5   = combineClims(out,gcur,4)
out$NineClim0.75  = combineClims(out,gcur,5)
out$NineClim0.975 = combineClims(out,gcur,6)
out$NineClim0.99  = combineClims(out,gcur,7)

##diffs from Climate

out$Diff30 = out$ThirClim0.5 - out$CurClim0.5
out$Diff50 = out$FiftClim0.5 - out$CurClim0.5
out$Diff90 = out$NineClim0.5 - out$CurClim0.5


out3 = out %>% st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% st_transform(32620)
      st_geometry(out3) = st_geometry(out3)/1000
      st_crs(out3) = 32620


#adding bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

         ss = st_nearest_feature(out3,ba)
         summary(st_distance(out3,ba[ss,],by_element=T)) #any weird dists?
         st_geometry(ba) = NULL
         out3$z = ba$z[ss]
         
ggplot(out3) +
  geom_sf(aes(fill=CurClim0.5,color=CurClim0.5)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('CanClimatologies2000-2019byQ.png') 

saveRDS(list(out,out3),file='CanClimatologiesandDiffsbyQ.rds')

#Calculating Diffs (generate climatologies for time blocks and subtract)

###############################################################
###########################################################################
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
##MPI
dd = d[grep('MPI',d)]
crs_utm20 <- 32620
ou = list()

for(i in 1:length(dd)){
  h = read.table(dd[i],header=T)
  print(dd[i])
  h$Date = as.Date(as.character(h$DateYYYYMMDD), format='%Y%m%d')
      if(i==1){
       hdim =dim(h)
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h1 = aggregate(BottomTemp~Q+Longitude+Latitude,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h1)[4]=paste("BT",unique(year(h$Date)),sep=".")
        out = h1
        h1 = st_as_sf(h1,coords =c('Longitude',"Latitude"),crs=4326)
        h1 = h1 %>% st_transform(32620)
        st_geometry(h1) = st_geometry(h1)/1000
        st_crs(h1) = 32620
          rm(h)
      } else {
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Q = ifelse(month(h$Date) %in% c(10,11,12),1,ifelse(month(h$Date) %in% c(1,2,3),2,ifelse(month(h$Date) %in% c(4,5,6),3,4)))
        h2 = aggregate(BottomTemp~Q+Longitude+Latitude,data=h,FUN=function(x) quantile(x,c(0.01,0.025,0.25,.5,.75,0.975,0.99)))
        names(h2)[4]= y
        o = list()
        for(j in 1:4){
        h4 = subset(h2,Q==j)
        h5 = subset(h1,Q==j)
        h3 = st_as_sf(h4,coords =c('Longitude',"Latitude"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        st_geometry(h3) = st_geometry(h3)/1000
        st_crs(h3) = 32620
        ou = st_nearest_feature(h5,h3)
        o[[j]] = h4[ou,]
        }
        h2 = as.data.frame(do.call(rbind,o))
        out = merge(out,h2)
        rm(h3)
       }
}


  
#current Climatology

#current Climatology
gcur = c(grep('200',names(out)),grep('201',names(out)))
out$CurClim0.01  = combineClims(out,gcur,1)
out$CurClim0.025 = combineClims(out,gcur,2)
out$CurClim0.25  = combineClims(out,gcur,3)
out$CurClim0.5   = combineClims(out,gcur,4)
out$CurClim0.75  = combineClims(out,gcur,5)
out$CurClim0.975 = combineClims(out,gcur,6)
out$CurClim0.99  = combineClims(out,gcur,7)

#30sClim
gcur = c(grep('203',names(out)))

out$ThirClim0.01  = combineClims(out,gcur,1)
out$ThirClim0.025 = combineClims(out,gcur,2)
out$ThirClim0.25  = combineClims(out,gcur,3)
out$ThirClim0.5   = combineClims(out,gcur,4)
out$ThirClim0.75  = combineClims(out,gcur,5)
out$ThirClim0.975 = combineClims(out,gcur,6)
out$ThirClim0.99  = combineClims(out,gcur,7)

#50sClim
gcur = c(grep('205',names(out)))

out$FiftClim0.01  = combineClims(out,gcur,1)
out$FiftClim0.025 = combineClims(out,gcur,2)
out$FiftClim0.25  = combineClims(out,gcur,3)
out$FiftClim0.5   = combineClims(out,gcur,4)
out$FiftClim0.75  = combineClims(out,gcur,5)
out$FiftClim0.975 = combineClims(out,gcur,6)
out$FiftClim0.99  = combineClims(out,gcur,7)

#90sClim
gcur = c(grep('209',names(out)))
out$NineClim0.01  = combineClims(out,gcur,1)
out$NineClim0.025 = combineClims(out,gcur,2)
out$NineClim0.25  = combineClims(out,gcur,3)
out$NineClim0.5   = combineClims(out,gcur,4)
out$NineClim0.75  = combineClims(out,gcur,5)
out$NineClim0.975 = combineClims(out,gcur,6)
out$NineClim0.99  = combineClims(out,gcur,7)

##diffs from Climate

out$Diff30 = out$ThirClim0.5 - out$CurClim0.5
out$Diff50 = out$FiftClim0.5 - out$CurClim0.5
out$Diff90 = out$NineClim0.5 - out$CurClim0.5

  

out4 = out %>% st_as_sf(coords=c('Longitude','Latitude'),crs=4326) %>% st_transform(32620)
      st_geometry(out4) = st_geometry(out4)/1000
      st_crs(out4) = 32620


#adding bathy
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

         ss = st_nearest_feature(out4,ba)
         summary(st_distance(out4,ba[ss,],by_element=T)) #any weird dists?
         st_geometry(ba) = NULL
         out4$z = ba$z[ss]
         
ggplot(out4) +
  geom_sf(aes(fill=CurClim0.5,color=CurClim0.5)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~Q) +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()
savePlot('MPIClimatologies2000-2019byQ.png') 

saveRDS(list(out,out4),file='MPIClimatologiesandDiffsbyQ.rds')

###########################################################################
##bnam
de = file.path(project.datadirectory('bio.lobster'),"Temperature Data",'BNAM')
o = bnamR(redo=F,bnam_location=file.path(de,'dTbtm_dSbtm_F_R85_2046-2065.mat'),
  outfile=file.path(de,'BNAM8.5proj2055.rds'),projections=T,yr=2055,standard=F)
o = as.data.frame(cbind(o$locsP,o$bTs))
o = subset(o,X>-70 & X< -55 & Y>40 & Y < 48)
o$Q1 = apply(o[,c('11','12','13')],1,mean)
o$Q2 = apply(o[,c('2','3','4')],1,mean)
o$Q3 = apply(o[,c('5','6','7')],1,mean)
o$Q4 = apply(o[,c('8','9')],1,mean)
o = subset(o,select=c(X,Y,Q1,Q2,Q3,Q4))
o = o%>% tidyr::pivot_longer(cols=c('Q1','Q2','Q3','Q4'),names_to='Q',values_to='bnam8.5.55')
s = st_as_sf(o,coords=c('X','Y'),crs=4326)
s = s %>% st_transform(32620)
st_geometry(s) = st_geometry(s)/1000
st_crs(s) = 32620

o = bnamR(redo=F,bnam_location=file.path(de,'dTbtm_dSbtm_F_R45_2046-2065.mat'),
  outfile=file.path(de,'BNAM4.5proj2055.rds'),projections=T,yr=2055,standard=F)
o = as.data.frame(cbind(o$locsP,o$bTs))
o = subset(o,X>-70 & X< -55 & Y>40 & Y < 48)
o$Q1 = apply(o[,c('11','12','13')],1,mean)
o$Q2 = apply(o[,c('2','3','4')],1,mean)
o$Q3 = apply(o[,c('5','6','7')],1,mean)
o$Q4 = apply(o[,c('8','9')],1,mean)
o = subset(o,select=c(X,Y,Q1,Q2,Q3,Q4))

o = o%>% tidyr::pivot_longer(cols=c('Q1','Q2','Q3','Q4'),names_to='Q',values_to='bnam4.5.55')

s1 = st_as_sf(o,coords=c('X','Y'),crs=4326)
s1 = s1 %>% st_transform(32620)
st_geometry(s1) = st_geometry(s1)/1000
st_crs(s1) = 32620

o = bnamR(redo=F,bnam_location=file.path(de,'dTbtm_dSbtm_F_R85_2066-2085.mat'),
  outfile=file.path(de,'BNAM8.5proj2075.rds'),projections=T,yr=2055,standard=F)
o = as.data.frame(cbind(o$locsP,o$bTs))
o = subset(o,X>-70 & X< -55 & Y>40 & Y < 48)
o$Q1 = apply(o[,c('11','12','13')],1,mean)
o$Q2 = apply(o[,c('2','3','4')],1,mean)
o$Q3 = apply(o[,c('5','6','7')],1,mean)
o$Q4 = apply(o[,c('8','9')],1,mean)
o = subset(o,select=c(X,Y,Q1,Q2,Q3,Q4))

o = o%>% tidyr::pivot_longer(cols=c('Q1','Q2','Q3','Q4'),names_to='Q',values_to='bnam8.5.75')

s2 = st_as_sf(o,coords=c('X','Y'),crs=4326)
s2 = s2 %>% st_transform(32620)
st_geometry(s2) = st_geometry(s2)/1000
st_crs(s2) = 32620



o = bnamR(redo=F,bnam_location=file.path(de,'dTbtm_dSbtm_F_R45_2066-2085.mat'),
  outfile=file.path(de,'BNAM4.5proj2075.rds'),projections=T,yr=2055,standard=F)
o = as.data.frame(cbind(o$locsP,o$bTs))
o = subset(o,X>-70 & X< -55 & Y>40 & Y < 48)
o$Q1 = apply(o[,c('11','12','13')],1,mean)
o$Q2 = apply(o[,c('2','3','4')],1,mean)
o$Q3 = apply(o[,c('5','6','7')],1,mean)
o$Q4 = apply(o[,c('8','9')],1,mean)
o = subset(o,select=c(X,Y,Q1,Q2,Q3,Q4))

o = o%>% tidyr::pivot_longer(cols=c('Q1','Q2','Q3','Q4'),names_to='Q',values_to='bnam4.5.75')

s3 = st_as_sf(o,coords=c('X','Y'),crs=4326)
s3 = s3 %>% st_transform(32620)
st_geometry(s3) = st_geometry(s3)/1000
st_crs(s3) = 32620

bnam = s
bnam$bnam4.5.55 = s1$bnam4.5.55
bnam$bnam4.5.75 = s3$bnam4.5.75
bnam$bnam8.5.75 = s2$bnam8.5.75

saveRDS(bnam,file='BNAMClimatologiesandDiffsbyQ.rds')


#############################################################################
#compile to one data frame
mpi = st_as_sf(readRDS(file='MPIClimatologiesandDiffsbyQ.rds')[[2]])
can = st_as_sf(readRDS(file='CanClimatologiesandDiffsbyQ.rds')[[2]])
had = st_as_sf(readRDS(file='HadClimatologiesDiffsbyQ.rds')[[2]])
glo = st_as_sf(readRDS(file='GlorysClimatologies2000-2019byQ.rds')[[2]])
bnam = st_as_sf(as.data.frame(readRDS(file='BNAMClimatologiesandDiffsbyQ.rds')))

q=1:4
gll = list()
for(i in 1:length(q)){
    gl = subset(glo,Q==q[i])
    m = subset(mpi,Q==q[i])
         mp = st_nearest_feature(gl,m)
         st_geometry(m) <- NULL
   gl$MPI30 = m[mp,'Diff30']        
   gl$MPI50 = m[mp,'Diff50']        
   gl$MPI90 = m[mp,'Diff90']        
   gll[[i]] = gl
  }

  glo1 = do.call(rbind,gll)


gll = list()
for(i in 1:length(q)){
    gl = subset(glo1,Q==q[i])
    ca = subset(can,Q==q[i])
      mp = st_nearest_feature(gl,ca)
       st_geometry(ca) <- NULL
    gl$CAN30 = ca[mp,'Diff30']        
    gl$CAN50 = ca[mp,'Diff50']        
    gl$CAN90 = ca[mp,'Diff90']        
    gll[[i]] = gl
  }

  glo2 = do.call(rbind,gll)
  

gll = list()
for(i in 1:length(q)){
    gl = subset(glo2,Q==q[i])
    ha = subset(had,Q==q[i])

         mp = st_nearest_feature(gl,ha)
         st_geometry(ha) <- NULL
   gl$HAD30 = ha[mp,'Diff30']        
   gl$HAD50 = ha[mp,'Diff50']        
   gl$HAD90 = ha[mp,'Diff90']        
    gll[[i]] = gl
  }

  glo3 = do.call(rbind,gll)

gll = list()
for(i in 1:length(q)){
    gl = subset(glo3,Q==q[i])
    ba = subset(bnam,Q==q[i])
        mp = st_nearest_feature(gl,ba)
  st_geometry(ba) <- NULL
  gl$BNAM4.5.55 = ba[mp,'bnam4.5.55']        
  gl$BNAM8.5.55 = ba[mp,'bnam8.5.55']        
  gl$BNAM4.5.75 = ba[mp,'bnam4.5.75']        
  gl$BNAM8.5.75 = ba[mp,'bnam8.5.75']        
  gll[[i]] = gl
}

 glo4 = do.call(rbind,gll)

 saveRDS(glo4,file='ClimatologyAndProjections.rds')
 
 
 #read 
 glo4 = readRDS(file='ClimatologyAndProjections.rds')
 g = st_as_sf(glo4)
 
 ggplot(data=g) + 
   geom_sf(aes(fill=HAD90,color=HAD90),size=1.7) + 
   scale_fill_viridis_c(limits=c(-.5,10)) +
   scale_color_viridis_c(limits=c(-.5,10)) +
   facet_wrap(~Q) +
   #  geom_sf(data=rL,size=1,colour='black',fill=NA)+
   theme( axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank()
   ) +
   coord_sf()
 