##climate data changes

require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(ggplot2)
require(sf)
require(forecast)
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
##HAD
dd = d[grep('Had',d)]
crs_utm20 <- 32620
ou = list()
te = read.table(dd[1],header=T)
te = te %>% subset(Time==1) %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
te$ID = 1:nrow(te)
te = subset(te,select=c(geometry,ID))

for(i in 1:length(dd)){
  x1 = read.table(dd[i],header=T)
  x1$Date = as.Date(as.character(x1$DateYYYYMMDD), format='%Y%m%d')
  x1 = st_as_sf(x1,coords = c("Longitude",'Latitude'),crs=4326) %>% 
       st_transform(crs_utm20)
  ou[[i]] = st_join(x1,te,join=st_nearest_feature,left=TRUE)
}

cb = do.call(rbind,ou)


k = unique(cb$ID)
summ = data.frame(ID=NA,Trend1=NA,Mean1=NA,median1=NA, max1=NA, min1=NA,Trend2=NA, Mean2=NA,median2=NA, max2=NA, min2=NA, Trend3=NA,Mean3=NA,median3=NA, max3=NA, min3=NA,Trend4=NA,Mean4=NA,median4=NA, max4=NA, min4=NA,Trend5=NA,Mean5=NA,median5=NA, max5=NA, min5=NA)
for(i in 1:length(k)){
      z =  subset(cb,ID==k[i])
      z.0 = ts(as.data.frame(subset(z,Date>'2020-01-01' &Date<'2025-01-01',select=BottomTemp))[,1],frequency=25)
      z.1 = ts(as.data.frame(subset(z,Date>'2025-01-01' &Date<'2030-01-01',select=BottomTemp))[,1],frequency=25)
      z1 = ts(as.data.frame(subset(z,Date>'2031-01-01' &Date<'2036-01-01',select=BottomTemp))[,1],frequency=25)
      z2 = ts(as.data.frame(subset(z,Date>'2036-01-01' & Date <'2056-01-01',select=BottomTemp))[,1],frequency = 25)
      z3 = ts(as.data.frame(subset(z,Date>'2056-01-01',select=BottomTemp))[,1],frequency=25)
      summ[i,] = rep(NA,times=26)
      summ[i,1] = k[i]
      if(sum(z.0)>0){
      z1p = decompose(z.0,'multiplicative')
      tr = z1p$trend
      se = z1p$seasonal
      z1pp = forecast::tslm(z.0~tr+se) # linear trend
      summ[i,2:6] = c(z1pp$coefficients['tr'],mean(z.0,na.rm=T),median(z.0,na.rm=T),quantile(z.0,0.975,na.rm=T),quantile(z.0,0.025,na.rm=T))
      }
      if(sum(z.1)>0){
      z1p = decompose(z.1,'multiplicative')
      tr = z1p$trend
      se = z1p$seasonal
      z1pp = forecast::tslm(z.1~tr+se) # linear trend
      summ[i,7:11] = c(z1pp$coefficients['tr'],mean(z.1,na.rm=T),median(z.1,na.rm=T),quantile(z.1,0.975,na.rm=T),quantile(z.1,0.025,na.rm=T))
      }
      if(sum(z1)>0){
      z1p = decompose(z1,'multiplicative')
      tr = z1p$trend
      se = z1p$seasonal
      z1pp = forecast::tslm(z1~tr+se) # linear trend
      summ[i,12:16] = c(z1pp$coefficients['tr'],mean(z1,na.rm=T),median(z1,na.rm=T),quantile(z1,0.975,na.rm=T),quantile(z1,0.025,na.rm=T))
      
      }
      if(sum(z2)>0){
      z1p = decompose(z2,'multiplicative')
      tr = z1p$trend
      se = z1p$seasonal
      z1pp = forecast::tslm(z2~tr+se) # linear trend
      summ[i,17:21] = c(z1pp$coefficients['tr'],mean(z2,na.rm=T),median(z2,na.rm=T),quantile(z2,0.975,na.rm=T),quantile(z2,0.025,na.rm=T))
      }
      if(sum(z3)>0){
      z1p = decompose(z3,'multiplicative')
      tr = z1p$trend
      se = z1p$seasonal
      z1pp = forecast::tslm(z3~tr+se) # linear trend
      summ[i,22:26] = c(z1pp$coefficients['tr'],mean(z3,na.rm=T),median(z3,na.rm=T),quantile(z3,0.975,na.rm=T),quantile(z3,0.025,na.rm=T))
      }
      
}
getwd()
saveRDS(summ,'HadTempSummary.rds')

rm(summ)

##Can

dd = d[grep('Can',d)]
crs_utm20 <- 32620
ou = list()
te = read.table(dd[1],header=T)
te = te %>% subset(Time==1) %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
te$ID = 1:nrow(te)
te = subset(te,select=c(geometry,ID))

for(i in 1:length(dd)){
  x1 = read.table(dd[i],header=T)
  x1$Date = as.Date(as.character(x1$DateYYYYMMDD), format='%Y%m%d')
  x1 = st_as_sf(x1,coords = c("Longitude",'Latitude'),crs=4326) %>% 
    st_transform(crs_utm20)
  ou[[i]] = st_join(x1,te,join=st_nearest_feature,left=TRUE)
}

cb = do.call(rbind,ou)


k = unique(cb$ID)
summ = data.frame(ID=NA,Trend1=NA,Mean1=NA,median1=NA, max1=NA, min1=NA,Trend2=NA, Mean2=NA,median2=NA, max2=NA, min2=NA, Trend3=NA,Mean3=NA,median3=NA, max3=NA, min3=NA,Trend4=NA,Mean4=NA,median4=NA, max4=NA, min4=NA,Trend5=NA,Mean5=NA,median5=NA, max5=NA, min5=NA)
for(i in 1:length(k)){
  z =  subset(cb,ID==k[i])
  z.0 = ts(as.data.frame(subset(z,Date>'2020-01-01' &Date<'2025-01-01',select=BottomTemp))[,1],frequency=25)
  z.1 = ts(as.data.frame(subset(z,Date>'2025-01-01' &Date<'2030-01-01',select=BottomTemp))[,1],frequency=25)
  z1 = ts(as.data.frame(subset(z,Date>'2031-01-01' &Date<'2036-01-01',select=BottomTemp))[,1],frequency=25)
  z2 = ts(as.data.frame(subset(z,Date>'2036-01-01' & Date <'2056-01-01',select=BottomTemp))[,1],frequency = 25)
  z3 = ts(as.data.frame(subset(z,Date>'2056-01-01',select=BottomTemp))[,1],frequency=25)
  summ[i,] = rep(NA,times=26)
  summ[i,1] = k[i]
  if(sum(z.0)>0){
    z1p = decompose(z.0,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z.0~tr+se) # linear trend
    summ[i,2:6] = c(z1pp$coefficients['tr'],mean(z.0,na.rm=T),median(z.0,na.rm=T),quantile(z.0,0.975,na.rm=T),quantile(z.0,0.025,na.rm=T))
  }
  if(sum(z.1)>0){
    z1p = decompose(z.1,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z.1~tr+se) # linear trend
    summ[i,7:11] = c(z1pp$coefficients['tr'],mean(z.1,na.rm=T),median(z.1,na.rm=T),quantile(z.1,0.975,na.rm=T),quantile(z.1,0.025,na.rm=T))
  }
  if(sum(z1)>0){
    z1p = decompose(z1,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z1~tr+se) # linear trend
    summ[i,12:16] = c(z1pp$coefficients['tr'],mean(z1,na.rm=T),median(z1,na.rm=T),quantile(z1,0.975,na.rm=T),quantile(z1,0.025,na.rm=T))
    
  }
  if(sum(z2)>0){
    z1p = decompose(z2,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z2~tr+se) # linear trend
    summ[i,17:21] = c(z1pp$coefficients['tr'],mean(z2,na.rm=T),median(z2,na.rm=T),quantile(z2,0.975,na.rm=T),quantile(z2,0.025,na.rm=T))
  }
  if(sum(z3)>0){
    z1p = decompose(z3,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z3~tr+se) # linear trend
    summ[i,22:26] = c(z1pp$coefficients['tr'],mean(z3,na.rm=T),median(z3,na.rm=T),quantile(z3,0.975,na.rm=T),quantile(z3,0.025,na.rm=T))
  }
  
}
saveRDS(summ,'CanTempSummary.rds')
rm(summ)
##MPI

dd = d[grep('MPI',d)]
crs_utm20 <- 32620
ou = list()
te = read.table(dd[1],header=T)
te = te %>% subset(Time==1) %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
te$ID = 1:nrow(te)
te = subset(te,select=c(geometry,ID))

for(i in 1:length(dd)){
  x1 = read.table(dd[i],header=T)
  x1$Date = as.Date(as.character(x1$DateYYYYMMDD), format='%Y%m%d')
  x1 = st_as_sf(x1,coords = c("Longitude",'Latitude'),crs=4326) %>% 
    st_transform(crs_utm20)
  ou[[i]] = st_join(x1,te,join=st_nearest_feature,left=TRUE)
}

cb = do.call(rbind,ou)


k = unique(cb$ID)
summ = data.frame(ID=NA,Trend1=NA,Mean1=NA,median1=NA, max1=NA, min1=NA,Trend2=NA, Mean2=NA,median2=NA, max2=NA, min2=NA, Trend3=NA,Mean3=NA,median3=NA, max3=NA, min3=NA,Trend4=NA,Mean4=NA,median4=NA, max4=NA, min4=NA,Trend5=NA,Mean5=NA,median5=NA, max5=NA, min5=NA)
for(i in 1:length(k)){
  z =  subset(cb,ID==k[i])
  z.0 = ts(as.data.frame(subset(z,Date>'2020-01-01' &Date<'2025-01-01',select=BottomTemp))[,1],frequency=25)
  z.1 = ts(as.data.frame(subset(z,Date>'2025-01-01' &Date<'2030-01-01',select=BottomTemp))[,1],frequency=25)
  z1 = ts(as.data.frame(subset(z,Date>'2031-01-01' &Date<'2036-01-01',select=BottomTemp))[,1],frequency=25)
  z2 = ts(as.data.frame(subset(z,Date>'2036-01-01' & Date <'2056-01-01',select=BottomTemp))[,1],frequency = 25)
  z3 = ts(as.data.frame(subset(z,Date>'2056-01-01',select=BottomTemp))[,1],frequency=25)
  summ[i,] = rep(NA,times=26)
  summ[i,1] = k[i]
  if(sum(z.0)>0){
    z1p = decompose(z.0,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z.0~tr+se) # linear trend
    summ[i,2:6] = c(z1pp$coefficients['tr'],mean(z.0,na.rm=T),median(z.0,na.rm=T),quantile(z.0,0.975,na.rm=T),quantile(z.0,0.025,na.rm=T))
  }
  if(sum(z.1)>0){
    z1p = decompose(z.1,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z.1~tr+se) # linear trend
    summ[i,7:11] = c(z1pp$coefficients['tr'],mean(z.1,na.rm=T),median(z.1,na.rm=T),quantile(z.1,0.975,na.rm=T),quantile(z.1,0.025,na.rm=T))
  }
  if(sum(z1)>0){
    z1p = decompose(z1,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z1~tr+se) # linear trend
    summ[i,12:16] = c(z1pp$coefficients['tr'],mean(z1,na.rm=T),median(z1,na.rm=T),quantile(z1,0.975,na.rm=T),quantile(z1,0.025,na.rm=T))
    
  }
  if(sum(z2)>0){
    z1p = decompose(z2,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z2~tr+se) # linear trend
    summ[i,17:21] = c(z1pp$coefficients['tr'],mean(z2,na.rm=T),median(z2,na.rm=T),quantile(z2,0.975,na.rm=T),quantile(z2,0.025,na.rm=T))
  }
  if(sum(z3)>0){
    z1p = decompose(z3,'multiplicative')
    tr = z1p$trend
    se = z1p$seasonal
    z1pp = forecast::tslm(z3~tr+se) # linear trend
    summ[i,22:26] = c(z1pp$coefficients['tr'],mean(z3,na.rm=T),median(z3,na.rm=T),quantile(z3,0.975,na.rm=T),quantile(z3,0.025,na.rm=T))
  }
  
}
saveRDS(summ,'MPITempSummary.rds')

#Work With Summaries

require(ggpubr)

MPI = readRDS('MPITempSummary.rds')
Can = readRDS('CanTempSummary.rds')
Had = readRDS('HadTempSummary.rds')

#MPI

dd = d[grep('MPI',d)]
crs_utm20 <- 32620
ou = list()
te = read.table(dd[1],header=T)
te = te %>% subset(Time==1) %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
te$ID = 1:nrow(te)
te = subset(te,select=c(geometry,ID))
MPI = st_as_sf(merge(MPI,te))

# Can

dd = d[grep('Can',d)]
crs_utm20 <- 32620
ou = list()
te = read.table(dd[1],header=T)
te = te %>% subset(Time==1) %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
te$ID = 1:nrow(te)
te = subset(te,select=c(geometry,ID))
Can = st_as_sf(merge(Can,te))

# Had

dd = d[grep('Had',d)]
crs_utm20 <- 32620
ou = list()
te = read.table(dd[1],header=T)
te = te %>% subset(Time==1) %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
te$ID = 1:nrow(te)
te = subset(te,select=c(geometry,ID))
Had = st_as_sf(merge(Had,te))

mm = c(min(c(Can$Mean1,MPI$Mean1,Had$Mean1),na.rm=T),max(c(Can$Mean1,MPI$Mean1,Had$Mean1),na.rm=T))

##Maps
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
##base models
baseCan = ggplot(Can) + 
  geom_sf(aes(fill=Mean1,color=Mean1))+ 
  ggtitle('Can')+
  scale_fill_viridis_c(name='Base',limits=mm) +
  scale_color_viridis_c(name='Base',limits=mm) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

  
ll = get_legend(baseCan)

baseCan = baseCan + theme(legend.position = 'none')
baseMPI = ggplot(MPI) + 
  geom_sf(aes(fill=Mean1,color=Mean1))+ 
  scale_fill_viridis_c(name='MPI',limits=mm) +
  scale_color_viridis_c(name='MPI',limits=mm) +
  ggtitle('MPI')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       axis.text.y=element_blank(),
                                       axis.ticks.y=element_blank())


baseHad = ggplot(Had) + 
  geom_sf(aes(fill=Mean1,color=Mean1))+ 
  scale_fill_viridis_c(name='Had',limits=mm) +
  scale_color_viridis_c(name='Had',limits=mm) +
  ggtitle('Had')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


### 10 yr diffs
##base models
Can$D1 = Can$Mean3-Can$Mean1
MPI$D1 = MPI$Mean3-MPI$Mean1
Had$D1 = Had$Mean3-Had$Mean1

###30 yr
Can$D2 = Can$Mean4-Can$Mean1
MPI$D2 = MPI$Mean4-MPI$Mean1
Had$D2 = Had$Mean4-Had$Mean1

#70 year
Can$D3 = Can$Mean5-Can$Mean1
MPI$D3 = MPI$Mean5-MPI$Mean1
Had$D3 = Had$Mean5-Had$Mean1


mm = c(min(c(Can$D1,MPI$D1,Had$D1),na.rm=T),max(c(Can$D3,MPI$D3,Had$D3),na.rm=T))






Can1 = ggplot(Can) + 
  geom_sf(aes(fill=D1,color=D1))+ 
#  ggtitle('Can')+
  scale_fill_viridis_c(name='10y',limits=mm) +
  scale_color_viridis_c(name='10y',limits=mm) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ll2 = get_legend(Can1)

Can1 = Can1 + theme(legend.position = 'none')

MPI1 = ggplot(MPI) + 
  geom_sf(aes(fill=D1,color=D1))+ 
  scale_fill_viridis_c(name='MPI',limits=mm) +
  scale_color_viridis_c(name='MPI',limits=mm) +
  #ggtitle('MPI')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


Had1 = ggplot(Had) + 
  geom_sf(aes(fill=D1,color=D1))+ 
  scale_fill_viridis_c(name='Had',limits=mm) +
  scale_color_viridis_c(name='Had',limits=mm) +
  #ggtitle('Had')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


###30yr


Can2 = ggplot(Can) + 
  geom_sf(aes(fill=D2,color=D2))+ 
  #  ggtitle('Can')+
  scale_fill_viridis_c(name='30y',limits=mm) +
  scale_color_viridis_c(name='30y',limits=mm) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ll3 = get_legend(Can2)

Can2 = Can2 + theme(legend.position = 'none')

MPI2 = ggplot(MPI) + 
  geom_sf(aes(fill=D2,color=D2))+ 
  scale_fill_viridis_c(name='MPI',limits=mm) +
  scale_color_viridis_c(name='MPI',limits=mm) +
  #ggtitle('MPI')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


Had2 = ggplot(Had) + 
  geom_sf(aes(fill=D2,color=D2))+ 
  scale_fill_viridis_c(name='Had',limits=mm) +
  scale_color_viridis_c(name='Had',limits=mm) +
  #ggtitle('Had')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


#70 year

Can3 = ggplot(Can) + 
  geom_sf(aes(fill=D3,color=D3))+ 
  #  ggtitle('Can')+
  scale_fill_viridis_c(name='70y',limits=mm) +
  scale_color_viridis_c(name='70y',limits=mm) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ll4 = get_legend(Can3)

Can3 = Can3 + theme(legend.position = 'none')

MPI3 = ggplot(MPI) + 
  geom_sf(aes(fill=D3,color=D3))+ 
  scale_fill_viridis_c(name='MPI',limits=mm) +
  scale_color_viridis_c(name='MPI',limits=mm) +
  #ggtitle('MPI')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


Had3 = ggplot(Had) + 
  geom_sf(aes(fill=D3,color=D3))+ 
  scale_fill_viridis_c(name='Had',limits=mm) +
  scale_color_viridis_c(name='Had',limits=mm) +
  #ggtitle('Had')+
  theme(legend.position = 'none',axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

require(gridExtra)


grid.arrange(baseCan,baseMPI,baseHad,ll,
             Can1,MPI1,Had1,ll2,
             Can2,MPI2,Had2,ll3,
             Can3,MPI3,Had3,ll4,
             ncol=4,nrow=4,widths=c(2.5,2.5,2.5,.6))
