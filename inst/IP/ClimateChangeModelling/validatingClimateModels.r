#validating current data from models

require(bio.lobster)
require(bio.utilites)
require(sf)
require(lubridate)
require(dplyr)

setwd(file.path(bio.datadirectory,'bio.lobster','analysis','ClimateModelling'))
	g = lobster.db('temperature.data')
	g$yr = year(g$T_DATE)
	g$W = ceiling(yday(g$T_DATE)/366*25)
	g = aggregate(TEMP~T_UID+LON_DD+LAT_DD+W+yr,data=g,FUN=median)
	g = g %>% st_as_sf(coords=c('LON_DD','LAT_DD'),crs=4326) %>% st_transform(32620)
	g$yr = year(g$T_DATE)
	g$W = ceiling(yday(g$T_DATE)/366*25)
	st_geometry(g) = st_geometry(g)/1000
	st_crs(g) = 32620
	d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
	dd = d[grep('Can',d)]
	ee = d[grep('Had',d)]
	gg = d[grep('MPI',d)]

yy = 2016:2022
oo = list()
m=0
for(i in 1:length(yy)){
		k = subset(g,yr == yy[i])
		ddd = dd[grep(paste(yy[i],"_",sep=""),dd)]
		te = read.table(ddd,header=T)
		te = te %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
		st_geometry(te) = st_geometry(te)/1000
		st_crs(te) = 32620

		eee = ee[grep(paste(yy[i],"_",sep=""),ee)]
		fe = read.table(eee,header=T)
		fe = fe %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
		st_geometry(fe) = st_geometry(fe)/1000
		st_crs(fe) = 32620

		ggg = gg[grep(paste(yy[i],"_",sep=""),gg)]
		de = read.table(ggg,header=T)
		de = de %>% st_as_sf(coords = c("Longitude",'Latitude'),crs=4326) %>% st_transform(crs_utm20)
		st_geometry(de) = st_geometry(de)/1000
		st_crs(de) = 32620

		ll = unique(k$W)	
		for(l in 1:length(ll)){
			m = m+1
			kk = subset(k,W ==ll[l])
			tt = subset(te,Time==ll[l],select=c(Depth_m,BottomTemp))
			tt = bio.utilities::rename.df(tt,c('Depth_m','BottomTemp'),c('CanZ','CanBT'))
     		
     		ff = subset(fe,Time==ll[l],select=c(Depth_m,BottomTemp))
			ff = bio.utilities::rename.df(ff,c('Depth_m','BottomTemp'),c('HadZ','HadBT'))

    		pp = subset(de,Time==ll[l],select=c(Depth_m,BottomTemp))
			pp = bio.utilities::rename.df(pp,c('Depth_m','BottomTemp'),c('MPIZ','MPIBT'))
  		
       	  	 ou = st_nearest_feature(kk,tt)
       	 ds = st_distance(kk,tt[ou,],by_element=T)
       	 st_geometry(tt) = NULL
       	 kk$CanZ = tt$CanZ[ou]
       	 kk$CanBT = tt$CanBT[ou]
       	 kk$CanDist = as.numeric(ds)
       	 
       	 ou = st_nearest_feature(kk,ff)
       	 st_geometry(ff) = NULL
       	 kk$HadZ = ff$HadZ[ou]
       	 kk$HadBT = ff$HadBT[ou]
       	 
       	 ou = st_nearest_feature(kk,pp)
       	 st_geometry(pp) = NULL
       	 kk$MPIZ = pp$MPIZ[ou]
       	 kk$MPIBT = pp$MPIBT[ou]
       	 
       	 oo[[m]] = kk
		}
	}

xx = st_as_sf(do.call(rbind,oo))
xx = subset(xx,CanDist<quantile(CanDist,0.90))
xx$Haddiff = xx$HadBT - xx$TEMP
xx$Candiff = xx$CanBT - xx$TEMP 
xx$MPIdiff = xx$MPIBT - xx$TEMP 

ggplot(subset(xx,yr==2016)) + 
  geom_sf(aes(fill=Haddiff,color=Haddiff)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~W) +
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()


###glorys
 
s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k= k[23:27]

ol = list()
m=0
for(i in 1:length(k)){
			h = readRDS(k[i])
			h$Date = as.Date(h$Date)
			y = unique(year(h$Date))
			h$W  = ceiling(yday(h$Date)/366*25)
			h = aggregate(bottomT~W+X+Y,data=h,FUN=median)
			h = st_as_sf(h,coords =c('X',"Y"),crs=4326)
			h = h %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
			st_geometry(h) = st_geometry(h)/1000
			st_crs(h) = 32620

			x1 = subset(xx,yr == y)
			uW = unique(x1$W)
					for(j in 1:length(uW)){
				m=m+1
			
						nn = subset(x1,W==uW[j])
						hh = subset(h,W==uW[j])
						ou = st_nearest_feature(nn,hh)
       	 ds = st_distance(nn,hh[ou,],by_element=T)
       	 st_geometry(hh) = NULL
       	 nn$GlT = hh$bottomT[ou]
       	 nn$GlD = as.numeric(ds)
			ol[[m]] = nn       	

			}
}

xx = st_as_sf(do.call(rbind,ol))

xx=subset(xx,GlD<quantile(GlD,0.9,na.rm=T))

xx$Gldiff = xx$GlT - xx$TEMP

ggplot(xx,aes(Haddiff)) + geom_density(aes(y=..density..))+ geom_density(data=aes(y=..density..))

ggplot(xx,aes(Candiff)) + geom_histogram(aes(y=..density..)) + facet_wrap(~yr)	

ggplot(xx,aes(MPIdiff)) + geom_histogram(aes(y=..density..)) + facet_wrap(~yr)	

ggplot(xx,aes(Gldiff)) + geom_histogram(aes(y=..density..)) + facet_wrap(~yr)	


aggregate(cbind(Haddiff,Candiff,MPIdiff,Gldiff)~yr,data=xx,FUN=summary)

saveRDS(xx,file='ModelsToObserved.rds')
xx = readRDS('ModelsToObserved.rds')

##bnam
s = bnamR(redo=F)
loc = st_as_sf(s[[1]],coords=c('X','Y'),crs=4326)
loc = st_transform(loc,32620)
st_geometry(loc) = st_geometry(loc)/1000
st_crs(loc) = 32620
yy = 2016:2018
bt = s[[2]][,-1]
t = s[[3]]
out = list()
m=0
for(i in 1:length(yy)){
	w = subset(xx,yr==yy[i])
	l = grep(as.character(yy[i]),t)
	kl = unique(w$W)
	for(n in 1:length(kl)){
		m=m+1
			ww = subset(w,W==kl[n])
			j = st_nearest_feature(ww,loc)
			tt = ceiling(kl[n]*25/50)
			tt = ifelse(tt==13,12,tt)
			ww$bnamt = bt[j,l[tt]]
			ww$distBN = as.numeric(st_distance(ww,loc[j,],by_element=T))
    	out[[m]] = ww  
		}
	}

xx=do.call(rbind,out)
xx = subset(xx,distBN<4)
xx$bnamdiff = xx$bnamt - xx$TEMP



 mad(xx$Haddiff)
 1.605878
 mad(xx$Candiff)
 1.711642
 mad(xx$MPIdiff)
 2.043772
 mad(xx$bnamdiff)
 2.293498
 mad(xx$Gldiff)
 0.9342395

plot(density(xx$Candiff),xlab='Model - Measured',main="",xlim=c(-8,8),lwd=2)
 lines(density(xx$Haddiff),col='red')
	lines(density(xx$MPIdiff),col='blue')
	lines(density(xx$bnamdiff),col='orange',lwd=2)
	lines(density(xx$Gldiff),col='purple',lwd=2)	
	abline(v=0,lwd=2,lty=3)
	legend('topleft',c('Can','Had','MPI','BNAM','Glorys'),lty=c(1,1,1,1,1),col=c('black','red','blue','orange','purple'),bty='n')

savePlot('Temp2Modelcomparisons.png')
xx$Z = round(xx$CanZ/5)*5
xs = aggregate(cbind(Candiff,MPIdiff,Haddiff,bnamdiff)~Z,data=xx,FUN=median)

with(xs,{
	plot(Z,Candiff,type='l',col='black',ylab='Diff from Observed Temp')
	lines(Z,MPIdiff,type='l',col='blue')
	lines(Z,Haddiff,type='l',col='red')
	lines(Z,bnamdiff,type='l',col='orange')
	
	})
	abline(h=0,lwd=2)
	legend('topright',c('Can','MPI','Had','BNAM'),lty=c(1,1,1,1),col=c('black','blue','red','orange'),bty='n')

savePlot('Temp2ModelcomparisonsbyDepth.png')
