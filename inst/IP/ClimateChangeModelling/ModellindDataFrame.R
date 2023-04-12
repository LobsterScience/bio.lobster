
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

#start with the glorys data frame 
survey = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
survey = st_as_sf(survey)
survey$Time = round(survey$DYEAR*25)

#envt data
d = dir(file.path(bio.datadirectory,'bio.lobster','Temperature Data/QuinnBT-2022/'),full.names = T)
dd = d[grep('Can',d)]
ee = d[grep('Had',d)]
gg = d[grep('MPI',d)]


s = bnamR(redo=F)
loc = st_as_sf(s[[1]],coords=c('X','Y'),crs=4326)
loc = st_transform(loc,32620)
st_geometry(loc) = st_geometry(loc)/1000
st_crs(loc) = 32620
bt = s[[2]][,-1]
t = s[[3]]


crs_utm20 <- 32620

	#yy = unique(survey$YEAR) 
	oo = list()
	m = 0
 yy = 2000:2022
for(i in 1:length(yy)){
		k = subset(survey,YEAR==yy[i])
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
print(yy[i])

		#bnam
			l = grep(as.character(yy[i]),t)

			ll = unique(k$Date)

		ll = unique(k$Time)
		if(any(ll==0)) ll = ll[ll>0]
	for(l in 1:length(ll)){
		m = m+1
			kk = subset(k,Time ==ll[l])
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

				#bnam
				kk$bnamt =NA
				kk$distBN=NA
if(length(l)>0){
			j = st_nearest_feature(kk,loc)
			tt = ceiling(ll[l]*25/50)
			tt = ifelse(tt==13,12,tt)
			kk$bnamt = bt[j,l[tt]]
			kk$distBN = as.numeric(st_distance(kk,loc[j,],by_element=T))
    		 } 
       	 oo[[m]] = kk
		}
	}

da = do.call(rbind,oo)
da = st_as_sf(da)
da = subset(da,CanDist<5)

saveRDS(da,file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModelAllTemps.rds'))
