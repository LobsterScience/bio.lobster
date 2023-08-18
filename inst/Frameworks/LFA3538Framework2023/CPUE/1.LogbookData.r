#catch rate analyses 

require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)
require(sf)
require(ggplot)
fd = file.path(project.datadirectory('bio.lobster'),'analysis','CPUE')
dir.create(fd); setwd(fd)

a = lobster.db('process.logs.unfiltered')

a = subset(a,LFA %in% c(34,35,36,38))
a = as_tibble(a)


#dealing with glorys and grids
gr = readRDS('C:/Users/Cooka/Documents/git/bio.lobster.data/mapping_data/GridPolyLand.rds')
  st_crs(gr) <- 4326
gr = st_transform(gr,32620) 
st_geometry(gr) <- st_geometry(st_as_sf(gr$geometry/1000)) 
st_crs(gr) <- 32620
  
te = readRDS(file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','GlorysClimatologies1993-2022byDOYWithLFA.rds'))
te = st_as_sf(te)

gt = st_join(te,gr,join=st_within)
gt = subset(gt,!is.na(grid))
gt$geometry <- NULL
te = as_tibble(gt)

#temp
tea = te %>% group_by(doy,LFA,grid) %>% summarize(across(starts_with('BT'),mean, .names = "{.col}")) %>% tidyr::pivot_longer(cols=starts_with('BT'),values_to='temp')
teasd = te %>% group_by(doy,LFA,grid) %>% summarize(across(starts_with('BT'),sd, .names = "{.col}")) %>% tidyr::pivot_longer(cols=starts_with('BT'),values_to='tempsd')
teamin = te %>% group_by(doy,LFA,grid) %>% summarize(across(starts_with('BT'),min, .names = "{.col}")) %>% tidyr::pivot_longer(cols=starts_with('BT'),values_to='tempmin')
teamax = te %>% group_by(doy,LFA,grid) %>% summarize(across(starts_with('BT'),max, .names = "{.col}")) %>% tidyr::pivot_longer(cols=starts_with('BT'),values_to='tempmax')

#depth
zmean =  aggregate(z~grid,data=te,FUN=mean)
names(zmean)[2] = 'zmean' 
zsd =  aggregate(z~grid,data=te,FUN=sd)
names(zsd)[2] = 'zsd' 
zmin =  aggregate(z~grid,data=te,FUN=min)
names(zmin)[2] = 'zmin' 
zmax =  aggregate(z~grid,data=te,FUN=max)
names(zmax)[2] = 'zmax' 
zz =list(zmean, zsd,zmin, zmax)
z = zz %>% purrr::reduce(full_join)
te_list= list(tea,teasd,teamin,teamax)
te1 = te_list %>% purrr::reduce(full_join)
te1$yr = as.numeric(substr(te1$name,4,7)  )
te1$dyear= te1$yr+te1$doy/365.25 - .5/365.25
te1$date = lubridate::date_decimal(te1$dyear)
te1$date = as.Date(te1$date,'%Y-%m-%d')
te1z = list(te1,z) %>% purrr::reduce(full_join)

aT = merge(a,te1,by.x=c('LFA','GRID_NUM','DATE_FISHED'),by.y=c('LFA','grid','date'),all.x=T)
aT = subset(aT, WEIGHT_LBS<10000)
aT = subset(aT,NUM_OF_TRAPS>0 )
aT = subset(aT,NUM_OF_TRAPS<2000 )
i = which(aT$LFA==38 & aT$SYEAR<2006)
j = which(aT$LFA==36 & aT$SYEAR<2006)
gt$L = paste(gt$LFA,gt$grid,sep="-")
l = unique(gt$L)
aT = aT[c(-i,-j),]
aT$L = paste(aT$LFA,aT$GRID_NUM,sep="-")
aT = subset(aT,L %in% l)

saveRDS(aT,'CPUETempDepth.rds')
##############
#characteristics
###
b = lobster.db('community_code')
d = lobster.db('vessels.by.port')
d = na.zero(d)
d1 = aggregate(cbind(GROSS_TONNAGE, BHP, LOA, BREADTH, DEPTH,YEAR_BUILT)~VR_NUMBER+LFA+YR_FISHED,data=d,FUN=min)
d = na.zero(d1,rev=T)
w = lobster.db('port')
v = lobster.db('port_location')

#Demographics on Lic
o = read.csv(file.path(project.datadirectory('bio.lobster'),'data','LicenceHolder','LicenceHolderInfo2022a.csv'))
i = grep('X',names(o))

o = subset(o,Years_Licence_Held<100)
o$LFA = do.call(rbind, strsplit(o$Desc_Eng," - "))[,2]
o$LicStartDate = as.Date(o$Licence_Participant_Start_Date,'%b %d, %Y')
o$BDate = as.Date(o$Birthdate,'%b %d, %Y')
o = subset(o,select=c(Licence_Id, Name_Last_First, BDate, LicStartDate, LFA, Years_Licence_Held, Age))
