#catch rate analyses 

require(bio.lobster)
require(bio.utilities)
require(dplyr)
require(devtools)
require(sf)
require(ggplot)

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

##############
#characteristics
###
b = lobster.db('community_code')
d = lobster.db('vessels.by.port')
d = na.zero(d)
d1 = aggregate(cbind(GROSS_TONNAGE, BHP, LOA, BREADTH, DEPTH,YEAR_BUILT)~VR_NUMBER+LFA+YR_FISHED,data=d,FUN=min)
d = na.zero(d1,rev=T)
w = lobster.db('port')
v = lobster.db('port_location.redo')

#Demographics on Lic
o = read.csv(file.path(project.datadirectory('bio.lobster'),'data','LicenceHolder','LicenceHolderInfo2022a.csv'))
i = grep('X',names(o))

o = subset(o,Years_Licence_Held<100)
o$LFA = do.call(rbind, strsplit(o$Desc_Eng," - "))[,2]
o$LicStartDate = as.Date(o$Licence_Participant_Start_Date,'%b %d, %Y')
o$BDate = as.Date(o$Birthdate,'%b %d, %Y')
o = subset(o,select=c(Licence_Id, Name_Last_First, BDate, LicStartDate, LFA, Years_Licence_Held, Age))
###########################
#start with an overview of all fishing year by year


















##by licence

aL = a
#aL = subset(aL,GRID_NUM>0 & !is.na(temp))

##LFA 38
a8 = subset(aL, LFA==38)
m = which(is.na(a8$GRID_NUM))
a8$GRID_NUM[m] = -99
a8 = merge(a8,v,by.x=c('LFA','COMMUNITY_CODE'),by.y=c('LFA','PORT_CODE'))
os = subset(o,LFA==38)
id = unique(a8$LICENCE_ID)
sa8 = split(a8,f=a8$LICENCE_ID)
out = list()
y=0
for(i in 1:length(sa8)){
    j = sa8[[i]]
    j = j[order(j$DATE_FISHED),]
    l = unique(j$SYEAR)
          outa= list()
          y=y+1
          for(oo in 1:length(l)){
              vv = subset(j,SYEAR==l[oo])
              va = tryCatch( aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DOS+WOS+SYEAR+COMMUNITY_CODE+GRID_NUM+SUBMITTER_NAME+VESSEL_NAME+DATE_FISHED+VR_NUMBER+LICENCE_ID,data=vv,FUN=sum),error=function(e) e)
              if(inherits(va,'error')){
                va = aggregate(LFA~DOS+WOS+SYEAR+COMMUNITY_CODE+GRID_NUM+SUBMITTER_NAME+VESSEL_NAME+DATE_FISHED+VR_NUMBER+LICENCE_ID,data=vv,FUN=length)
                va$LFA = NULL
                va$WEIGHT_KG = va$NUM_OF_TRAPS=NA
                }
          outa[[oo]] = va
          }
          out[[y]] = do.call(rbind,outa)
    }
  

#compiling by licence
grList = c(38:42,48:53,61:66,74:78,86:89,97:99,107,108)
oul = list()
for(i in 1:length(out)){
    x = out[[i]]
    x$Trips = 1
    xtr = aggregate(Trips~SYEAR+GRID_NUM+VR_NUMBER,data=x,FUN=sum)
    xGr = aggregate(GRID_NUM~SYEAR,data=subset(xtr,GRID_NUM %in% grList),FUN=function(x) length(unique(x)))
    xV = length(unique(x$VR_NUMBER))
    xS = aggregate(Trips~SYEAR+VR_NUMBER+SUBMITTER_NAME, data=x,FUN= sum)
    xL =  subset(o,Licence_Id==unique(x$LICENCE_ID))
    if(nrow(xL)>0) {if(length(grep(xL$Name_Last_First, o$Name_Last_First))>1)  browser()} #if name has 2 lics
    x1 = merge(x,te1z, by.x=c('GRID_NUM','DATE_FISHED'),by.y=c('grid','date'))
}

    

#Logbook Processing
a$DYR = lubridate::decimal_date(a$DATE_FISHED) - lubridate::year(a$DATE_FISHED)
a$WYR = ceiling(a$DYR*52)
a$DWYR = lubridate::year(a$DATE_FISHED) + a$WYR/52
a$P=1

xa = aggregate(cbind(WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA+SD_LOG_ID,data=a,FUN=sum)
xa$P = 1
x = aggregate(cbind(P,WEIGHT_KG, NUM_OF_TRAPS)~SYEAR+VR_NUMBER+LICENCE_ID+LFA,data=xa,FUN=sum)
x$CPUE = x$WEIGHT_KG/x$NUM_OF_TRAPS


#CPUE and vessel merge

xv = merge(x,d, by.x=c('VR_NUMBER','SYEAR','LFA'),by.y=c('VR_NUMBER','YR_FISHED','LFA'),all.x=T)

##CPUE and vessel and operator merge

xvo = merge(xv, o, by.x=c('LICENCE_ID','LFA'),by.y=c('Licence_Id','LFA'),all.x=T)




##aggregate catches and landing
aa = aggregate(cbind(NUM_OF_TRAPS,WEIGHT_KG)~SYEAR+DATE_FISHED+LFA+GRID_NUM+LICENCE,data=a,FUN=sum)
ab = aggregate(VR_NUMBER~SYEAR+DATE_FISHED+LFA+GRID_NUM,data=a,FUN=function(x) length(unique(x)))
aa = list(aa,ab) %>% purrr::reduce(full_join)
aT = merge(aa,te1z,by.x=c('LFA','GRID_NUM','DATE_FISHED'),by.y=c('LFA','grid','date'),all.x=T)
aT = subset(aT,GRID_NUM>0 & !is.na(temp))


