#wind data from Sable (source D Brickman 2020)

require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(PBSmapping)
require(mgcv)

dr = file.path(project.datadirectory('bio.lobster'),'data','wind')
a = read.table(file=file.path(dr, 'sable_daily.txt'),skip=13,header=T)

#meterological direction is where it comes from to where it is going with N as 0, first line of data states direction 62.5 which is ENE wind meterologically
# this data is incorrectly coded for direction (i.e. west is 270 instead of 0 as is typical)-- correcting the direction and recalculating U and Vspeeds  so need to use
# the wspd and dir (which are correct) to fix the Uspd and Vspd where Uspd is the xdirection and Vspd is the ydirection


a$mathDir = 270 - a$Dir

a$Uspd = a$Wspd * sin(a$mathDir*pi/180)
a$Vspd = a$Wspd * cos(a$mathDir*pi/180)

a$Date = as.Date(with(a, paste(Year, Mo, Dy, sep="-")), '%Y-%m-%d')


		logs = lobster.db("process.logs")
		vlog = lobster.db("process.vlog.redo")

		tmp1 = subset(logs,select=c("DATE_FISHED","SYEAR","WEIGHT_KG","LFA","NUM_OF_TRAPS","subarea","GRID_NUM"))
		tmp1$type = 'mandatory'
		tmp2 = subset(vlog,select=c("FDATE","SYEAR","W_KG","N_TRP","LFA","X","Y"))
		names(tmp2) = c("DATE_FISHED","SYEAR","WEIGHT_KG","NUM_OF_TRAPS","subarea","X","Y")
		tmp2$LFA = tmp2$subareas
		tmp2 = assignArea(tmp2,coords=c("X","Y"))
		tmp2 = subset(tmp2,select=c("DATE_FISHED","SYEAR","WEIGHT_KG","LFA","NUM_OF_TRAPS","subarea","LFA_GRID"))
		tmp2$type = 'voluntary'
	    names(tmp2) = names(tmp1)

	    cpue.data = rbind(tmp2,tmp1)


		xx = merge(cpue.data,a[,c('Year','Date','Dir','Wspd','Uspd','Vspd')],by.x='DATE_FISHED',by.y='Date')
		
		a$DL1 = a$Date+1
		a = rename.df(a,c('Dir','Wspd','Uspd','Vspd'),c('Dir_L1','Wspd_L1','Uspd_L1','Vspd_L1'))
		xx = merge(xx,a[,c('DL1','Dir_L1','Wspd_L1','Uspd_L1','Vspd_L1')],by.x='DATE_FISHED',by.y='DL1')
		
		a$DL2 = a$Date+2
		a = rename.df(a,c('Dir_L1','Wspd_L1','Uspd_L1','Vspd_L1'),c('Dir_L2','Wspd_L2','Uspd_L2','Vspd_L2'))
		xx = merge(xx,a[,c('DL2','Dir_L2','Wspd_L2','Uspd_L2','Vspd_L2')],by.x='DATE_FISHED',by.y='DL2')
	
		a$DL3 = a$Date+3
		a = rename.df(a,c('Dir_L2','Wspd_L2','Uspd_L2','Vspd_L2'),c('Dir_L3','Wspd_L3','Uspd_L3','Vspd_L3'))
		xx = merge(xx,a[,c('DL3','Dir_L3','Wspd_L3','Uspd_L3','Vspd_L3')],by.x='DATE_FISHED',by.y='DL3')
	

#Start with 32


	    g = subset(xx,LFA =='32')

	    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+Uspd+Vspd+Year+Wspd+Dir+
	    	Uspd_L1+Vspd_L1+Wspd_L1+Dir_L1+
	    	Uspd_L2+Vspd_L2+Wspd_L2+Dir_L2+
	    	Uspd_L3+Vspd_L3+Wspd_L3+Dir_L3
	    	,data=g,FUN=sum)
	    x$CPUE = x$WEIGHT_KG / x$NUM_OF_TRAPS
	    x=x[order(x$DATE_FISHED),]
	    with(subset(x,Year==1985),plot(DATE_FISHED,CPUE,type='l'))
	    x$lWt = log(x$WEIGHT_KG)
	    x$lTr = log(x$NUM_OF_TRAPS)
	    x$Doy = yday(x$DATE_FISHED)

#does Wind affect effort on a given day?
## offset to max traps fished within season so that total reporting effort is captured
	mT = aggregate(NUM_OF_TRAPS~Year,data=x, FUN=max)
	names(mT)[2] = 'maxTraps'
	x = merge(x,mT)
	x$propMaxTraps = x$NUM_OF_TRAPS / x$maxTraps

outs = gam(propMaxTraps~	s(Year)+
			   	s(Doy)+
			   	s(Uspd,Vspd)
			   	,data=x, family = betar(link='logit'), method='REML')

#you will get a warning about saturation -- likely due to low dispersion parameter--not terribly concerning

#converting u and v to direction
uvToDir = function(u,v){
		180+180/pi*atan2(v,u)
		}

uvToSpeed = function(u,v){
		sqrt(u^2+v^2)
		}
		

outs = gam(lWt~	s(Year)+
			   	s(Doy)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +
			   	offset(lTr),data=x, method='REML')



#################
	    g = subset(xx,LFA =='32')

	    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+Uspd+Vspd+Year+Wspd+Dir+
	    	Uspd_L1+Vspd_L1+Wspd_L1+Dir_L1+
	    	Uspd_L2+Vspd_L2+Wspd_L2+Dir_L2+
	    	Uspd_L3+Vspd_L3+Wspd_L3+Dir_L3
	    	,data=g,FUN=sum)
	    x$CPUE = x$WEIGHT_KG / x$NUM_OF_TRAPS
	    x=x[order(x$DATE_FISHED),]
	    with(subset(x,Year==1985),plot(DATE_FISHED,CPUE,type='l'))
	    x$lWt = log(x$WEIGHT_KG)
	    x$lTr = log(x$NUM_OF_TRAPS)
	    x$Doy = yday(x$DATE_FISHED)

#does Wind affect effort on a given day?
## offset to max traps fished within season so that total reporting effort is captured
	mT = aggregate(NUM_OF_TRAPS~Year,data=x, FUN=max)
	names(mT)[2] = 'maxTraps'
	x = merge(x,mT)
	x$propMaxTraps = x$NUM_OF_TRAPS / x$maxTraps

outs = gam(propMaxTraps~	s(Year)+
			   	s(Doy)+
			   	s(Uspd,Vspd)
			   	,data=x, family = betar(link='logit'), method='REML')

#you will get a warning about saturation -- likely due to low dispersion parameter--not terribly concerning

#converting u and v to direction
uvToDir = function(u,v){
		180+180/pi*atan2(v,u)
		}

uvToSpeed = function(u,v){
		sqrt(u^2+v^2)
		}
		

outs = gam(lWt~	s(Year)+
			   	s(Doy)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +
			   	offset(lTr),data=x, method='REML')



#Add in Glorys Temperature Data
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	#Get the unique EIDS from GLORYS data
		L = subset(LFAs,PID==32)
		fd = file.path(project.datadirectory('bio.lobster'),'data','GLORYS','SummaryFiles')
		gL = dir(fd,full.names=T)
		gL = gL[grep('Isobath',gL)]
		EIDs = readRDS(gL[1])
		EIDs = EIDs[!duplicated(EIDs[,c('X','Y','EID')]),c('X','Y','EID')]
		I = findPolys(EIDs,L)$EID
g = subset(xx,LFA==32)
    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+Uspd+Vspd+Year+Wspd+Dir+
	    	Uspd_L1+Vspd_L1+Wspd_L1+Dir_L1+
	    	Uspd_L2+Vspd_L2+Wspd_L2+Dir_L2+
	    	Uspd_L3+Vspd_L3+Wspd_L3+Dir_L3
	    	,data=g,FUN=sum)
	
out = list()
for(i in 1:length(gL)){
	jk = readRDS(gL[i])
	jk = subset(jk,EID %in% I & month(date) %in% 4:7 )
	out[[i]] = jk
}
	out = do.call(rbind,out)
	aGL = aggregate(cbind(vo_surface,vo_bottom,thetao,uo_surface,uo_bottom,bottomT,zos)~date, data=out,FUN=median)
	 aGL$DATE = as.Date(aGL$date)

	oo = merge(x,aGL, by.x='DATE_FISHED',by.y='DATE')

outs = gam(lWt~	s(Year)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +  s(bottomT)+ s(bottomT, Doy)+
			   	offset(lTr),data=oo, method='REML')


require(gratia)
draw(outs)
vis.gam(outs,view=c('bottomT','Doy'),plot.type='contour',too.far=.05)

###############
##LFA 31A



	    g = subset(xx,LFA %in% c('311','31A'))

	    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+Uspd+Vspd+Year+Wspd+Dir+
	    	Uspd_L1+Vspd_L1+Wspd_L1+Dir_L1+
	    	Uspd_L2+Vspd_L2+Wspd_L2+Dir_L2+
	    	Uspd_L3+Vspd_L3+Wspd_L3+Dir_L3
	    	,data=g,FUN=sum)
	    x$CPUE = x$WEIGHT_KG / x$NUM_OF_TRAPS
	    x=x[order(x$DATE_FISHED),]
	    x$lWt = log(x$WEIGHT_KG)
	    x$lTr = log(x$NUM_OF_TRAPS)
	    x$Doy = yday(x$DATE_FISHED)

#does Wind affect effort on a given day?
## offset to max traps fished within season so that total reporting effort is captured
	mT = aggregate(NUM_OF_TRAPS~Year,data=x, FUN=max)
	names(mT)[2] = 'maxTraps'
	x = merge(x,mT)
	x$propMaxTraps = x$NUM_OF_TRAPS / x$maxTraps

outs = gam(propMaxTraps~	s(Year)+
			   	s(Doy)+
			   	s(Uspd,Vspd)
			   	,data=x, family = betar(link='logit'), method='REML')

#you will get a warning about saturation -- likely due to low dispersion parameter--not terribly concerning


outs = gam(lWt~	s(Year)+
			   	s(Doy)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +
			   	offset(lTr),data=x, method='REML')



#Add in Glorys Temperature Data
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	#Get the unique EIDS from GLORYS data
		L = subset(LFAs,PID==311)
		fd = file.path(project.datadirectory('bio.lobster'),'data','GLORYS','SummaryFiles')
		gL = dir(fd,full.names=T)
		gL = gL[grep('Isobath',gL)]
		EIDs = readRDS(gL[1])
		EIDs = EIDs[!duplicated(EIDs[,c('X','Y','EID')]),c('X','Y','EID')]
		I = findPolys(EIDs,L)$EID
	
out = list()
for(i in 1:length(gL)){
	jk = readRDS(gL[i])
	jk = subset(jk,EID %in% I & month(date) %in% 4:7 )
	out[[i]] = jk
}
	out = do.call(rbind,out)
	aGL = aggregate(cbind(vo_surface,vo_bottom,thetao,uo_surface,uo_bottom,bottomT,zos)~date, data=out,FUN=median)
	 aGL$DATE = as.Date(aGL$date)

	oo = merge(x,aGL, by.x='DATE_FISHED',by.y='DATE')
	oo$lWt =log(oo$WEIGHT_KG)
	oo$lTr = log(oo$NUM_OF_TRAPS)

outs = gam(lWt~	s(Year)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +  s(bottomT)+ s(bottomT, Doy)+
			   	offset(lTr),data=oo, method='REML')


require(gratia)
draw(outs)
vis.gam(outs,view=c('bottomT','Doy'),plot.type='contour',too.far=.05)
##################
##31b




	    g = subset(xx,LFA %in% c('312','31B'))

	    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+Uspd+Vspd+Year+Wspd+Dir+
	    	Uspd_L1+Vspd_L1+Wspd_L1+Dir_L1+
	    	Uspd_L2+Vspd_L2+Wspd_L2+Dir_L2+
	    	Uspd_L3+Vspd_L3+Wspd_L3+Dir_L3
	    	,data=g,FUN=sum)
	    x$CPUE = x$WEIGHT_KG / x$NUM_OF_TRAPS
	    x=x[order(x$DATE_FISHED),]
	    x$lWt = log(x$WEIGHT_KG)
	    x$lTr = log(x$NUM_OF_TRAPS)
	    x$Doy = yday(x$DATE_FISHED)

#does Wind affect effort on a given day?
## offset to max traps fished within season so that total reporting effort is captured
	mT = aggregate(NUM_OF_TRAPS~Year,data=x, FUN=max)
	names(mT)[2] = 'maxTraps'
	x = merge(x,mT)
	x$propMaxTraps = x$NUM_OF_TRAPS / x$maxTraps

outs = gam(propMaxTraps~	s(Year)+
			   	s(Doy)+
			   	s(Uspd,Vspd)
			   	,data=x, family = betar(link='logit'), method='REML')

#you will get a warning about saturation -- likely due to low dispersion parameter--not terribly concerning


outs = gam(lWt~	s(Year)+
			   	s(Doy)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +
			   	offset(lTr),data=x, method='REML')



#Add in Glorys Temperature Data
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	#Get the unique EIDS from GLORYS data
		L = subset(LFAs,PID==312)
		fd = file.path(project.datadirectory('bio.lobster'),'data','GLORYS','SummaryFiles')
		gL = dir(fd,full.names=T)
		gL = gL[grep('Isobath',gL)]
		EIDs = readRDS(gL[1])
		EIDs = EIDs[!duplicated(EIDs[,c('X','Y','EID')]),c('X','Y','EID')]
		I = findPolys(EIDs,L)$EID
	
out = list()
for(i in 1:length(gL)){
	jk = readRDS(gL[i])
	jk = subset(jk,EID %in% I & month(date) %in% 4:7 )
	out[[i]] = jk
}
	out = do.call(rbind,out)
	aGL = aggregate(cbind(vo_surface,vo_bottom,thetao,uo_surface,uo_bottom,bottomT,zos)~date, data=out,FUN=median)
	 aGL$DATE = as.Date(aGL$date)

	oo = merge(x,aGL, by.x='DATE_FISHED',by.y='DATE')
	oo$lWt =log(oo$WEIGHT_KG)
	oo$lTr = log(oo$NUM_OF_TRAPS)

outs = gam(lWt~	s(Year)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +  s(bottomT)+ s(bottomT, Doy)+
			   	offset(lTr),data=oo, method='REML')


require(gratia)
draw(outs)
vis.gam(outs,view=c('bottomT','Doy'),plot.type='contour',too.far=.05)



#################
##30




	    g = subset(xx,LFA %in% c('30'))

	    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+Uspd+Vspd+Year+Wspd+Dir+
	    	Uspd_L1+Vspd_L1+Wspd_L1+Dir_L1+
	    	Uspd_L2+Vspd_L2+Wspd_L2+Dir_L2+
	    	Uspd_L3+Vspd_L3+Wspd_L3+Dir_L3
	    	,data=g,FUN=sum)
	    x$CPUE = x$WEIGHT_KG / x$NUM_OF_TRAPS
	    x=x[order(x$DATE_FISHED),]
	    x$lWt = log(x$WEIGHT_KG)
	    x$lTr = log(x$NUM_OF_TRAPS)
	    x$Doy = yday(x$DATE_FISHED)

#does Wind affect effort on a given day?
## offset to max traps fished within season so that total reporting effort is captured
	mT = aggregate(NUM_OF_TRAPS~Year,data=x, FUN=max)
	names(mT)[2] = 'maxTraps'
	x = merge(x,mT)
	x$propMaxTraps = x$NUM_OF_TRAPS / x$maxTraps

outs = gam(propMaxTraps~	s(Year)+
			   	s(Doy)+
			   	s(Uspd,Vspd)
			   	,data=x, family = betar(link='logit'), method='REML')

#you will get a warning about saturation -- likely due to low dispersion parameter--not terribly concerning


outs = gam(lWt~	s(Year)+
			   	s(Doy)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +
			   	offset(lTr),data=x, method='REML')



#Add in Glorys Temperature Data
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	#Get the unique EIDS from GLORYS data
		L = subset(LFAs,PID==30)
		fd = file.path(project.datadirectory('bio.lobster'),'data','GLORYS','SummaryFiles')
		gL = dir(fd,full.names=T)
		gL = gL[grep('Isobath',gL)]
		EIDs = readRDS(gL[1])
		EIDs = EIDs[!duplicated(EIDs[,c('X','Y','EID')]),c('X','Y','EID')]
		I = findPolys(EIDs,L)$EID
	
out = list()
for(i in 1:length(gL)){
	jk = readRDS(gL[i])
	jk = subset(jk,EID %in% I & month(date) %in% 4:7 )
	out[[i]] = jk
}
	out = do.call(rbind,out)
	aGL = aggregate(cbind(vo_surface,vo_bottom,thetao,uo_surface,uo_bottom,bottomT,zos)~date, data=out,FUN=median)
	 aGL$DATE = as.Date(aGL$date)

	oo = merge(x,aGL, by.x='DATE_FISHED',by.y='DATE')
	oo$lWt =log(oo$WEIGHT_KG)
	oo$lTr = log(oo$NUM_OF_TRAPS)

outs = gam(lWt~	s(Year)+
			   	s(Uspd_L1)+
			   	s(Vspd_L1) +  s(bottomT, Doy) +
			   	offset(lTr),data=oo, method='REML')


require(gratia)
draw(outs)
vis.gam(outs,view=c('bottomT','Doy'),plot.type='contour',too.far=.05)


