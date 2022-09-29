#wind data from Sable (source D Brickman 2020)

require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(PBSmapping)
require(mgcv)
require(gratia)
dr = file.path(project.datadirectory('bio.lobster'),'data','wind')
a = read.table(file=file.path(dr, 'sable_daily.txt'),skip=13,header=T)
la()
#meterological direction is where it comes from to where it is going with N as 0, first line of data states direction 62.5 which is ENE wind meterologically
#(and thus -ve u and -ve v)
# this data is incorrectly coded for direction (i.e. west is 270 instead of 0 as is typical)-- correcting the direction and recalculating U and Vspeeds  so need to use
# the wspd and dir (which are correct) to fix the Uspd and Vspd where Uspd is the xdirection and Vspd is the ydirection


a$mathDir = 270 - a$Dir

a$Uspd = a$Wspd * sin(a$mathDir*pi/180)
a$Vspd = a$Wspd * cos(a$mathDir*pi/180)

a$Date = as.Date(with(a, paste(Year, Mo, Dy, sep="-")), '%Y-%m-%d')

	port_loc = lobster.db("port_location")
	
		logs = lobster.db("process.logs")
		vlog = lobster.db("process.vlog.redo")
		logs = merge(logs,port_loc,by.y='PORT_CODE',by.x='COMMUNITY_CODE')
		vlog$LICENCE_ID = paste(vlog$PORT_CODE, vlog$FCODE,sep="-")
		
		tmp1 = subset(logs,select=c("DATE_FISHED","SYEAR","WEIGHT_KG","LFA.x","NUM_OF_TRAPS","GRID_NUM","COMMUNITY_CODE",'CENTLON','CENTLAT','LICENCE_ID'))
		tmp1$type = 'mandatory'
		tmp2 = subset(vlog,select=c("FDATE","SYEAR","W_KG","N_TRP","LFA","X","Y",'PORT_CODE',"LICENCE_ID"))
		names(tmp2) = c("DATE_FISHED","SYEAR","WEIGHT_KG","NUM_OF_TRAPS","subarea","X","Y","COMMUNITY_CODE","LICENCE_ID")
		tmp2$LFA.x = tmp2$subareas
		tmp2 = assignArea(tmp2,coords=c("X","Y"))
		tmp2 = subset(tmp2,select=c("DATE_FISHED","SYEAR","WEIGHT_KG","LFA","NUM_OF_TRAPS","LFA_GRID",'COMMUNITY_CODE','X',"Y","LICENCE_ID"))
		tmp2$type = 'voluntary'
	    names(tmp2) = names(tmp1)

	    cpue.data = rbind(tmp2,tmp1)


#WIND
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
	
		xx = rename.df(xx,c('CENTLON','CENTLAT','LFA.x'),c('X','Y','LFA'))

		xx = subset(xx, LFA %in% c(27, 28, 29, 30, '31A','31B',32) )

	 
    #LFA Polygons
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	L = subset(LFAs, PID %in% c(27, 28, 29, 30, 311,312,32) )
	#
	##glorys data is still loadings sept 14
	
		fd = file.path(project.datadirectory('bio.lobster'),'data','GLORYS','SummaryFiles')
		gL = dir(fd,full.names=T)
		gL = gL[grep('Isobath',gL)]
		EIDs = readRDS(gL[1])
		EIDs = EIDs[!duplicated(EIDs[,c('X','Y','EID')]),c('X','Y','EID')]
		I = findPolys(EIDs,L)


	#by Year
out = list()
for(i in 1:length(gL)){
	jk = readRDS(gL[i])
	jk = subset(jk,EID %in% unique(I$EID) & month(date) %in% 4:7 )
	out[[i]] = jk
}
	out = do.call(rbind,out)
	out = merge(out,I[,c('EID','PID')], )
	aGL = aggregate(cbind(vo_surface,vo_bottom,thetao,uo_surface,uo_bottom,bottomT,zos)~date+PID, data=out,FUN=median)
	 aGL$DATE = as.Date(aGL$date)

	oo = merge(xx,aGL, by.x=c('DATE_FISHED','LFA'),by.y=c('DATE','PID'))

oo$lWt = log(oo$WEIGHT_KG)
oo$lTr = log(oo$NUM_OF_TRAPS)
oo$COMMUNITY_CODEf = as.factor(oo$COMMUNITY_CODE)
oo$Doy = yday(oo$DATE_FISHED)

saveRDS(oo,file.path('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\EAFM\\CPUETempWindData.rds'))
