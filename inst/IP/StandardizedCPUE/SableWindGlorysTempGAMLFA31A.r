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
	
		xx = rename.df(xx,c('CENTLON','CENTLAT'),c('X','Y'))
###

	    g = subset(xx,LFA.x %in% c(311,'31A'))

	    x = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~DATE_FISHED+COMMUNITY_CODE+Uspd+Vspd+Year+Wspd+Dir+
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

		g$lWt = log(g$WEIGHT_KG)
	  	g$lTr = log(g$NUM_OF_TRAPS)
	   	g$Doy = yday(g$DATE_FISHED)

#does Wind affect effort on a given day?
## offset to max traps fished within season so that total reporting effort is captured
	
	mT = aggregate(NUM_OF_TRAPS~Year+COMMUNITY_CODE,data=g, FUN=max)
	names(mT)[2] = 'maxTraps'
	x = merge(x,mT)
	x$propMaxTraps = x$NUM_OF_TRAPS / x$maxTraps

#converting u and v to direction
uvToDir = function(u,v){
		180+180/pi*atan2(v,u)
		}

uvToSpeed = function(u,v){
		sqrt(u^2+v^2)
		}
	
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


	#by LFA
out = list()
for(i in 1:length(gL)){
	jk = readRDS(gL[i])
	jk = subset(jk,EID %in% I & month(date) %in% 4:7 )
	out[[i]] = jk
}
	out = do.call(rbind,out)
	aGL = aggregate(cbind(vo_surface,vo_bottom,thetao,uo_surface,uo_bottom,bottomT,zos)~date, data=out,FUN=median)
	 aGL$DATE = as.Date(aGL$date)

	oo = merge(g,aGL, by.x='DATE_FISHED',by.y='DATE')

oo$lWt = log(oo$WEIGHT_KG)
oo$lTr = log(oo$NUM_OF_TRAPS)
oo$COMMUNITY_CODEf = as.factor(oo$COMMUNITY_CODE)
oo$Doy = yday(oo$DATE_FISHED)

oo$SoakDays = NA 
iu = unique(oo$LICENCE_ID)
outt = list()
mm=0
for(i in 1:length(iu)){
		u = subset(oo,LICENCE_ID ==iu[i])
		ge = unique(u$SYEAR)
		for(j in 1:length(ge)){
				mm=mm+1
				vc = subset(u,SYEAR==ge[j])
				if(nrow(vc)>1){
				vc$SoakDays = c(1,vc$DATE_FISHED[2:nrow(vc)] - vc$DATE_FISHED[1:(nrow(vc)-1)] )
				outt[[mm]] = vc
			}
		}
	}

outall = do.call(rbind,outt)
outall = subset(outall,SoakDays<10)
outall = subset(outall, is.finite(lWt))


###predators 


require(bio.survey)
fp1 = file.path(project.datadirectory('bio.lobster'),"analysis","LFA34-38")
p=list()
#loadfunctions('bio.groundfish')
p$strat=443:457
p$series =c('summer')# p$series =c('4vswcod');p$series =c('georges')
p$years.to.estimate = c(1970:2017)
p$functional.groups = T
yy = list()
yy[['LobPred']] = c(10,11,12,13,40,50,200,201,203,204,300)
p$species = 'LobPred'
p$yy = yy

p$vessel.correction = T
p$vessel.correction.fixed = 1.2
p$length.based = F
p$by.sex = p$sex.based = F
p$alpha = 0.05
p$strata.efficiencies=F
#out = groundfish.analysis(DS='ab.redo',p=p)
#MPA functional groups
p$functional.groups = T
p$bootstrapped.ci=F
p$strata.files.return=F

p$clusters = c( rep( "localhost", 7) )

p = make.list(list(v=p$species, yrs=p$years.to.estimate),Y=p)
p$runs = p$runs[order(p$runs$v),]
#parallel.run(groundfish.analysis,DS='stratified.estimates.redo',p=p,specific.allocation.to.clusters=T) #silly error arisingexit


aout= groundfish.analysis(DS='stratified.estimates.redo',out.dir = 'bio.lobster',p=p)
aout = subset(aout, select=c('yr','w.yst','n.yst'))
aout$SYEAR7 = aout$yr+7
aout$LBP = log(aout$w.yst)
outall = merge(outall, aout, by.x='Year', by.y='SYEAR7')


#best model Feb 16
outs = bam(lWt~	as.factor(Year)+
			  s(Uspd_L1,Vspd_L1)+
			   	s(bottomT)+ 
			   	s(Doy,bottomT)+
			   	COMMUNITY_CODEf +
			   	(SoakDays) +
			   	offset(lTr),data=subset(outall,month(DATE_FISHED)>4), method='REML')

#outall$yr = outall$n.yst = outall$w.yst = outall$LBP = NULL
require(gratia)
draw(outs, parametric=F)
savePlot('~/dellshared/lfa31ACPUEFactors.png')
newD = data.frame(Year=as.factor(1993:2018), Uspd_L1=0, Vspd_L1=0, bottomT = 3.9, Doy = 125, COMMUNITY_CODEf = '11511', SoakDays=2, lTr = log(1), LBP=4)
predict(outs, newdata=newD)
	a_lp_matrix = predict(object = outs, newD,
		               type = "lpmatrix")
	n_sims =1000	

		a_coef_mean = coef(outs)
		a_vcov = vcov(outs)
		a_par_coef_posterior = rmvn(n = n_sims, 
                                    mu = a_coef_mean,
          	                        V = a_vcov)
		ilink = family(outs)$linkinv

		preds = ilink(a_lp_matrix %*% t(a_par_coef_posterior))
 		apreds = exp(as.data.frame(preds))
	ag = apply(apreds[,1:1000],1,quantile,c(.025,0.5,.975))
par(mfrow = c(2,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.3, 0, 0.2))
#par(mfrow=c(1,2))  
plot(1993:2018, ag[2,],type='b',pch=16,xlab='Year', ylab='Standardized CPUE',ylim=c(0,2.8), xlim=c(1986,2020))
arrows(1993:2018,y0=ag[3,], y1=ag[1,], length=0)		

   yrs =1993:2018 
  		  
          prs = seq( from=0.025, to=0.975, length.out=100)
          Bq =  apply(apreds[1:17,1:1000], 1, quantile, probs=prs, na.rm=T  )
	      cols = gray.colors( floor(length( prs)/2) )
          cols2 = c(cols[length(cols):1], cols )
          for ( j in 1:length(prs) ) {
            abline( h=median(Bq[j,])*.4, lwd=4, col=cols2[j] )
          }
           abline( h=median(Bq[50,])*.4, lwd=1, col='red' )
          
lines(1993:2018, ag[2,],type='b',pch=16)
arrows(1993:2018,y0=ag[3,], y1=ag[1,], length=0)		

cD = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR, data=subset(cpue.data, LFA.x %in% c(311,'31A')),FUN=sum)
cD$CPUE = cD$WEIGHT_KG / cD$NUM_OF_TRAPS
#x11()
plot(cD$CPUE~cD$SYEAR,type='b',xlab='Year',pch=16,ylab='Raw CPUE')
abline(h=median(cD$CPUE[1:24])*.4,lwd=2,col='red')
savePlot('dellshared/LFA31ACPUE.png')
#MLS
1997	31A	81
1998	31A	82.5
1999	31A	84
2000	31A	86
2001	31A	86
2002	31A	86
2003	31A	86
2004	31A	84
2005	31A	84
2006	31A	84
2007	31A	82.5

plot(1994:2018,(ag[2,2:ncol(ag)] - ag[2,1:(ncol(ag)-1)])/ag[2,1:(ncol(ag)-1)])
abline(v=c(1998,1999,2005,2006))


pred = aggregate(LBP~Year,data=outall, FUN=mean)
temp = aggregate(bottomT~Year,data=outall, FUN=mean)

cor.test((pred[,2]),(ag[2,]))
cor.test((temp[1:19,2]),(ag[2,8:ncol(ag)]))

plot((pred[,2]),(ag[2,]))
plot((temp[1:19,2]),(ag[2,8:ncol(ag)]))

plot(1993:2018, ag[2,],type='b',pch=16,xlab='Year', ylab='Standardized CPUE',ylim=c(0,2), xlim=c(1986,2020))
arrows(1993:2018,y0=ag[3,], y1=ag[1,], length=0)		
abline(v=c(1998,1999,2000,2001, 2005,2006, 2007,2008),col=c('blue','blue','blue','green','red','red','red','orange'))
savePlot('dellshared/LFA31AMLSinc.png')


plot(1993:2018, ag[2,],type='b',pch=16,xlab='Year', ylab='Standardized CPUE',ylim=c(0,2), xlim=c(1986,2020))
arrows(1993:2018,y0=ag[3,], y1=ag[1,], length=0)		
abline(v=c(2004, 2007, 2011, 2014),col=c('blue','blue','red','red'))
savePlot('dellshared/LFA31AMLSdec.png')

#offset to max traps fished within season so that total reporting effort is captured
	
ooo = aggregate(NUM_OF_TRAPS~Year+COMMUNITY_CODE+DATE_FISHED+Doy+Uspd+Uspd_L1+Uspd_L2+Uspd_L3+Vspd+Vspd_L1+Vspd_L2+Vspd_L3,data=outall, FUN=sum)
	mT = aggregate(NUM_OF_TRAPS~Year+COMMUNITY_CODE,data=ooo, FUN=max)
	names(mT)[3] = 'maxTraps'
	oo = merge(ooo,mT)
	oo$propMaxTraps = oo$NUM_OF_TRAPS / oo$maxTraps
	oo$COMMUNITY_CODEf	= as.factor(oo$COMMUNITY_CODE)
outsEffort = gam(propMaxTraps~	s(Year)+
			   	s(Doy)+
			   	s(Uspd,Vspd, by=COMMUNITY_CODEf)
			   	,data=subset(oo,Year>1991), family = betar(link='logit'), method='REML')
draw(outsEffort)
savePlot('~/tmp/lfa29Effort.png')

outT = gam(bottomT~	s(Year)+
			   	s(Uspd_L1,Vspd_L1)+s(Doy),
			   	data=outall, method='REML')
draw(outT)
savePlot('~/tmp/lfa29bottomT.png')

outW = gam(Uspd~	s(Year)+
			   	s(Doy),
			   	data=outall, method='REML')
draw(outW)
savePlot('~/tmp/lfa29WU.png')

outV = gam(Vspd~	s(Year)+
			   	s(Doy),
			   	data=outall, method='REML')
draw(outV)
savePlot('~/tmp/lfa29WV.png')


