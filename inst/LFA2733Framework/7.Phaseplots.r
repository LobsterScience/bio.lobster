#7. Phase plots

#CCIR RR with landings based reference points


# CCIR
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()

load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledExploitationCCIR.rdata'))

RRs = aggregate(ERfm~LFA,data=oi,FUN=max)
RR75  = aggregate(ERf75~LFA,data=oi,FUN=max)



#catch rate phase plots


la()
load(file=file.path(project.datadirectory('bio.lobster'),'outputs','rawcpueIndicators27-33B.rdata')) 

hq = rename.df(cpueData2,'YEAR','YR')
hq$DATE = hq$CATCH = hq$EFFORT <- NULL

hq = subset(hq,YR>=1985 & YR<2017)
ad = c(27,29,30,'31A','31B',32,33)

mu = aggregate(CPUE~LFA,data=subset(hq,YR %in% 1990:2016),FUN=median)

	mu$usr = mu$CPUE * 0.8
	mu$lrp = mu$CPUE * 0.4
	
	for(i in 1:length(ad)) {
				print(ad[i])
				poi = po = ad[i]
				if(po == 27) 	{  poi = "LFA 27 Combined"}
				if(po == 29) 	{  poi = 'LFA 29'}
				if(po == 30) 	{  poi = 'LFA 30'}
				if(po == '31A') {  poi = 'LFA 31A'}
				if(po == '31B') {  poi = 'LFA 31B'}
				if(po == 32) 	{  poi = 'LFA 32'}
				if(po == 33) 	{ poi = "LFA 33 Combined"}

				oo = subset(oi,LFA==poi,select=c(Yr,ERfm))
				
      
				RR = subset(RRs,LFA==poi)$ERfm
				RR7 = subset(RR75,LFA==poi)$ERf75
				usr = subset(mu,LFA==ad[i])$usr
				lrp = subset(mu,LFA==ad[i])$lrp

				#oo = subset(oi,LFA==poi)
				ot = data.frame(Yr=seq(min(oo$Yr),max(oo$Yr)))
				oo = merge(ot,oo,all=T)
				iw = NULL
				if(any(is.na(oo$ERfm))) iw = oo[which(is.na(oo$ERfm)),'Yr']
				op = rmed(oo$Yr,oo$ERfm)
				
				RR = subset(RRs,LFA==poi)$ERfm
				RR7 = subset(RR75,LFA==poi)$ERf75
				mm = min(op$yr)
				lp = subset(hq,YR>=mm & LFA == ad[i],select=c(YR,CPUE))
				lp = rmed(lp$YR,lp[,2]) 

				if(!is.null(iw)) {
					ih = which(lp$yr == iw)
					lp$yr = lp$yr[-ih]
					lp$x = lp$x[-ih]
				}
				hcrPlot(B=lp$x,mF=op$x,USR=usr,LRP=lrp,RR=RR7,yrs=lp$yr,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T) 
					fd = file.path(project.figuredirectory('bio.lobster'),'ReferencePoints')
				 title(paste('LFA',po))
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
			savePlot(file.path(fd,paste('PhasePlotsCPUECCIR',po,'.png',sep='')),type='png')
		
	}



#landings phase plots
	g = lobster.db('annual.landings')
	p = lobster.db('seasonal.landings')
	g = g[-nrow(g),]
	p = p[-nrow(p),]


	g = subset(g,YR>=1985)
	gg = subset(g,YR<=2009)

	apply(gg,2,median)*0.8

	ad = c(27,29,30,'31A','31B',32,33)
	usr = c(1629,113,79,110,160,242,1838)
	lrp = usr/2

	for(i in 1:length(ad)) {
			print(ad[i])
				poi = po = ad[i]
				if(po == 27) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]; poi = "LFA 27 Combined"}
				if(po == 29) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]; poi = 'LFA 29'}
				if(po == 30) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]; poi = 'LFA 30'}
				if(po == '31A') { lp = g[,c('YR',names(g)[grep(po,names(g))])]; poi = 'LFA 31A'}
				if(po == '31B') { lp = g[,c('YR',names(g)[grep(po,names(g))])]; poi = 'LFA 31B'}
				if(po == 32) 	{ lp = g[,c('YR',names(g)[grep(po,names(g))])]; poi = 'LFA 32'}
				if(po == 33) 	{ lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); lp$YR = as.numeric(substr(lp$YR,6,9)); lp = subset(lp,YR>=1985); poi = "LFA 33 Combined"}
				op = list()

				oo = subset(oi,LFA==poi)
				ot = data.frame(Yr=seq(min(oo$Yr),max(oo$Yr)))
				oo = merge(ot,oo,all=T)
				iw = NULL
				if(any(is.na(oo$ERfm))) iw = oo[which(is.na(oo$ERfm)),'Yr']
				op = rmed(oo$Yr,oo$ERfm)
				
      
				RR = subset(RRs,LFA==poi)$ERfm
				RR7 = subset(RR75,LFA==poi)$ERf75
				mm = min(op$yr)
				lp = subset(lp,YR>=mm)
				lp = rmed(lp$YR,lp[,2]) 

				if(!is.null(iw)) {
					ih = which(lp$yr == iw)
					lp$yr = lp$yr[-ih]
					lp$x = lp$x[-ih]
				}
				hcrPlot(B=lp$x,mF=op$x,USR=usr[i],LRP=lrp[i],RR=RR,yrs=lp$yr,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'Landings',yr.ends=T) 
					fd = file.path(project.figuredirectory('bio.lobster'),'ReferencePoints')
				 arrows(x0 = usr[i], x1 = usr[i]*1000, length=0,y0 =RR7 , lty="dashed", col="blue", lwd=2 )
				 title(paste('LFA',po))
			dir.create( fd, recursive = TRUE, showWarnings = FALSE )
			savePlot(file.path(fd,paste('PhasePlotsLandingsCCIR',po,'.png',sep='')),type='png')
			

			}

