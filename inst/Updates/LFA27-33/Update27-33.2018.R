####### Update 27-33 2017:
p = bio.lobster::load.environment()
la()

#lobster.db('logs.redo',p=p)
lobster.db(DS='process.logs.redo', p=p)
logsInSeason<-lobster.db('process.logs')

p$lfas = c("27", "28", "29", "30", "31A", "31B", "32", "33") # specify lfas for data summary
p$subareas = c("27N","27S", "28", "29", "30", "31A", "31B", "32", "33E", "33W") # specify lfas for data summary


	TempModelling = TempModel( annual.by.area=F)
	CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
	CPUE.data<-CPUEModelData(p,redo=F)

    cpueSubAreaMan.dat = CPUEplot(subset(CPUE.data,type=="mandatory"),subarea= p$subareas,yrs=1981:2017,graphic='R')
 	manlogs = cpueSubAreaMan.dat$annual.dat

    cpueSubAreaVol.dat = CPUEplot(subset(CPUE.data,type=="voluntary"),subarea= p$subareas,yrs=1981:2017,graphic='R')
 	vollogs = cpueSubAreaVol.dat$annual.dat



#---------------------------------------------------------------------------Plots for Update
	require(lattice)
	require(plyr)
	require(ggplot2)

#for update

	
	#long.term.mean<-ddply(subset(manlogs,YEAR>2007), .(LFA), summarize, lt.mean=mean(CPUE))


	png(file.path(project.figuredirectory('bio.lobster'),'CommercialCPUELFA27332018.png'),width=8,height=10,units='in',res=200)
				p1 = ggplot(vollogs,aes(x=as.factor(YEAR),y=CPUE, group=LFA, colour="red")) + 
				  geom_line () + 
				  facet_wrap(~LFA, ncol=2) +
				  #geom_segment(aes(x='2008', y=lt.mean,xend='2017',yend=lt.mean,group=LFA), colour="cornflowerblue", linetype="dashed", data=long.term.mean) + 
				  geom_line(data=manlogs,colour="black") + 
				  geom_point(data=manlogs,colour="black") +
				  scale_y_continuous(limits=c(0,2.3), breaks=c(0,1,2)) + 
				  scale_x_discrete(breaks=seq(1980,2020,5))+ 
				  theme_bw() + 
				  theme(axis.text.x=element_text(angle=0,hjust=0.5,size=9.0),strip.text=element_text(size=12.0),legend.position="none") +
				  xlab("Year") + 
				  ylab("CPUE (Kg/Trap Haul)")
				  p1
	dev.off()
	
	# FSRS model plot 

	FSRSvesday<-FSRSModelData()
	#FSRSvesdayComm<-FSRSModelData(trap.type="commercial")
	FSRSModelResultsRecruit = list()
	FSRSModelResultsShort = list()
	FSRSModelResultsLegal = list()
	shorts.lst = list()
	legals.lst = list()
	recruit.lst = list()

	for(i in 1:length( p$subareas)){
	st = Sys.time()

		mdata = subset(FSRSvesday,subarea==p$subareas[i])

		FSRSModelResultsShort[[i]]=FSRSmodel(mdata, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		pdata	= 	FSRSModelResultsShort[[i]]$pData
		pdata$Area = p$subareas[i]
		shorts.lst[[i]] = pdata
		
		#FSRSModelResultsLegal[[i]]=FSRSmodel(mdata, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		#pdata	= 	FSRSModelResultsLegal[[i]]$pData
		#pdata$Area = p$subareas[i]
		#legals.lst[[i]] = pdata
		#	

		#FSRSModelResultsRecruit[[i]]=FSRSmodel(mdata, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		#pdata	= 	FSRSModelResultsRecruit[[i]]$pData
		#pdata$Area = p$subareas[i]
		#recruit.lst[[i]] = pdata
		print( Sys.time() - st)


	}

	names(FSRSModelResultsShort) = p$subareas
	#names(FSRSModelResultsLegal) = p$subareas
	#names(FSRSModelResultsRecruit) = p$subareas
	
	shorts = do.call("rbind",shorts.lst)
	#legals = do.call("rbind",legals.lst)
	#recruit = do.call("rbind",recruit.lst)
	shorts = subset(shorts,YEAR<2018)

	library(ggplot2)

	#pdf(file.path( project.figuredirectory('bio.lobster'),"FSRSmodelBayesShorts.pdf"),8, 10)
	png(file.path(project.figuredirectory('bio.lobster'),"FSRSmodelBayesShorts.png"),width=8,height=10,units='in',res=200)
	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = median, x = YEAR), shape = 16, size = 2)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = median), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp
	dev.off()

	##pdf(file.path( project.figuredirectory('bio.lobster'),"FSRSmodelBayesLegals.pdf"),8, 10)
	#png(file.path(project.figuredirectory('bio.lobster'),"FSRSmodelBayesLegals.png"),width=8,height=10,units='in',res=200)
	#lp <- ggplot()
	#lp <- lp + geom_point(data = legals, aes(y = median, x = YEAR), shape = 16, size = 2)
	#lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	#lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	#lp <- lp + geom_line(data = legals, aes(x = YEAR, y = median), colour = "black")
	#lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	#lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	#lp
	#dev.off()

	##pdf(file.path( project.figuredirectory('bio.lobster'),"FSRSmodelBayesRecruits.pdf"),8, 10)
	#png(file.path(project.figuredirectory('bio.lobster'),"FSRSmodelBayesRecruits.png"),width=8,height=10,units='in',res=200)
	#rp <- ggplot()
	#rp <- rp + geom_point(data = recruit, aes(y = median, x = YEAR), shape = 16, size = 2)
	#rp <- rp + xlab("Year") + ylab("Lobsters / Trap")
	#rp <- rp + theme(text = element_text(size=15)) + theme_bw()
	#rp <- rp + geom_line(data = recruit, aes(x = YEAR, y = median), colour = "black")
	#rp <- rp + geom_ribbon(data = recruit, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	#rp <- rp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	#rp

	dev.off()

	
	# Landings
	LandingsUpdate<-lobster.db('annual.landings')
	LR = as.data.frame(reshape(LandingsUpdate[,c("YR","LFA27",  "LFA28",  "LFA29",  "LFA30",  "LFA31",  "LFA32",  "LFA33")],
		varying=c("LFA27",  "LFA28",  "LFA29",  "LFA30",  "LFA31",  "LFA32",  "LFA33"),v.names='Landings',timevar='LFA',times=c("LFA27",  "LFA28",  "LFA29",  "LFA30", "LFA31",  "LFA32",  "LFA33"), direction='long'))
	 attr(LR,'reshapeLong') <- NULL
	 LR$LFA1 <- bio.utilities::recode(LR$LFA,c("'LFA28'='LFA28.32';'LFA29'='LFA28.32';'LFA30'='LFA28.32'; 'LFA31'='LFA28.32';'LFA32'='LFA28.32'"))

	 L =aggregate(Landings~LFA1+YR,data=LR,FUN=sum)
	
	# Manon's Plot version - adds Gulf landings portion, USR and LRP. Note that LFA33 USR and LRP are calculated from last data export and differs
	# from reference point document
	require(ggplot2)
	require(gridExtra)
	
	pdf(file.path(project.figuredirectory('bio.lobster'),'Commercial.Landings.LFA27-33.2017.pdf'),width=8,height=10)
			    	 L27 = subset(L,LFA1=='LFA27')
				#landings data from MComeau
					Gulf27 <- data.frame(YR = 2004:2015, LandingsGulf= c(115,	117,	118,	110,	138,	104,146.2,149.3,161.4,208.8,174.3,158.4)) 
					L27 = merge(L27,Gulf27,by='YR',all.x=T)	
					L27$LandingsGulf[which(is.na(L27$LandingsGulf))] <-0
					L27$Landings = L27$Landings - L27$LandingsGulf
					
					L27 = L27[,-4]
					Gulf27 <- data.frame(YR = 2004:2015,LFA1='27G', Landings= c(115,	117,	118,	110,	138, 104,146.2,149.3,161.4,208.8,174.3,158.4)) 
					L27 = rbind(L27,Gulf27)
					names(L27)[2] <- 'LFA'
					MEAN3YR<-c(NA,NA,1027,1106,1202,1217,1308,1268,1284,1241,1119,1017,1027,1145,1275,1174,1135,1037,1013,894,743,691,803,894,940,782,662,
					           547,535,540,480,369,337,312,337,356,396,651,1053,1695,2279,2590,2423,2230,2234,2260,2095,1900,1725,1763,1686,1906,2006,
					           2158,2277,2489,2515,2346,2265,2343,2703,2729,3011,3126,3561,4136,4792,5435,6083,7651)
					L27.rp<-data.frame(YR= rep(1947:2016,times=2),LFA=rep("LFA27",times=140),RP=c(rep("USR",times=70),rep("LRP",times=70)),
					                   Landings=c(rep(1629,times=70),rep(814,times=70)))
					L28.32.rp<-data.frame(YR= rep(1947:2016,times=2),LFA=rep("LFA28.32",times=),RP=c(rep("USR",times=70),rep("LRP",times=70)),
					                   Landings=c(rep(691,times=70),rep(346,times=70)))
					L33.rp<-data.frame(YR= rep(1947:2016,times=3),LFA=rep("LFA33",times=210),TYPE=c(rep("USR",times=70),rep("LRP",times=70),rep("3-year Mean",times=70)),
					                   Landings=c(rep(1794,times=70),rep(897,times=70),MEAN3YR))
					
				L27.rp$RP <- factor(L27.rp$RP)
				L27.rp$RP <- factor(L27.rp$RP, levels = rev(levels(L27.rp$RP)))
				p1<-ggplot(L27, aes(x=YR,y=Landings,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
				  scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2016, 4)) +
				  scale_fill_manual(values=c("#999999","#000000"),name='Landings',breaks=c("LFA27","27G"), labels=c("LFA 27","LFA 27 Gulf")) +
				  ggtitle("LFA 27") + theme(axis.text.x=element_text(size=10, colour='black'),panel.border=element_rect(fill=NA,colour='black'),
				  axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15),legend.position=c(0.10,0.69),
				  legend.title = element_blank()) + xlab('') + ylab('Landings (t)') + 
				  geom_line(data=L27.rp, aes(x=YR,y=Landings,group=RP,linetype=RP, color= RP),size=1.0) +
				  scale_color_manual(values=c("#0072B2","#009E73")) + scale_linetype_manual(values = c("dashed","solid")) 
				  
				 L28.32 <- subset(L,LFA1=='LFA28.32')
				 names(L28.32)[1] <- 'LFA'
				 L28.32.rp$RP <- factor(L28.32.rp$RP)
				 L28.32.rp$RP <- factor(L28.32.rp$RP, levels = rev(levels(L28.32.rp$RP)))
				 p2<-ggplot(L28.32,aes(x=YR,y=Landings,fill=(LFA))) + geom_bar(stat='identity', width=1.0) + guides(fill=FALSE) +
							   scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2016, 4)) +
				         scale_fill_manual(values="#000000") +ggtitle("LFA 28-32") + 
				         theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
							   panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
							   axis.title.y= element_text(size=15),legend.position=c(0.07,0.85),legend.title = element_blank()) + 
							   xlab('') + ylab('Landings (t)') +
				         geom_line(data=L28.32.rp, aes(x=YR,y=Landings,group=RP,linetype=RP, color= RP),size=1.0) +
				         scale_color_manual(values=c("#0072B2","#009E73")) + scale_linetype_manual(values = c("dashed","solid")) 
				   
				LandingsUpdate<-lobster.db('seasonal.landings')
				LU = LandingsUpdate[,c('SYEAR','LFA33')]
				LU$YR = 1976:2017
				L3 = subset(L,LFA1=='LFA33' & YR<1976)
				LU = subset(LU,YR<2017,select=c(LFA33,YR))
				names(LU) <- c('Landings','YR')
				LU$TYPE <- 'Seasonal'
				L3 = L3[,c('Landings','YR')]
				L3$TYPE = 'Annual'
				LU = rbind(L3,LU)
				L33.rp$TYPE <- factor(L33.rp$TYPE)
				L33.rp$TYPE <- factor(L33.rp$TYPE, levels = rev(levels(L33.rp$TYPE)))
				
				 p3<-ggplot(LU,aes(x=YR,y=Landings,colour=TYPE, group=TYPE)) + 
							   geom_bar(data=subset(LU,TYPE=='Annual'),fill='darkgray',colour='darkgray',stat='identity', width=1.0) + 
							   geom_bar(data=subset(LU,TYPE=='Seasonal'),fill='black',colour='black', stat='identity',width=1.0) + 
				         #geom_line(data=LU, aes(x=YR,y=MEAN3YR), color="#D55E00") + geom_point(data=LU, aes(x=YR,y=MEAN3YR),colour="#D55E00") +
				   
							   scale_y_continuous(limits=c(0,10200),breaks=seq(0, 10200, 1000)) + scale_x_continuous(breaks=seq(1947, 2016, 4)) + 
				         ggtitle("LFA 33") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
							   panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
							   axis.title.y= element_text(size=15),legend.position=c(0.10,0.80),legend.title = element_blank()) + 
                 xlab('') + ylab('Landings (t)') + 
				         geom_line(data=L33.rp, aes(x=YR,y=Landings,group=TYPE,linetype=TYPE, color= TYPE),size=1.0) +
                 scale_color_manual(values=c("#0072B2","#009E73","#D55E00")) + scale_linetype_manual(values = c("dashed","solid","dotted"))  
				   
  							 grid.arrange(p1,p2,p3,ncol=1)  
 dev.off()
 

	# for LFA33 presentation
	pdf(file.path( project.datadirectory("lobster"),"R","FSRScpueLFA33.pdf"),8, 5)
	xyplot(cpue~year|subarea, data=subset(cpue.dat,lfa=='33'), ylab="CPUE (Kg / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,1))
	xyplot(pred.s.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='33'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,4.5))
	xyplot(pred.l.cpue~SYEAR|subarea, data=subset(FSRScpue.dat,LFA=='33'), ylab="CPUE (No. Lobsters / Trap Haul)",xlab= "Year", as.table=T,type='b',ylim=c(0,1.2))
	dev.off()

	subareas<-read.csv(file.path( project.datadirectory("lobster"), "data","LFA2733subarea.csv"))
	polyprop33<-merge(subset(subareas,LFA==33),data.frame(LFA=33,subarea=c("33 West","33 East"),col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5))),all=T)
	names(polyprop33)[1:3]<-c("PID","label","SID")
	LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAgridPolys.csv"))

	pdf(file.path( project.datadirectory("lobster"),"R","LFA33map.pdf"),8, 8)
	LobsterMap('33',poly.lst=list(subset(LFAgrid,PID==33),polyprop33),boundaries='none',isobaths=c(seq(50,450,50),seq(500,1000,100)),bathcol=rgb(0,0,1,0.3))
	legend('bottomright',c("West","East"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
	dev.off()



