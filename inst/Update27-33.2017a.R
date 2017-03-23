####### Update 27-33 2017:
p=list()
p$yrs = 1947:2016
require(bio.lobster)
lobster.db('logs.redo',p=p)
lobster.db(DS='process.logs.redo', p=p)



#Update the log book data
	#Filtering by date trap and weight:
	Fish.Date<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FishingSeasonDates.csv"))
	lfa <- unique(Fish.Date$LFA)
	max_trap<-c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
	max_lbs<-c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
	#If Linux
	Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,"%Y-%m-%d")
	Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,"%Y-%m-%d")
  #If Windows
	Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,"%d/%m/%Y")
	Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,"%d/%m/%Y")
	
## LOGS
logsInSeason<-lobster.db('process.logs')
	

### SUMMERIZE CPUE
### by subarea for 27 & 33, nogrid excluded
cpue.lst<-list()
cpue1.lst<-list()
for(i in 1:length(lfa)) {
	dat<-subset(logsInSeason,LFA==lfa[i]&TOTAL_NUM_TRAPS<max_trap[i],c('subarea','SYEAR','WEIGHT_KG','NUM_OF_TRAPS'))
	if(lfa[i]%in%c('27','33')){
		subs<-unique(dat$subarea)
		for(j in 1:length(subs)){
			sdat<-na.omit(subset(dat,subarea==subs[j],c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS')))
			catch<-with(sdat,tapply(WEIGHT_KG,SYEAR,sum))
			effort<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,sum))
			n<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,length))

			cpue1.lst[[j]]<-data.frame(lfa=lfa[i],subarea=subs[j],year=sort(unique(sdat$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)

		}
		cpue.lst[[i]]<-do.call("rbind",cpue1.lst)
	}
	if(!lfa[i]%in%c('27','33')){
		sdat<-na.omit(subset(dat,select=c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS')))
		catch<-with(sdat,tapply(WEIGHT_KG,SYEAR,sum))
		effort<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,sum))
		n<-with(sdat,tapply(NUM_OF_TRAPS,SYEAR,length))

		cpue.lst[[i]]<-data.frame(lfa=lfa[i],subarea=lfa[i],year=sort(unique(sdat$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
	}
}

names(cpue.lst)<-lfa
cpue.dat<-do.call("rbind",cpue.lst)

write.csv(cpue.dat,file.path( project.datadirectory("bio.lobster"), "data","products","CommercialCPUE.csv"),row.names=F)

### by LFA
cpue2.lst<-list()
for(i in 1:length(lfa)) {
	dat<-na.omit(subset(logsInSeason,LFA==lfa[i]&TOTAL_NUM_TRAPS<max_trap[i],c('SYEAR','WEIGHT_KG','NUM_OF_TRAPS')))
	catch<-with(dat,tapply(WEIGHT_KG,SYEAR,sum))
	effort<-with(dat,tapply(NUM_OF_TRAPS,SYEAR,sum))
	n<-with(dat,tapply(NUM_OF_TRAPS,SYEAR,length))

	cpue2.lst[[i]]<-data.frame(lfa=lfa[i],year=sort(unique(dat$SYEAR)),n=n,catch=catch,effort=effort,cpue=catch/effort)
}

names(cpue2.lst)<-lfa
LOGcpue.dat<-do.call("rbind",cpue2.lst)

write.csv(LOGcpue.dat,file.path( project.datadirectory("bio.lobster"), "data","products","CommercialCPUE_LFA.csv"),row.names=F)


#---------------------------------------------------------------------------Plots for Update
	require(lattice)
	require(plyr)
	require(bio.utilities)

#for update

	LOGcpue.dat<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","CommercialCPUE_LFA.csv"))
	cpue.dat<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","CommercialCPUE.csv"))
	vollog.dat<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","products","VolLogsCPUE_LFA.csv"))
	vollog.dat$lfa = bio.utilities::recode(vollog.dat$lfa,"'27N'='27 North';'27S'='27 South'; '33E'='33 East'; '33W' = '33 West' ")
	logs.0016<-subset(cpue.dat,year<=2016 & lfa<34)
	logs.0016$lfa <- logs.0016$subarea
	long.term.mean<-ddply(subset(logs.0016,year>2007), .(lfa), summarize, lt.mean=mean(cpue))
	require(ggplot2)


	vollog.27<-subset(vollog.dat,lfa %in% c('27 North','27 South'))
	
	pdf(file.path(project.figuredirectory('bio.lobster'),'Commercial.CPUE.Vollog.LFA27-33.2017.pdf'),width=8,height=10)
				p1 = ggplot(vollog.dat,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~lfa, ncol=2) +
				  geom_segment(aes(x='2008', y=lt.mean,xend='2015',yend=lt.mean,group=lfa), colour="cornflowerblue", linetype="dashed",
				  data=subset(long.term.mean,lfa<"34")) + geom_line(data=logs.0016,colour="black") + 
				  geom_point(data=logs.0016,colour="black") +scale_y_continuous(limits=c(0,2.2), breaks=c(0,1,2)) + 
				  scale_x_discrete(breaks=seq(1981,2016,4))+ 
				  theme_bw() + theme(axis.text.x=element_text(angle=0,hjust=0.5,size=9.0),strip.text=element_text(size=12.0),legend.position="none") +
				  xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
				  p1
	dev.off()
	
	#Plot by LFA
	vollog.27<-subset(vollog.dat,lfa %in% c('27 North','27 South'))
	logs.27<-subset(logs.0815,lfa %in% c('27 North','27 South'))
	vollog.2829<-subset(vollog.dat,lfa %in% c("28","29"))
	logs.2829<-subset(cpue.dat,lfa %in% c("28","29"))
	
	
	pdf(file.path(project.figuredirectory('bio.lobster'),'Commercial.CPUE.Vollog.LFA27.2017.pdf'),width=8,height=10)
	p1 = ggplot(vollog.2829,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~lfa, ncol=1) +
	  geom_segment(aes(x='2008', y=lt.mean,xend='2015',yend=lt.mean,group=lfa), colour="cornflowerblue", 
	               data=subset(long.term.mean,lfa %in% c("28","29"))) + geom_line(data=logs.2829,colour="black") + 
	  geom_point(data=logs.2829,colour="black") +scale_y_continuous(limits=c(0,2.0), breaks=c(0,0.50,1.00,1.50,2)) + 
	  scale_x_discrete(breaks=seq(1981,2016,5))+ 
	  theme_bw() + theme(axis.text.x=element_text(angle=0,hjust=0.5,size=9.0),strip.text=element_text(size=12.0),legend.position="none") +
	  xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
	p1
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
