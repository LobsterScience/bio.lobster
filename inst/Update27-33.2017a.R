####### Update 27-33 2017:
p=list()
p$yrs = 1947:2016
require(bio.lobster)
#lobster.db('logs.redo',p=p)
lobster.db(DS='process.logs.redo', p=p)



#Update the log book data
	#Filtering by date trap and weight:
	Fish.Date<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FishingSeasonDates.csv"))
	lfa <- unique(Fish.Date$LFA)
	max_trap<-c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
	max_lbs<-c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
	Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,"%Y-%m-%d")
	Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,"%Y-%m-%d")


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
	vollog.dat$lfa = recode(vollog.dat$lfa,"'27N'='27 North';'27S'='27 South'; '33E'='33 East'; '33W' = '33 West' ")
	logs.0814<-subset(cpue.dat,year<2017&lfa<34)
	logs.0814$lfa <- logs.0814$subarea
	long.term.mean<-ddply(subset(logs.0814,year>2007), .(lfa), summarize, lt.mean=mean(cpue))
	require(ggplot2)




	pdf(file.path(project.figuredirectory('bio.lobster'),'Commercial.CPUE.Vollog.LFA27-33.2017.pdf'),width=8,height=10)
				p1 = ggplot(vollog.dat,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~lfa, ncol=2) +
				  geom_segment(aes(x='2008', y=lt.meanq(),xend='2015',yend=lt.mean,group=lfa), colour="cornflowerblue", data=subset(long.term.mean,lfa<"34")) + geom_line(data=logs.0814,colour="black") + 
				  geom_point(data=logs.0814,colour="black") +scale_y_continuous(limits=c(0,2.2), breaks=c(0,1,2)) + scale_x_discrete(breaks=seq(1980,2015,5),
				  labels=c('1981','1986','1991','1996','2001','2006','2011','2016')) + 
				  theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") + xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
				  p1
	dev.off()
	
	# Landings
	LandingsUpdate<-lobster.db('annual.landings')
	LR = as.data.frame(reshape(LandingsUpdate[,c("YR","LFA27",  "LFA28",  "LFA29",  "LFA30",  "LFA31",  "LFA32",  "LFA33")],
		varying=c("LFA27",  "LFA28",  "LFA29",  "LFA30",  "LFA31",  "LFA32",  "LFA33"),v.names='Landings',timevar='LFA',times=c("LFA27",  "LFA28",  "LFA29",  "LFA30", "LFA31",  "LFA32",  "LFA33"), direction='long'))
	 attr(LR,'reshapeLong') <- NULL
	 LR$LFA1 <- recode(LR$LFA,c("'LFA28'='LFA28.32';'LFA29'='LFA28.32';'LFA30'='LFA28.32'; 'LFA31'='LFA28.32';'LFA32'='LFA28.32'"))

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
					Gulf27 <- data.frame(YR = 2004:2015,LFA1='27G', Landings= c(115,	117,	118,	110,	138,	104,146.2,149.3,161.4,208.8,174.3,158.4)) 
					L27 = rbind(L27,Gulf27)
					names(L27)[2] <- 'LFA'

				p1<-ggplot(L27,aes(x=YR,y=Landings,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
							   scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) + 
							   ggtitle("LFA 27") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
							   panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) +
							   xlab('') + ylab('Landings (t)') + scale_fill_manual(breaks=factor('LFA'),values=c("#D55E00","#000000")) + geom_segment(aes(x=1947,y=1629,xend=2016,yend=1629),
							   col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=814,xend=2016,yend=814),col="#009E73",linetype="solid", size=1.0) +
							   geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0) + 
							   geom_text(aes(1947,3500,label = 'Gulf Landings'),col="#D55E00",size=4,hjust=0)
							 
				 
					 L28.32 <- subset(L,LFA1=='LFA28.32')
				 	 names(L28.32)[1] <- 'LFA'
				 p2<-ggplot(L28.32,aes(x=YR,y=Landings,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
							   scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) +
							   ggtitle("LFA 28-32") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
							   panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) + 
							   xlab('') + ylab('Landings (t)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=691,xend=2016,yend=691), 
							   col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=346,xend=2016,yend=346), col="#009E73",linetype="solid", size=1.0) +
							   geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0)
				 

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

				 p3<-ggplot(LU,aes(x=YR,y=Landings,colour=TYPE, group=TYPE)) + 
							   geom_bar(data=subset(LU,TYPE=='Annual'),fill='black',colour='black',stat='identity', width=1.0) +
							   geom_bar(data=subset(LU,TYPE=='Seasonal'),fill='darkgray',colour='darkgray', stat='identity',width=1.0) +
							   scale_y_continuous(limits=c(0,10200),breaks=seq(0, 10200, 1000)) + scale_x_continuous(breaks=seq(1947, 2016, 4)) + ggtitle("LFA 33") +
							   theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
							         panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
							         axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)') +
							   geom_segment(aes(x=1947,y=1794,xend=2016,yend=1794), col="#0072B2",linetype="dashed", size=1.0) +
							   geom_segment(aes(x=1947,y=897,xend=2016,yend=897),col="#009E73",linetype="solid", size=1.0) + 
							   geom_text(aes(1947,7000,label = '- - - USR (80%)'),col="#0072B2",size=4, hjust=0) + geom_text(aes(1947,6300,label = '----- LRP (40%)'),col="#009E73",size=4,hjust=0)
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
