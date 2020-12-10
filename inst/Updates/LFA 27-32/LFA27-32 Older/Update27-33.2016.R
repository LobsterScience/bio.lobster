####### Update 27-33 2016:



#---------------------------------------------------------------------------Plots for Update
	require(lattice)

    # run script for modelling catch rates in FSRS traps
    loadfunctions( "lobster", functionname="FSRScpue.r") 


	# Logs CPUE
	LOGcpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE_LFA.csv"))
	cpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE.csv"))
	vollog.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","VolLogsCPUE_LFA.csv"))


	#Manon's Plot version
	#Plot in update displays all commercial and voluntary log CPUEs. Long term mean CPUE from commercial logs (2008-2014) is also included.
	logs.0814<-subset(cpue.dat,year<2015&year>2007)
	long.term.mean<-ddply(logs.0814, .(year, subarea), summarize, lt.mean=mean(cpue))
	require(ggplot2)

  pdf('Commercial.CPUE.LFA27-33.pdf',width=8,height=10)
	ggplot(subset(cpue.dat,lfa<"34"),aes(x=as.factor(year),y=cpue, group=lfa)) + geom_point() + geom_line () + facet_wrap(~subarea, ncol=2) +
	  geom_hline(aes(yintercept=lt.mean), colour="blue4", data=subset(long.term.mean,lfa<"34")) + geom_line(vollog.dat,aes(x=year,y=cpue)) +
	  scale_y_continuous(limits=c(0,5.2), breaks=c(0,6,2)) + theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") +
	  xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
	dev.off()

	pdf('Commercial.CPUE.Vollog.LFA27-33.pdf',width=8,height=10)
	ggplot(vollog.dat,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~subarea, ncol=2) +
	  geom_segment(aes(x='2008', y=lt.mean,xend='2014',yend=lt.mean), colour="cornflowerblue", data=subset(long.term.mean,lfa<"34")) + geom_line(data=subset(cpue.dat,lfa<34),colour="black") + 
	  geom_point(data=subset(cpue.dat,lfa<34),colour="black") +scale_y_continuous(limits=c(0,2.2), breaks=c(0,1,2)) + scale_x_discrete(breaks=seq(1981,2015,4),
	  labels=c('1981','1985','1989','1993','1997','2001','2005','2009','2013')) + 
	  theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") + xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
	dev.off()
	
	# Landings
	LandingsUpdate<-lobster.db('annual.landings')
	LandingsUpdate<-lobster.db('seasonal.landings')

	x11(6.5,8)
	bwd<-8
	par(mfrow=c(3,1),las=1,mar=c(2,2,2,2),omi=c(0.2,0.8,0.2,0.2))
	plot(Landings.tons~YR,subset(LandingsUpdate,LFA==27),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 27",ylim=c(0,6000))
	axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)
	plot(Landings.tons~YR,subset(LandingsUpdate,LFA==28.32),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 28-32",ylim=c(0,6000))
	axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)
	plot(Landings.tons~YR,subset(LandingsUpdate,LFA==33),type='h',lwd=bwd,lend=3,xlab='',ylab='',main="LFA 33",ylim=c(0,6000))
	axis(1,at=subset(LandingsUpdate,LFA==27)$YR,lab=F,tck=-0.01)

	mtext("Landings (mt)",2,3,outer=T,las=0)
	
	# Manon's Plot version - adds Gulf landings portion, USR and LRP. Note that LFA33 USR and LRP are calculated from last data export and differs
	# from reference point document
	require(ggplot2)
	require(gridExtra)
	
	pdf('Commercial.Landings.LFA27-33.pdf',width=8,height=10)
  cbPalette <- c("#000000","#D55E00")
 p1<-ggplot(subset(LandingsUpdate,LFA<28),aes(x=YR,y=Landings.tons,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
   scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) + 
   ggtitle("LFA 27") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
   panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) +
   xlab('') + ylab('Landings (MT)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=1629,xend=2015,yend=1629),
   col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=814,xend=2015,yend=814),col="#009E73",linetype="solid", size=1.0) +
   geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0) + 
   geom_text(aes(1947,3500,label = 'Gulf Landings'),col="#D55E00",size=4,hjust=0)
 
 p2<-ggplot(subset(LandingsUpdate,LFA==28.32),aes(x=YR,y=Landings.tons,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
   scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) +
   ggtitle("LFA 28-32") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
   panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) + 
   xlab('') + ylab('Landings (MT)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=691,xend=2015,yend=691), 
   col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=346,xend=2015,yend=346), col="#009E73",linetype="solid", size=1.0) +
   geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0)
 
 p3<-ggplot(subset(LandingsUpdate,LFA==33),aes(x=YR,y=Landings.tons,colour=TYPE, group=TYPE)) + 
   geom_bar(data=subset(LandingsUpdate,c(LFA==33&TYPE=='Annual'&YR<1976)),fill='black',colour='black',stat='identity', width=1.0) +
   geom_bar(data=subset(LandingsUpdate,c(LFA=33&TYPE=='Seasonal')),fill='darkgray',colour='darkgray', stat='identity',width=1.0) +
   scale_y_continuous(limits=c(0,8000),breaks=seq(0, 8000, 1000)) + scale_x_continuous(breaks=seq(1947, 2015, 4)) + ggtitle("LFA 33") +
   theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
         panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
         axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (MT)') +
   geom_segment(aes(x=1947,y=1794,xend=2015,yend=1794), col="#0072B2",linetype="dashed", size=1.0) +
   geom_segment(aes(x=1947,y=897,xend=2015,yend=897),col="#009E73",linetype="solid", size=1.0) + 
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
