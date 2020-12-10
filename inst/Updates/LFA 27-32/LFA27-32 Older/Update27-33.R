####### Update 27-33: Feb 2015



#---------------------------------------------------------------------------Plots for Update
	require(gridExtra)
	require(plyr)
	require(ggplot2)

    # run script for modelling catch rates in FSRS traps
    loadfunctions( "lobster", functionname="FSRScpue.r") 

	
	# FSRS CPUE
	shorts<-read.csv(file.path( project.datadirectory("lobster"), "data","products","FSRSmodelresultsSHORT.csv"))
	legals<-read.csv(file.path( project.datadirectory("lobster"), "data","products","FSRSmodelresultsLEGAL.csv"))


	# FSRS Shorts
	pdf(file.path( project.datadirectory("lobster"),"figures","FSRSshorts.pdf"),8, 10)

	p <- ggplot()
	p <- p + geom_point(data = shorts, aes(y = mu, x = YEAR), shape = 16, size = 3)
	p <- p + xlab("Year") + ylab("Lobsters / Trap")
	p <- p + theme(text = element_text(size=15)) + theme_bw()
	p <- p + geom_line(data = shorts, aes(x = YEAR, y = mu), colour = "black")
	p <- p + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	p <- p + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	p

	sp <- ggplot()
	sp <- sp + geom_point(data = shorts, aes(y = mu, x = YEAR), shape = 16, size = 3)
	sp <- sp + xlab("Year") + ylab("Lobsters / Trap")
	sp <- sp + theme(text = element_text(size=15)) + theme_bw()
	sp <- sp + geom_line(data = shorts, aes(x = YEAR, y = mu), colour = "black")
	sp <- sp + geom_ribbon(data = shorts, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	sp <- sp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	sp

	dev.off()

	#Individual Shorts Plots for Presentations
	area<-unique(shorts$Area)
	for(a in (area)){
		shorts.fsrs<-subset(shorts,Area==a)
		p <- ggplot()
	    p <- p + geom_point(data = shorts.fsrs, aes(y = mu, x = YEAR), shape = 16, size = 3)
	    p <- p + xlab("Year") + ylab("Lobsters / Trap")
	    p <- p + theme(text = element_text(size=15)) + theme_bw()
	    p <- p + geom_line(data = shorts.fsrs, aes(x = YEAR, y = mu), colour = "black")
	    p <- p + geom_ribbon(data = shorts.fsrs, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	    p <- p + facet_wrap( ~Area, ncol=2,scales = "fixed")
	    p
		
		pdf(file.path(project.datadirectory('lobster'),"figures","FSRS",paste0("FSRSshorts",a,".pdf")))
		print(p)
		
		dev.off()

	}
	
	# Legals
	pdf(file.path( project.datadirectory("lobster"),"figures","FSRSlegals.pdf"),8, 10)

	lp <- ggplot()
	lp <- lp + geom_point(data = legals, aes(y = mu, x = YEAR), shape = 16, size = 3)
	lp <- lp + xlab("Year") + ylab("Lobsters / Trap")
	lp <- lp + theme(text = element_text(size=15)) + theme_bw()
	lp <- lp + geom_line(data = legals, aes(x = YEAR, y = mu), colour = "black")
	lp <- lp + geom_ribbon(data = legals, aes(x = YEAR, ymax = ub, ymin = lb ), alpha = 0.5)
	lp <- lp + facet_wrap(  ~Area, ncol=2,scales = "fixed")
	lp

	dev.off()


	# Logs CPUE
	LOGcpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE_LFA.csv"))
	cpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","CommercialCPUE.csv"))
	vollog.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","VolLogsCPUE_LFA.csv"))

	long.term.mean<-ddply(subset(cpue.dat,year<2015&year>2007), .( lfa,subarea), summarize, lt.mean=mean(cpue))

	#Plot in update displays all commercial and voluntary log CPUEs. Long term mean CPUE from commercial logs (2008-2014) is also included.
	pdf('Commercial.CPUE.LFA27-33.pdf',width=8,height=10)

	ggplot(vollog.dat,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~subarea, ncol=2) +
	  geom_segment(aes(x='2008', y=lt.mean,xend='2014',yend=lt.mean), colour="cornflowerblue", data=subset(long.term.mean,subarea!="NA")) + geom_line(data=subset(cpue.dat,subarea!="NA"),colour="black") + 
	  geom_point(data=subset(cpue.dat,subarea!="NA"),colour="black") +scale_y_continuous(limits=c(0,2.2), breaks=c(0,1,2)) + scale_x_discrete(breaks=seq(1981,2015,4),
	  labels=c('1981','1985','1989','1993','1997','2001','2005','2009','2013')) + 
	  theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") + xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
	dev.off()

	#Individual Plots for Presentations
	lfa<-unique(vollog.dat$lfa)
	for(a in (lfa)){
			vollog.lfa<-subset(vollog.dat,lfa==a) #lfa 31A and 31B have to be added to file
			lt.mean.lfa<-subset(long.term.mean,lfa==a)
	        cpue.lfa<-subset(cpue.dat,lfa==a)

		p<-ggplot(vollog.lfa,aes(x=as.factor(year),y=cpue, group=lfa, colour="red")) + geom_line () + facet_wrap(~subarea, ncol=1) +
  		geom_segment(aes(x='2008', y=lt.mean,xend='2014',yend=lt.mean), colour="cornflowerblue", data=lt.mean.lfa) + geom_line(data=cpue.lfa,colour="black") + 
  		geom_point(data=cpue.lfa,colour="black") + scale_x_discrete(breaks=seq(1985,2015,4),
  		labels=c('1985','1989','1993','1997','2001','2005','2009','2013')) + theme_bw() + theme(axis.text.x=element_text(size=10.0),strip.text=element_text(size=12.0),legend.position="none") + xlab("Year") + ylab("CPUE (Kg/Trap Haul)")
		
		pdf(file.path(project.datadirectory('lobster'),"figures","Commercial CPUE",paste0("CommercialCPUE",a,".pdf")))
		print(p)
		
		dev.off()

	}

	
	# Landings, including Gulf landings portion, USR and LRP. Note that LFA33 USR and LRP are calculated from last data export and differs
	# from reference point document
	LandingsUpdate<-read.csv(file.path( project.datadirectory("lobster"), "data","Landings.27-33.1947.2015.csv"),header=T)

	pdf('Commercial.Landings.LFA27-33.pdf',width=8,height=10)

    cbPalette <- c("#000000","#D55E00")
    p1<-ggplot(subset(LandingsUpdate,LFA<28),aes(x=YR,y=Landings.tons,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
     scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) + 
     ggtitle("LFA 27") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
     panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) +
     xlab('') + ylab('Landings (t)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=1629,xend=2015,yend=1629),
     col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=814,xend=2015,yend=814),col="#009E73",linetype="solid", size=1.0) +
     geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0) + 
     geom_text(aes(1947,3500,label = 'Gulf Landings'),col="#D55E00",size=4,hjust=0)
 
    p2<-ggplot(subset(LandingsUpdate,LFA==28.32),aes(x=YR,y=Landings.tons,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
     scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) +
     ggtitle("LFA 28-32") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
     panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) + 
     xlab('') + ylab('Landings (t)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=691,xend=2015,yend=691), 
     col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=346,xend=2015,yend=346), col="#009E73",linetype="solid", size=1.0) +
     geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0)
 
    p3<-ggplot(subset(LandingsUpdate,LFA==33),aes(x=YR,y=Landings.tons,colour=TYPE, group=TYPE)) + 
     geom_bar(data=subset(LandingsUpdate,c(LFA==33&TYPE=='Annual'&YR<1976)),fill='black',colour='black',stat='identity', width=1.0) +
     geom_bar(data=subset(LandingsUpdate,c(LFA=33&TYPE=='Seasonal')),fill='darkgray',colour='darkgray', stat='identity',width=1.0) +
     scale_y_continuous(limits=c(0,8000),breaks=seq(0, 8000, 1000)) + scale_x_continuous(breaks=seq(1947, 2015, 4)) + ggtitle("LFA 33") +
     theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
         panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
         axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)') +
     geom_segment(aes(x=1947,y=1794,xend=2015,yend=1794), col="#0072B2",linetype="dashed", size=1.0) +
     geom_segment(aes(x=1947,y=897,xend=2015,yend=897),col="#009E73",linetype="solid", size=1.0) + 
     geom_text(aes(1947,7000,label = '- - - USR (80%)'),col="#0072B2",size=4, hjust=0) + geom_text(aes(1947,6300,label = '----- LRP (40%)'),col="#009E73",size=4,hjust=0)
    grid.arrange(p1,p2,p3,ncol=1) 

 dev.off()


	
	# Landings, including Gulf landings portion, USR and LRP. Note that LFA33 USR and LRP are calculated from last data export and differs
	# from reference point document
	LandingsUpdate<-read.csv(file.path( project.datadirectory("lobster"), "data","Landings.27-33.1947.2015.csv"),header=T)

	pdf('Commercial.Landings.LFA27-33.pdf',width=8,height=10)

    cbPalette <- c("#000000","#D55E00")
    p1<-ggplot(subset(LandingsUpdate,LFA<28),aes(x=YR,y=Landings.tons,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
     scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) + 
     ggtitle("LFA 27") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
     panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) +
     xlab('') + ylab('Landings (t)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=1629,xend=2015,yend=1629),
     col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=814,xend=2015,yend=814),col="#009E73",linetype="solid", size=1.0) +
     geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0) + 
     geom_text(aes(1947,3500,label = 'Gulf Landings'),col="#D55E00",size=4,hjust=0)
 
    p2<-ggplot(subset(LandingsUpdate,LFA==28.32),aes(x=YR,y=Landings.tons,fill=factor(LFA))) + geom_bar(stat='identity', width=1.0) +
     scale_y_continuous(limits=c(0,5000),breaks=seq(0, 5000, 1000))  + scale_x_continuous(breaks=seq(1947, 2015, 4)) +
     ggtitle("LFA 28-32") + theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
     panel.border=element_rect(fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),axis.title.y= element_text(size=15)) + 
     xlab('') + ylab('Landings (t)') + scale_fill_manual(breaks=factor('LFA'),values=cbPalette) + geom_segment(aes(x=1947,y=691,xend=2015,yend=691), 
     col="#0072B2",linetype="dashed", size=1.0) + geom_segment(aes(x=1947,y=346,xend=2015,yend=346), col="#009E73",linetype="solid", size=1.0) +
     geom_text(aes(1947,4500,label = '- - - USR (80%)'),col="#0072B2",size=4,hjust=0) + geom_text(aes(1947,4000,label = '------ LRP (40%)'),col="#009E73",size=4,hjust=0)
 
    p3<-ggplot(subset(LandingsUpdate,LFA==33),aes(x=YR,y=Landings.tons,colour=TYPE, group=TYPE)) + 
     geom_bar(data=subset(LandingsUpdate,c(LFA==33&TYPE=='Annual'&YR<1976)),fill='black',colour='black',stat='identity', width=1.0) +
     geom_bar(data=subset(LandingsUpdate,c(LFA=33&TYPE=='Seasonal')),fill='darkgray',colour='darkgray', stat='identity',width=1.0) +
     scale_y_continuous(limits=c(0,8000),breaks=seq(0, 8000, 1000)) + scale_x_continuous(breaks=seq(1947, 2015, 4)) + ggtitle("LFA 33") +
     theme(axis.text.x=element_text(size=10, colour='black'),panel.background=element_rect(colour='black'),
         panel.border=element_rect(,fill=NA,colour='black'),axis.text.y=element_text(size=10, colour='black'),
         axis.title.y= element_text(size=15)) + xlab('') + ylab('Landings (t)') +
     geom_segment(aes(x=1947,y=1794,xend=2015,yend=1794), col="#0072B2",linetype="dashed", size=1.0) +
     geom_segment(aes(x=1947,y=897,xend=2015,yend=897),col="#009E73",linetype="solid", size=1.0) + 
     geom_text(aes(1947,7000,label = '- - - USR (80%)'),col="#0072B2",size=4, hjust=0) + geom_text(aes(1947,6300,label = '----- LRP (40%)'),col="#009E73",size=4,hjust=0)
    grid.arrange(p1,p2,p3,ncol=1) 

 dev.off()

	# for LFA33 presentation
	FSRScpue.dat<-read.csv(file.path( project.datadirectory("lobster"), "data","FSRScpue.csv"))

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

	subareas<-read.csv(file.path( project.datadirectory("lobster"), "data","LFA2733subarea.csv"))
	polyprop<-merge(subset(subareas,LFA%in%c(27,33)),data.frame(LFA=c(27,27,33,33),subarea=c("27 North","27 South","33 West","33 East"),lty=2),all=T)
	names(polyprop)[1:3]<-c("PID","label","SID")
	LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAgridPolys.csv"))

	pdf(file.path( project.datadirectory("lobster"),"R","LFA33map.pdf"),8, 8)
	LobsterMap('33',poly.lst=list(subset(LFAgrid,PID==33),polyprop33),boundaries='none',isobaths=c(seq(50,450,50),seq(500,1000,100)),bathcol=rgb(0,0,1,0.3))
	legend('bottomright',c("West","East"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
	dev.off()

	LFAlabels<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","LFAlabels.csv"))

	lab.lst=list(LFAlabels,data.frame(PID=c(27:33,311,312,271,272,331,332),cex=c(rep(0.8,9),rep(0.6,4)),font=c(rep(2,9),rep(3,4))),col='black')

	LobsterMap(xlim=c(-67,-57.5),ylim=c(42.5,48),addsubareas=T,labels=lab.lst,isobath=c(100,200,300,400,500),land.col='grey90',mapRes="MR")
