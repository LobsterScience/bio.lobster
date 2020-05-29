#bycatch presentation Feb 2018

  
	p = bio.lobster::load.environment()


    p$syr = 2005
    p$yrs = p$syr:p$current.assessment.year

    figdir = file.path(project.datadirectory("bio.lobster"),"figures","Bycatch")
    dir.create(figdir)

    
	## Fishery Footprint - Mean Pots Hauled 

	   logsInSeason<-lobster.db('process.logs')
logsInSeason$mm= months(logsInSeason$DATE_FISHED)

logsInSeason$Ses = ifelse(logsInSeason$mm %in% c('October','November','December'),1,ifelse(logsInSeason$mm %in% c('January','February'),2,ifelse(logsInSeason$mm %in% c('March','April','May'),3,ifelse(logsInSeason$mm %in% c('June','July'),4,0))))
logsInSeason = subset(logsInSeason,SYEAR %in% 2013:2017)
	logsInSeason$ID = paste(logsInSeason$LICENCE_ID, logsInSeason$DATE_FISHED,sep="-")
	
	dd = aggregate(ID~LFA+GRID_NUM+Ses,data=logsInSeason,FUN=function(x) length(unique(x)))

	lfas = c('33','34','35','36','38')
	ses = c(1,2,3,4)
	nses = c('Fall','Winter','Spring','Summer')
	quants = c(.1,.2,.3,.4,.5,.6,.7,.8,.9)
	for(i in 1:length(lfas)){
		g = subset(dd,LFA==lfas[i])
		potLevels = as.numeric(round(quantile(g$ID,quants),0))
		for(j in 1:length(ses)){
			if(any(ses[j] %in% g$Ses)){
	potgrids = lobGridPlot(subset(g,Ses == ses[j],select=c("LFA","GRID_NUM","ID")),FUN=mean,lvls=potLevels) 
	pdf(file.path(figdir,paste0("FishFootTrips", lfas[i],nses[j],".pdf")))
	LobsterMap(lfas[i],poly.lst=potgrids)
	SpatialHub::contLegend('bottomright',lvls=potgrids$lvls,Cont.data=potgrids,title="Mean Number of Trips",inset=0.02,cex=0.8,bg='white')
	title(paste('LFA',lfas[i],"-",nses[j]))
	dev.off()
		}
	}
	}
	 
	
	

	lfas = c('27','28','29','30','31A','31B','32')
	ses = c(3,4)
	nses = c('Spring','Summer')
	quants = c(.1,.2,.3,.4,.5,.6,.7,.8,.9)
	for(i in 1:length(lfas)){
		g = subset(dd,LFA==lfas[i])
		potLevels = as.numeric(round(quantile(g$ID,quants),0))
		for(j in 1:length(ses)){
			if(any(ses[j] %in% g$Ses)){
	potgrids = lobGridPlot(subset(g,Ses == ses[j],select=c("LFA","GRID_NUM","ID")),FUN=mean,lvls=potLevels) 
	pdf(file.path(figdir,paste0("FishFootTrips", lfas[i],nses[j],".pdf")))
	if(lfas[i]=='31A')lfas[i]='31a'
	if(lfas[i]=='31B')lfas[i]='31b'
	LobsterMap(lfas[i],poly.lst=potgrids)
	SpatialHub::contLegend('bottomright',lvls=potgrids$lvls,Cont.data=potgrids,title="Mean Number of Trips",inset=0.02,cex=0.8,bg='white')
	title(paste('LFA',lfas[i],"-",nses[j]))
	dev.off()
		}
	}
	}
	 
	
	
