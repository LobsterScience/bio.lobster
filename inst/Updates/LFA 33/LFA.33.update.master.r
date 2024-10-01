# LFA 33 Update Script

	p = bio.lobster::load.environment()
	require(bio.utilities)
	require(ggplot2)
	la()
	p$yrs <- NULL #ensuring empty variable
	
	#la()
#If running script after the fishery year, run this line
#p$current.assessment.year=p$current.assessment.year-1


	    # define place for figures to go
	    figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA33",p$current.assessment.year))
	    dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
	    p$lfas = c("33") # specify lfas for data summary
	    p$subareas = c("33W", "33E") # specify lfas for data summary
	    
	    figdir = file.path(project.datadirectory("bio.lobster","assessments","Updates","LFA33",p$current.assessment.year))
	    dir.create( figdir, recursive = TRUE, showWarnings = FALSE )
	   
	    #If you only want to update logs for the last two years, run this:
	    #p$yr=p$current.assessment.year

	    # update data through ROracle
	    NewDataPull =F
	    #NewDataPull =T

	    if(NewDataPull){
	      lobster.db('fsrs.redo')
	      lobster.db('logs.redo')
	      lobster.db('annual.landings.redo')
	      lobster.db('seasonal.landings.redo')
	      #lobster.db('vlog.redo')
	      logs=lobster.db('process.logs.redo')
	      per.rec= lobster.db("percent_reporting")
	    }

#Run a report of missing vs received logs and save a csv copy
	    
fl.name=paste("percent_logs_reported", Sys.Date(),"csv", sep=".")
per.rec=per.rec[order(per.rec$YEARMTH),]
write.csv(per.rec, file=paste0(figdir,"/",fl.name),na="", row.names=F)

# Map ################

		#x11(width=5, height=5)
	#	LobsterMap('33')
	#	savePlot(file.path(figdir,'LFA33map.png'),type='png')

		#or

		png(filename=file.path(figdir, "MapLFA33.png"),width=5, height=5, units = "in", res = 800)
		LobsterMap('33')
		dev.off()

# CPUE ###############


	#
		logs=lobster.db("process.logs")

		#Choose one to redo or not Add TempSkip=T to not model CPUE with Temps
		CPUE.data<-CPUEModelData2(p,redo=T)
		#CPUE.data<-CPUEModelData2(p,redo=F)
		
		
		cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(p$current.assessment.year),graphic='R')$annual.data
	
		crd = subset(cpueData,LFA==33,c("YEAR","CPUE"))
		crd = crd[is.finite(crd$CPUE),]
		mu = median(crd$CPUE[crd$YEAR %in% c(1990:2016)])
		usr = mu * 0.8
		lrp = mu * 0.4


	

		png(filename=file.path(figdir, "CPUE_only.png"),width=8, height=5.5, units = "in", res = 800)
    		par(mar=c(2.0,5.5,2.0,3.0))
    		xlim=c(1990,max(crd$YEAR))
    		plot(crd[,1],crd[,2],xlab='Year',ylab='CPUE (kg/TH)',type='b',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
    		points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
		dev.off()

    png(filename=file.path(figdir, "CPUE_LFA33.png"),width=8, height=5.5, units = "in", res = 800)
        par(mar=c(4.0,5.5,2.0,3.0))
        xlim=c(1990,max(crd$YEAR))
        plot(crd[,1],crd[,2],xlab='Year',ylab='CPUE (kg/TH)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
        points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
        running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
        crd=merge(crd,running.median,all=T)
        lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
        abline(h=usr,col='green',lwd=2,lty=2)
        abline(h=lrp,col='red',lwd=2,lty=3)
    dev.off()
   
    write.csv(crd,file.path(figdir,file='CatchRateRefs33.csv'))
    
    #French Version of Figure:
    png(filename=file.path(figdir, "CPUE_LFA33.French.png"),width=8, height=5.5, units = "in", res = 800)
    par(mar=c(4.0,5.5,2.0,3.0))
    xlim=c(1990,max(crd$YEAR))
    plot(crd[,1],crd[,2],xlab='Annee',ylab='CPUE (kg/casier leve)',type='p',pch=16,xlim=xlim, ylim=c(0, 1.05*max(crd$CPUE)))
    points(max(crd$YEAR), crd$CPUE[which(crd$YEAR==max(crd$YEAR))], pch=17, col='red', cex=1.2)
    running.median = with(rmed(crd[,1],crd[,2]),data.frame(YEAR=yr,running.median=x))
    crd=merge(crd,running.median,all=T)
    lines(crd[,1],crd$running.median,col='blue',lty=1,lwd=2)
    abline(h=usr,col='green',lwd=2,lty=2)
    abline(h=lrp,col='red',lwd=2,lty=3)
    dev.off()
    
    # plot
    #x11(width=8,height=5)
    CatchRatePlot(data = crd ,usr = usr,lrp=lrp,lfa = 33,fd=figdir, save=F)

    # Plots unbiased annual CPUE for all LFAs in Maritimes region
    # Good for context in presentations at AC
    
    a = lobster.db('process.logs')
    a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 
    
    aa = split(a,f=list(a$LFA,a$SYEAR))
    cpue.lst<-list()
    m=0
    #by time
    for(i in 1:length(aa)){
      tmp<-aa[[i]]
      tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
      names(tmp)<-c('time','catch','effort')
      tmp$date<-as.Date(tmp$time)
      first.day<-min(tmp$date)
      tmp$time<-julian(tmp$date,origin=first.day-1)
      tmp$time = ceiling(tmp$time/7) #convert to week of season
      if(nrow(tmp)>5){
        m=m+1
        g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
        g$lfa=unique(aa[[i]]$LFA)
        g$yr = unique(aa[[i]]$SYEAR)
        g = t(g)[,2]
        cpue.lst[[m]] <- g
      }
    }
    cc =as.data.frame(do.call(rbind,cpue.lst))
    cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
    cc = cc[order(cc$lfa,cc$yr),]
    cc$yr = as.numeric(cc$yr)
    cc$fyr = as.factor(cc$yr)
    last_bar_color="black"
      point_colors <- ifelse(cc$yr <max(cc$yr), last_bar_color, "orange")
      cc1 = cc
      
      png(filename=file.path(figdir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
      ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+
        geom_smooth(se=FALSE)+geom_point(data=cc1,aes(x=yr,y=CPUE,colour=fyr))+facet_wrap(~lfa,scales='free_y')+
        scale_colour_manual(values = point_colors)+theme(legend.position = 'none')+
        labs(y= "CPUE", x = "Year")
      dev.off()
      
      
      # Plots unbiased annual CPUE for all LFAs in Maritimes region
      # Good for context in presentations at AC
      
      a = lobster.db('process.logs')
      a = subset(a,SYEAR %in% 2004:p$current.assessment.year) 
      
      aa = split(a,f=list(a$LFA,a$SYEAR))
      cpue.lst<-list()
      m=0
      #by time
      for(i in 1:length(aa)){
        tmp<-aa[[i]]
        tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
        names(tmp)<-c('time','catch','effort')
        tmp$date<-as.Date(tmp$time)
        first.day<-min(tmp$date)
        tmp$time<-julian(tmp$date,origin=first.day-1)
        tmp$time = ceiling(tmp$time/7) #convert to week of season
        if(nrow(tmp)>5){
          m=m+1
          g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
          g$lfa=unique(aa[[i]]$LFA)
          g$yr = unique(aa[[i]]$SYEAR)
          g = t(g)[,2]
          cpue.lst[[m]] <- g
        }
      }
      cc =as.data.frame(do.call(rbind,cpue.lst))
      cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
      cc = cc[order(cc$lfa,cc$yr),]
      cc$yr = as.numeric(cc$yr)
      cc$fyr = as.factor(cc$yr)
      last_bar_color="black"
        point_colors <- ifelse(cc$yr <max(cc$yr), last_bar_color, "orange")
        cc1 = cc
        
        png(filename=file.path(figdir, "all_lfas_cpue.png"),width=8, height=5.5, units = "in", res = 800)
        ggplot(cc,aes(x=yr,y=CPUE))+geom_point()+
          geom_smooth(se=FALSE)+geom_point(data=cc1,aes(x=yr,y=CPUE,colour=fyr))+facet_wrap(~lfa,scales='free_y')+
          scale_colour_manual(values = point_colors)+theme(legend.position = 'none')+
          labs(y= "CPUE", x = "Year")
        dev.off()
        
        
        
        #Unbiased cpue patterns by week of season
      #Will need to modify AC presentation to include these contextual CPUE figures
      #-----------------------------------------
      
      a = lobster.db('process.logs')
      a = subset(a,SYEAR %in% 2004:2004:p$current.assessment.year & LFA %in% p$lfas)
      strt.yr=p$current.assessment.year-11
      a = subset(a,SYEAR %in% strt.yr:strt.yr:p$current.assessment.year & LFA %in% p$lfas) 
      
      aa = split(a,f=list(a$LFA,a$SYEAR))
      aa = rm.from.list(aa)
      cpue.lst<-list()
      
      
      aa = split(a,f=list(a$LFA,a$SYEAR))
      cpue.lst<-list()
      m=0
      #annual
      for(i in 1:length(aa)){
        tmp<-aa[[i]]
        tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
        names(tmp)<-c('time','catch','effort')
        tmp$date<-as.Date(tmp$time)
        first.day<-min(tmp$date)
        tmp$time<-julian(tmp$date,origin=first.day-1)
        tmp$time = ceiling(tmp$time/7) #convert to week of season
        if(nrow(tmp)>5){
          m=m+1
          g<-as.data.frame(biasCorrCPUE(tmp,by.time=F))
          g$lfa=unique(aa[[i]]$LFA)
          g$yr = unique(aa[[i]]$SYEAR)
          g = t(g)[,2]
          cpue.lst[[m]] <- g
        }
      }
      cc =as.data.frame(do.call(rbind,cpue.lst))
      cc$CPUE = as.numeric(cc$`biasCorrCPUE(tmp, by.time = F)`)
      cc = cc[order(cc$lfa,cc$yr),]
      cc$yr = as.numeric(cc$yr)
      cc$fyr = as.factor(cc$yr)
      
      cc1 = split(cc,f=cc$lfa)
      
      for(i in 1:length(cc1)){
        cc1[[i]]$mCPUE = as.numeric(with(cc1[[i]],rmed(yr,CPUE))$x)
      }
      
      cc2 = do.call(rbind,cc1)
      
      #ggplot(cc2,aes(x=yr,y=CPUE))+geom_point()+
      #  geom_line(aes(x=yr,y=mCPUE),colour='red',size=1.1)+facet_wrap(~lfa,scales='free_y')+geom_point(data=subset(cc2,yr==2023),aes(x=yr,y=CPUE),colour='orange',shape=16,size=2)
      
      ##by week
      aa = split(a,f=list(a$LFA,a$SYEAR))
      cpue.lst<-list()
      m=0
      
      aa = split(a,f=list(a$LFA,a$SYEAR))
      aa = rm.from.list(aa)
      cpue.lst<-list()
      
      #by time
      for(i in 1:length(aa)){
        tmp<-aa[[i]]
        tmp = tmp[,c('DATE_FISHED','WEIGHT_KG','NUM_OF_TRAPS')]
        names(tmp)<-c('time','catch','effort')
        tmp$date<-as.Date(tmp$time)
        first.day<-min(tmp$date)
        tmp$time<-julian(tmp$date,origin=first.day-1)
        tmp = tmp[order(tmp$time),]
        tmp$time = ceiling(tmp$time/7) #convert to week of season
        g<-as.data.frame(biasCorrCPUE(tmp,by.time=T,min.sample.size = 5))
        g$lfa=unique(aa[[i]]$LFA)
        g$yr = unique(aa[[i]]$SYEAR)
        # g = t(g)[,1]
        cpue.lst[[i]] <- g
      }
      
      cc =as.data.frame(do.call(rbind,cpue.lst))
      
      mean= aggregate(cc, CPUE~yr+lfa,mean )
      
      
     l=p$lfas
        png(filename=file.path(figdir, paste0("weekly_cpue_",l,".png")),width=8, height=5.5, units = "in", res = 800)
        print(
          ggplot(subset(cc,lfa==l),aes(x=t,y=CPUE))+geom_point()+
            geom_smooth(se=F)+facet_wrap(~yr)+
            labs(title =paste0("LFA ",l))+
            labs(y= "CPUE (kg/th)", x = "Week of Season") +
            theme(plot.title = element_text(hjust = 0.5))+
            # stat_summary(fun='mean', geom="line")
            geom_hline(data=mean[mean$lfa==l,], aes(yintercept= CPUE), color='red', linewidth=0.6)
          
        )
        dev.off()
    
      
      
      #######-----------------------------------------


# CCIR ###############
#The following two tables need to be updated to reflect current year:
#C:\bio.data\bio.lobster\data\inputs\ccir.inputs.csv
#C:\bio.data\bio.lobster\data\inputs\MinLegalSize
    
		lobster.db('ccir.redo')
		inp = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_inputs.csv'))
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_groupings.rdata')) #object names Groupings
		load(file.path(project.datadirectory('bio.lobster'),'data','inputs','ccir_seasons.rdata'))

    
		lobster.db('ccir')

		logs = lobster.db('process.logs')


		require(bio.ccir)
		require(rstan)
		#load_all(paste(git.repo,'bio.ccir',sep="/")) # for debugging
		
		#make sure to index year below as appropriate
		#ccir_data = subset(ccir_data,YEAR<=p$current.assessment.year) 
		
		#to only run last three years:
		ccir_data = subset(ccir_data,YEAR=c((p$current.assessment.year-2):(p$current.assessment.year))) #Don't know that this works
		
		dat = ccir_compile_data(x = ccir_data,log.data = logs, area.defns = Groupings[7], size.defns = inp, season.defns = Seasons, sexs = 1.5) #sexs 1.5 means no sex defn

		out.binomial = list()
		attr(out.binomial,'model') <- 'binomial'
		for(i in 33:length(dat)) { #if run breaks, update 1:length(dat) to reflect run# ie.e 16:length(dat)
			print(i)
		  ds = dat[[i]]
			#ds$method = 'binomial'
			x = ccir_stan_run_binomial(dat = ds,save=T)
			out.binomial[[i]] <- ccir_stan_summarize(x)
		}

### If the folder C:\bio.data\bio.lobster\outputs\ccir\summary contains other model runs for different areas (i.e.27-32)
### move these to the appropriate folder within the summary folder (aka hide them)
	
		#load statement below combines ccir summaries if broken runs
		#ensure folder has only model run summaries
		da = file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary') #modify as required

		d = list.files(da,full.names=T)
		d=d[!file.info(d)$isdir]
		#d=setdiff(list.files(da, full.names=T), list.dirs(recursive = FALSE, full.names = FALSE))
		out.binomial = list()
		#ensure folder has only model run summaries!!!!!
		
		for( i in 1:length(d)){
		  load(d[i])
		  out.binomial[[i]] <- out
		}


		out.binomial[[1]]$LFA = "33W"
		out.binomial[[2]]$LFA = "33E"
		ouBin = ccir_collapse_summary(out.binomial)
		attr(ouBin,'model') <- 'binomial'
		#ouBin$Yr = ouBin$Yr +1
		save(ouBin,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','compiledBinomialModels33.rdata'))

		g = unique(ouBin$Grid)
		g = strsplit(g,"\\.")
		o = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[1]]),FUN=sum)
		names(o)[2] = g[[1]][1]
		o2 = aggregate(WEIGHT_KG~SYEAR,data=subset(logs,GRID_NUM %in% g[[2]]),FUN=sum)
		names(o2)[2] = g[[2]][1]
		o = merge(o,o2)
		names(o)[1] = 'Yr'
		oo <- ccir_timeseries_exploitation_plots(ouBin,combined.LFA=T,landings=o)

		save(oo,file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','lfa33','compiledExploitationCCIR33.rdata'))
		load(file=file.path(project.datadirectory('bio.lobster'),'outputs','ccir','summary','lfa33','compiledExploitationCCIR33.rdata'))
		RR75 = max(oo$ERf75[oo$Yr<p$current.assessment.year])#index year if needed

#oo=read.csv(file.path(figdir, "LFA33ccirout.csv"))
	# plot

png(filename=file.path(figdir, "CCIR_LFA33.png"),width=8, height=5, units = "in", res = 800)
ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 33,fd=figdir, save=F)
dev.off()

#French Version
png(filename=file.path(figdir, "CCIR_LFA33.French.png"),width=8, height=5, units = "in", res = 800)
ExploitationRatePlots(data = oo[,c("Yr","ERfm","ERfl","ERfu")],lrp=RR75,lfa = 33,fd=figdir, save=F, French=T)
dev.off()



# FSRS #############

		FSRSvesday<-FSRSModelData()

		mdata = subset(FSRSvesday,LFA==33&SYEAR<2025) #index year

		FSRSModelResultsLegal=FSRSmodel(mdata,lfa=33, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=33, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)
		FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=33, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=T,ptraps=1000)

		#FSRSModelResultsLegal=FSRSmodel(mdata,lfa=33, response="LEGALS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		legals = FSRSModelResultsLegal$pData
		legals$Area = 33

		#FSRSModelResultsRecruit=FSRSmodel(mdata,lfa=33, response="RECRUITS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		recruit =  FSRSModelResultsRecruit$pData
		recruit$Area = 33

		#FSRSModelShortsRecruit=FSRSmodel(mdata,lfa=33, response="SHORTS",interaction=F,type="bayesian",iter=5000,redo=F,ptraps=1000)
		shorts =  FSRSModelShortsRecruit$pData
		shorts$Area = 33

 	save(list=c("shorts","legals","recruit"),file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators33.rdata"))
  
 	load(file=file.path(project.datadirectory("bio.lobster"),"outputs","fsrsModelIndicators33.rdata"))

	# plot
	#x11(width=8,height=7)
 	png(filename=file.path(figdir, "FSRS.legals.recruits.png"),width=8, height=5, units = "in", res = 800)
	FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 33,fd=figdir,title='', save=F)
  dev.off()

png(filename=file.path(figdir, "FSRS.legals.recruits.French.png"),width=8, height=5, units = "in", res = 800)
FSRSCatchRatePlot(recruits = recruit[,c("YEAR","median","lb","ub")],legals=legals[,c("YEAR","median","lb","ub")],lfa = 33,fd=figdir,title='',French=T, save=F)
dev.off()


# Landings and Effort ############

land = lobster.db('seasonal.landings')
    
    
#if running this section without having done the CPUE analysis during the same session, run 2 lines below 
#CPUE.data<-CPUEModelData(p,redo=F)
#cpueData=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:max(CPUE.data$SYEAR),graphic='R')$annual.data

		land$YEAR = as.numeric(substr(land$SYEAR,6,9))
		land$LANDINGS = land$LFA33
		fishData = merge(cpueData,land[,c("YEAR","LANDINGS")])
		fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE
		
		

		# plot Landings

		png(filename=file.path(figdir, "Landings_LFA33.png"),width=8, height=5, units = "in", res = 800)
    		par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
    		plot(fishData$YEAR,fishData$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main="LFA 33",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
    		lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
    dev.off()

	# plot Landings / Effort Together

		png(filename=file.path(figdir, "Landings_Effort_LFA33.png"),width=8, height=5, units = "in", res = 800)
    		#FisheryPlot <- function(data,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FisheryPlot',lfa),preliminary=NULL,units='t',...) {
    	  par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
    	  plot(fishData$YEAR,fishData$LANDINGS,xlab='Year',ylab='Landings (t)',type='h',main="LFA 33",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
    	  lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)

    	  par(new=T)
    	  plot(fishData$YEAR,fishData$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(fishData$EFFORT2/1000,na.rm=T)))
    	  points(max(fishData$YEAR),fishData$EFFORT2[length(fishData$EFFORT2)]/1000, type='b', pch=21,bg=rgb(1,0.6,0))
    	  axis(4)
    	  mtext("Effort ('000s Trap Hauls)", 4, 3.5, outer = F,las=0)

    	  write.csv(fishData,file.path(figdir,paste('FisheryPlot33.csv',sep='')))
	  dev.off()
	  
	  #French Version
	  
	  png(filename=file.path(figdir, "Landings_Effort_LFA33_French.png"),width=8, height=5, units = "in", res = 800)
	  #FisheryPlot <- function(data,lfa=NULL,fd=file.path(project.figuredirectory('bio.lobster','ReferencePoints')),title = paste('LFA',lfa),fn=paste0('FisheryPlot',lfa),preliminary=NULL,units='t',...) {
	  par(mar=c(5.1, 4.1, 4.1, 5.1),las=1)
	  plot(fishData$YEAR,fishData$LANDINGS,xlab='Ann?e',ylab='D?barquements (t)',type='h',main="LFA 33",ylim=c(0,max(fishData$LANDINGS)*1.2),pch=15,col='grey',lwd=10,lend=3)
	  lines(max(fishData$YEAR),fishData$LANDINGS[length(fishData$LANDINGS)],type='h',pch=21,col=rgb(1,0.6,0),lwd=10,lend=3)
	  
	  par(new=T)
	  plot(fishData$YEAR,fishData$EFFORT2/1000,ylab='',xlab='', type='b', pch=16, axes=F,ylim=c(0,max(fishData$EFFORT2/1000,na.rm=T)))
	  points(max(fishData$YEAR),fishData$EFFORT2[length(fishData$EFFORT2)]/1000, type='b', pch=21,bg=rgb(1,0.6,0))
	  axis(4)
	  mtext("Effort (x 1000 casiers lev?s)", 4, 3.5, outer = F,las=0)
	  dev.off()


	  #-----------------------------------------------------------------------

	  # to compare weekly fishing effort year to year (include in AC presentation if desired)
	  logs33=logs[logs$LFA=="33",]
	  logs33$unique_days=paste(logs33$VR_NUMBER, logs33$DATE_FISHED, sep=':')
	  
	  #-----------------------------------------------------------------------
	  
	

	  #To Double Check Number of Fishing Days in each week of season
	  days=aggregate(DATE_FISHED~WOS+SYEAR,data=logs33,FUN=function(x) length(unique(x)))
	  days.y0=days[days$SYEAR==max(days$SYEAR),]
	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
	  plot(x=days$WOS,y=days$DATE_FISHED, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(2,27))
	  lines(days.y0$WOS, days.y0$DATE_FISHED, col="red", lwd=2)
	  text(paste(days.y0$SYEAR[1]), x=25, y=1, col="red", cex=1.6)
	  lines(days.y1$WOS, days.y1$DATE_FISHED, col="blue")
	  text(paste(days.y1$SYEAR[1]), x=25, y=2, col="blue", cex=1.6)

	  # to plot weekly fishing effort year to year (include in AC presentation if desired)
	  png(filename=file.path(figdir, "Weekly_Comparison_effort.png"),width=8, height=5.5, units = "in", res = 1200)
    	  days=aggregate(unique_days~WOS+SYEAR, data=logs33, length)
    	  days.y0=days[days$SYEAR==max(days$SYEAR),]
    	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
    	  plot(x=days$WOS,y=days$unique_days, type='n', main= "Days Fished by Week", xlab="Week of Season", ylab="Days Fished", xlim=c(2,27), xaxt='n')
    	  axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
    	  #plot past 10 years in light gray)
    	  for (i in c(0:10)){
    	  day=days[days$SYEAR==(max(days$SYEAR)-i),]  
    	  lines(day$WOS, day$unique_days, col="gray94") 
    	   }
    	  lines(days.y0$WOS, days.y0$unique_days, col="red")
    	  #text(paste(days.y0$SYEAR[1]), x=26, y=300, col="red", cex=1.5)
    	  lines(days.y1$WOS, days.y1$unique_days, col="blue")
    	  #text(paste(days.y1$SYEAR[1]), x=26, y=800, col="blue", cex=1.5)
    	  legend(x=1, y=800, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray88", "blue", "red"), bty='n')
    	  
	  dev.off()

	  # to plot weekly fishing CPUE year to year (include in AC presentation if desired)
	  png(filename=file.path(figdir, "Weekly_Comparison_cpue.png"),width=8, height=5.5, units = "in", res = 1200)
	  days=aggregate(CPUE~WOS+SYEAR, data=logs33, median)
	  days.y0=days[days$SYEAR==max(days$SYEAR),]
	  days.y1=days[days$SYEAR==(max(days$SYEAR)-1),]
	  plot(x=days$WOS,y=days$CPUE, type='n', main= "Median CPUE by Week", xlab="Week of Season", ylab="CPUE (kg/trap)", xlim=c(2,27), xaxt='n')
	  for (i in c(0:10)){
	    day=days[days$SYEAR==(max(days$SYEAR)-i),]  
	    lines(day$WOS, day$CPUE, col="gray94")
	  }
	  axis(side=1, at=seq(1,25,by=4.4), lab=c('Dec','Jan', 'Feb', "Mar", 'Apr', 'May'))
	  lines(days.y0$WOS, days.y0$CPUE, col="red")
	  #text(paste(days.y0$SYEAR[1]), x=26, y=1, col="red", cex=1.5)
	  lines(days.y1$WOS, days.y1$CPUE, col="blue")
	  #text(paste(days.y1$SYEAR[1]), x=26, y=1.4, col="blue", cex=1.5)
	  legend(x=23, y=1.55,cex=0.8, lty=1,c(paste((max(days$SYEAR)-10), (max(days$SYEAR)-2), sep=" - "),days.y1$SYEAR[1],days.y0$SYEAR[1]), col=c("gray88", "blue", "red"), bty='n')
	  dev.off()

	  # 2024- Good idea to include grid maps for AC presentation showing percent change from previous and maybe even actual CPUE by grid
	 
	  
	  # Phase plot for conclusions and advice (not used in FSR)
	  
	  #x11(width=8,height=7)
	  
	  x = read.csv(file.path(figdir,"CatchRateRefs33.csv"))
	  y = read.csv(file.path(figdir,"ExploitationRefs33.csv"))
	  
	  x=x[x$YEAR %in% unique(y$Yr),]
	  
	  
	  #Ensure these numbers are correct before producing phase plots
	  RR75 = 0.8551163
	  usr = 0.2776314
	  lrp = 0.1388157
	  
	  png(filename=file.path(figdir, "PhasePlot_LFA33.png"),width=8, height=8, units = "in", res = 800)
	  hcrPlot(B=x$running.median[x$YEAR>2005],mF=y$running.median,USR=usr,LRP=lrp,RR=RR75,yrs=2006:max(x$YEAR),ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T)
	  dev.off()
	  
	  #French Version
	  png(filename=file.path(figdir, "PhasePlot_LFA33_French.png"),width=8, height=8, units = "in", res = 800)
	  hcrPlot(B=x$running.median[x$YEAR>2005],mF=y$running.median,USR=usr,LRP=lrp,RR=RR75,yrs=2006:max(x$YEAR),ylims=c(0,1),xlims=NULL,FrenchCPUE=T, labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T)
	  dev.off()
	  
	  
	  #hcrPlot(B=x$CPUE[x$YEAR>2005],mF=y$ERfm,USR=usr,LRP=lrp,RR=RR75,yrs=2006:2018,ylims=c(0,1),xlims=NULL,labels=c('USR','LRP','RR'),RRdec=F, ylab = 'Exploitation', xlab = 'CPUE',yr.ends=T)
	  #savePlot(file.path(figdir,'PhasePlot33.png'),type='png')

#----------------------------------------------------------------
#plotting as per csasdown 4 panel plot

#add in the theme_csas
	  
	  theme_csas <- function(base_size = 11, base_family = "", text_col = "grey20",
	                         panel_border_col = "grey70") {
	    half_line <- base_size / 2
	    theme_light(base_size = base_size, base_family = "") +
	      theme(
	        panel.grid.major = element_blank(),
	        panel.grid.minor = element_blank(),
	        axis.ticks.length = unit(half_line / 2.2, "pt"),
	        strip.background = element_rect(fill = NA, colour = NA),
	        strip.text.x = element_text(colour = text_col),
	        strip.text.y = element_text(colour = text_col),
	        axis.text = element_text(colour = text_col),
	        axis.title = element_text(colour = text_col),
	        legend.title = element_text(colour = text_col, size = rel(0.9)),
	        panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
	        legend.key.size = unit(0.9, "lines"),
	        legend.text = element_text(size = rel(0.7), colour = text_col),
	        legend.key = element_rect(colour = NA, fill = NA),
	        legend.background = element_rect(colour = NA, fill = NA),
	        plot.title = element_text(colour = text_col, size = rel(1)),
	        plot.subtitle = element_text(colour = text_col, size = rel(.85))
	      )
	  }
	  
	  
	  #format from FSAR branch of CSASdown
	  
# Catch and eff
	  aaa=fishData
	  aap = aaa[nrow(aaa),] #Full data
	 aaa = aaa[1:(nrow(aaa)-1),] #Full data without final year
	  
	
	  ymax=12000
	  scaleright = max(aaa$EFFORT2)/ymax
	  g1 <- ggplot(data = aaa, aes(x = YEAR,y=LANDINGS)) +
	    geom_bar(stat='identity',fill='black') +
	    geom_bar(data=aap,aes(x=YEAR,y=LANDINGS),stat='identity',fill='gray66') +
	    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',shape=16)+
	    geom_point(data=aap,aes(x=YEAR,y=EFFORT2/scaleright),colour='grey66',shape=17,size=1.5)+
	    geom_line(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',linetype='dashed')+
	    scale_y_continuous(name='Landings', sec.axis= sec_axis(~.*scaleright/1000, name= 'Effort',breaks = seq(0,10000,by=2000)))+
	    labs(x = "Year") +
	    theme_csas()
	  
	  #French Landings
	  
	  g1.fr <- ggplot(data = aaa, aes(x = YEAR,y=LANDINGS)) +
	    geom_bar(stat='identity',fill='black') +
	    geom_bar(data=aap,aes(x=YEAR,y=LANDINGS),stat='identity',fill='gray66') +
	    geom_point(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',shape=16)+
	    geom_point(data=aap,aes(x=YEAR,y=EFFORT2/scaleright),colour='grey66',shape=17,size=1.5)+
	    geom_line(data=aaa,aes(x=YEAR,y=EFFORT2/scaleright),colour='black',linetype='dashed')+
	    scale_y_continuous(name='Débarquements', sec.axis= sec_axis(~.*scaleright/1000, name= 'Effort',breaks = seq(0,10000,by=2000)))+
	    labs(x = "Année") +
	    theme_csas()
	 
	  # standardized cpue
	    g2 <- ggplot(data = crd, aes(x = YEAR)) +
	    geom_point(aes(y = CPUE),size=1.5) +
	    geom_line(aes(y= running.median),colour='grey45')+
	    geom_point(data=subset(crd, YEAR==max(crd$YEAR)),aes(x=YEAR,y=CPUE),colour='grey66',shape=17,size=2.2)+
	    labs(x = "Year", y = " CPUE") +
	    geom_hline(yintercept=usr,colour='grey50',lwd=1.1,linetype='dashed')+
	    geom_hline(yintercept=lrp,colour='grey50',lwd=1.1,linetype='dotted')+
	    theme_csas() 

	    g2.fr <- ggplot(data = crd, aes(x = YEAR)) +
	      geom_point(aes(y = CPUE),size=1.5) +
	      geom_line(aes(y= running.median),colour='grey45')+
	      geom_point(data=subset(crd, YEAR==max(crd$YEAR)),aes(x=YEAR,y=CPUE),colour='grey66',shape=17,size=2.2)+
	      labs(x = "Année", y = " CPUE") +
	      geom_hline(yintercept=usr,colour='grey50',lwd=1.1,linetype='dashed')+
	      geom_hline(yintercept=lrp,colour='grey50',lwd=1.1,linetype='dotted')+
	      theme_csas() 
	  
	  # Exploitation CCIR
	    exref=read.csv(file.path(figdir, "ExploitationRefs33.csv"))
	    g3 <- ggplot(data = exref, aes(x = Yr)) +
	    geom_ribbon(aes(ymin=ERfl,ymax=ERfu), fill="grey", alpha=0.22) +
	    geom_point(aes(y = ERfm)) +
	    geom_line(aes(y= ERfm),colour='grey',lwd=0.9, linetype='dotted')+
	    geom_line(aes(y= running.median),colour='grey45')+
	    geom_hline(yintercept=RR75,colour='grey50',lwd=1.1,linetype='dashed')+
	    scale_y_continuous(limits=c(0,1), n.breaks=6)+
	    labs(x = "Year", y = 'Exploitation Index') +
	    theme_csas()
	  
	    g3.fr <- ggplot(data = exref, aes(x = Yr)) +
	      geom_ribbon(aes(ymin=ERfl,ymax=ERfu), fill="grey", alpha=0.22) +
	      geom_point(aes(y = ERfm)) +
	      geom_line(aes(y= ERfm),colour='grey',lwd=0.9, linetype='dotted')+
	      geom_line(aes(y= running.median),colour='grey45')+
	      geom_hline(yintercept=RR75,colour='grey50',lwd=1.1,linetype='dashed')+
	      scale_y_continuous(limits=c(0,1), n.breaks=6)+
	      labs(x = "Année", y = "Indice d'Exploitation") +
	      theme_csas()
	  
	    # Recruitment 
	    rec=read.csv(file.path(figdir, "FSRSRecruitCatchRate33.recruits.csv"))
	    g4 <- ggplot(data = rec, aes(x = YEAR)) +
	    geom_ribbon(data=rec,aes(ymin=lb,ymax=ub), fill="grey", alpha=0.2) +
	    geom_point(aes(y = median)) +
	    geom_line(aes(y= median),colour='grey45')+
	    scale_y_continuous(limits=c(0,4), n.breaks=5)+
	    labs(x = "Year", y = 'Recruitment Index') +
	    theme_csas()
	    
	    g4.fr <- ggplot(data = rec, aes(x = YEAR)) +
	      geom_ribbon(data=rec,aes(ymin=lb,ymax=ub), fill="grey", alpha=0.2) +
	      geom_point(aes(y = median)) +
	      geom_line(aes(y= median),colour='grey45')+
	      scale_y_continuous(limits=c(0,4), n.breaks=5)+
	      labs(x = "Année", y = "Indice de Recrutement") +
	      theme_csas()
	    
	  
	  fsrplot=cowplot::plot_grid(g1, g2, g3, g4, ncol = 2, labels = "AUTO",label_x=0.15,label_y=0.98, label_size = 15, align = "hv")
	  fsrplot.fr=cowplot::plot_grid(g1.fr, g2.fr, g3.fr, g4.fr, ncol = 2, labels = "AUTO",label_x=0.15,label_y=0.98, label_size = 15, align = "hv")

	  png(filename=file.path(figdir, "fsrs.panel.plot.png"), width=1200, height=900, res=125)
	  fsrplot
	  dev.off()	  
	  
	  #French version
	  png(filename=file.path(figdir, "fsrs.panel.plot.french.png"), width=1200, height=900, res=125)
	  fsrplot.fr
	  dev.off()
	  
	 
# Contextual Indicators #############


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#mood


# run this "bio/bio.lobster/inst/LFA2733Framework/Assessment/1.IndicatorEstimation.CohortAnalysis.r"
# and this "bio/bio.lobster/inst/LFA2733Framework/Assessment/ContextualIndicators.r"

# Fishery footprint

