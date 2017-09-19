#map of survey density


###need to rerun broken
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"analysis")
la()
load_all('~/git/bio.survey/')


#NEFSC Setup

		LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
				LFA41 = subset(LFA41,SID==1)
				attr(LFA41,'projection') <- 'LL'


      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2016)
      p$length.based = T
      p$size.class = c(50,300)
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$strata.efficiencies=F
      p$clusters = c( rep( "localhost", 7) )
      p$strata.files.return=T
  
      
						p$season =c('spring')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'all'
                      	p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                        aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)

	aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data
	aa$yr = aa$GMT_YEAR
	YG = 5 # year grouping
	y = unique(aa$yr)
	yL = y[length(y)] #last year
	yLL = length(y)-1
	yLm = yLL %% YG
	yLr = yLL %/% YG
	yw = y[which(y %in% y[1:yLm])] #add the early years to the first histogram and keep the rest at 5 years
	
	yLw = c(rep(1,yLm),rep(1:yLr,each = YG),yLr+1)
	grps = data.frame(yr = y,ry = yLw)
	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(project.figuredirectory('bio.lobster'),paste('surveyBubblesNEFSCSpring',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap(xlim=c(-71,-63.5),ylim=c(39,45),boundaries='LFAs',addAmericanStrata=T,output='bio.lobster',fname='americanmapfull41.pdf',save=F,labcex =0.8,labels=T)
		addPolys(LFA41,border='blue')
		j = makePBS(j,polygon=F)
		j$Z =j$TOTNO
		addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",symbol.bg = rgb(1,0,0,.6),max.size=0.8,z.max=1000,type='surface')
		legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
		dev.off()	
	}



###fall

					p$season =c('fall')# p$series =c('spring');p$series =c('fall')
                        p$define.by.polygons = F
                        p$lobster.subunits=F
                        p$area = 'all'
                      	p = make.list(list(yrs=p$years.to.estimate),Y=p)
                    
                        aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)

	aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data
	aa$yr = aa$GMT_YEAR
	YG = 5 # year grouping
	y = unique(aa$yr)
	yL = y[length(y)] #last year
	yLL = length(y)-1
	yLm = yLL %% YG
	yLr = yLL %/% YG
	yw = y[which(y %in% y[1:yLm])] #add the early years to the first histogram and keep the rest at 5 years
	
	yLw = c(rep(1,yLm),rep(1:yLr,each = YG),yLr+1)
	grps = data.frame(yr = y,ry = yLw)
	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(project.figuredirectory('bio.lobster'),paste('surveyBubblesNEFSCFall',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap(xlim=c(-71,-63.5),ylim=c(39,45),boundaries='LFAs',addAmericanStrata=T,output='bio.lobster',fname='americanmapfull41.pdf',save=F,labcex =0.8,labels=T)
		j = makePBS(j,polygon=F)
		j$Z =j$TOTNO
		addPolys(LFA41,border='blue')
		addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
		legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
		dev.off()	
	}


#DFO

	  p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'all'
      p$years.to.estimate = c(1970:2016)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p$strata.files.return=T
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$reweight.strata = F #this subsets 
      
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
      


	aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data

	aa$yr = substr(aa$mission,4,7)
	YG = 5 # year grouping
	y = unique(aa$yr)
	yL = y[length(y)] #last year
	yLL = length(y)-1
	yLm = yLL %% YG
	yLr = yLL %/% YG
	yw = y[which(y %in% y[1:yLm])] #add the early years to the first histogram and keep the rest at 5 years
	
	yLw = c(rep(1,yLm),rep(1:yLr,each = YG),yLr+1)
	grps = data.frame(yr = y,ry = yLw)
	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(project.figuredirectory('bio.lobster'),paste('surveyBubblesDFOSummer',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap(xlim=c(-69,-56.8),ylim=c(41.2,47.5),boundaries='LFAs',addSummerStrata=T,output='bio.lobster',fname = 'summerstratamap.pdf',save=F,labcex =0.8,labels=F)
		j = makePBS(j,polygon=F)
		j$Z =j$totno
		addPolys(LFA41,border='blue')
		addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
		legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
		dev.off()	
	}



###Georges

     p$series =c('georges')# p$series =c('georges');p$series =c('fall')

      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(1987:2016)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=F
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)

      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'all'
      p$reweight.strata = F #this subsets 
      p$strata.files.return=T
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)


	aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data

	aa$yr = substr(aa$mission,4,7)
	YG = 5 # year grouping
	y = unique(aa$yr)
	yL = y[length(y)] #last year
	yLL = length(y)-1
	yLm = yLL %% YG
	yLr = yLL %/% YG
	yw = y[which(y %in% y[1:yLm])] #add the early years to the first histogram and keep the rest at 5 years
	
	yLw = c(rep(1,yLm),rep(1:yLr,each = YG),yLr+1)
	grps = data.frame(yr = y,ry = yLw)
	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(project.figuredirectory('bio.lobster'),paste('surveyBubblesDFOGeorges',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap(xlim=c(-70.5,-63.5),ylim=c(38.5,45),boundaries='LFAs',addGeorgesStrata=T,output='bio.lobster',fname='georgesmap41.pdf',save=F,labcex =0.8,labels=T)
		j = makePBS(j,polygon=F)
		j$Z =j$totno
		addPolys(LFA41,border='blue')
		addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
		legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
		dev.off()	
	}





