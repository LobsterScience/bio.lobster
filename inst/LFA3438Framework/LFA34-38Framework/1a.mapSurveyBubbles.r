#map of survey density


###need to rerun broken
require(bio.survey)
require(bio.lobster)
require(bio.groundfish)
p = bio.lobster::load.environment()
p$libs = NULL
fp = file.path(project.datadirectory('bio.lobster'),"figures","LFA3438Framework2019")
dir.create(fp,showWarnings=F)
la()
load_all('~/git/bio.survey/')


#NEFSC Setup
	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
	LFA34 = subset(LFAs,PID %in% c(34,35,36,38))
	
	attr(LFA34,'projection') <- 'LL'

      p$reweight.strata = F #this subsets 
      p$years.to.estimate = c(1969:2018)
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
	YG = 10 # year grouping
	y = unique(aa$yr)
	yL = y[length(y)] #last year
	yLL = length(y)-1
	yLm = yLL %% YG
	yLr = yLL %/% YG
	yw = y[which(y %in% y[1:yLm])] #add the early years to the first histogram and keep the rest at 5 years
	
	yLw = c(rep(1,yLm),rep(1:yLr,each = YG),yLr+1)
	grps = data.frame(yr = y,ry = yLw)
	defined.groups=T
if(defined groups ){
	grps = data.frame(yr = y)
	grps$ry = ifelse(grps$yr %in% 1969:1980,1,
				ifelse(grps$yr %in% 1981:1990,2,
				ifelse(grps$yr %in% 1991:1998,3,
				ifelse(grps$yr %in% 1999:2009,4,5))))
	}
	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(fp,paste('surveyBubblesNEFSCSpring',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap('34-38',boundaries='LFAs',addAmericanStrata=T,save=F,labcex =0.8,labels=T)
		addPolys(LFA34,border='red')
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
	if(defined groups ){
	grps = data.frame(yr = y)
	grps$ry = ifelse(grps$yr %in% 1969:1980,1,
				ifelse(grps$yr %in% 1981:1990,2,
				ifelse(grps$yr %in% 1991:1998,3,
				ifelse(grps$yr %in% 1999:2009,4,5))))
	}

	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(fp,paste('surveyBubblesNEFSCFall',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap('34-38',boundaries='LFAs',addAmericanStrata=T,fname='americanmapfull41.pdf',save=F,labcex =0.8,labels=T)
		j = makePBS(j,polygon=F)
		j$Z =j$TOTNO
		addPolys(LFA34,border='red')
		addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
		legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
		dev.off()	
	}


#DFO

	  p$series =c('summer')# p$series =c('georges');p$series =c('fall')
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'all'
      p$years.to.estimate = c(1970:2018)
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
if(defined groups ){
	grps = data.frame(yr = y)
	grps$ry = ifelse(grps$yr %in% 1969:1980,1,
				ifelse(grps$yr %in% 1981:1990,2,
				ifelse(grps$yr %in% 1991:1998,3,
				ifelse(grps$yr %in% 1999:2009,4,5))))
	}

	aa = merge(aa,grps,by='yr',all.x=T)
	
	h = split(aa,f=aa$ry)

	for(i in 1:length(h)) {
		j = h[[i]]
		pdf(file.path(fp,paste('surveyBubblesDFOSummer',min(j$yr),max(j$yr),'pdf',sep=".")))
		LobsterMap('34-38',boundaries='LFAs',addSummerStrata=T,save=F,labcex =0.8,labels=F)
		j = makePBS(j,polygon=F)
		j$Z =j$totno
		addPolys(LFA34,border='red')
		addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
		legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
		dev.off()	
	}



###Lobster Survey

   