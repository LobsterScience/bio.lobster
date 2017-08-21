#Relative F and AIM

require(bio.lobster)
p = bio.lobster::load.environment()
require('bio.polygons')
p$libs = NULL
require(PBSmapping)
require(bio.lobster)
require(bio.utilities)
la()
 fp = file.path(project.datadirectory('bio.lobster'),'analysis')
 fn = file.path( project.figuredirectory("bio.lobster"))
# fisheries data

				lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

				logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))

				logs41$yr = year(logs41$DATE_FISHED) #2002 to present
				ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
				ziff41$DDLON = ziff41$DDLON * -1
				off41$yr  = year(off41$DATE_FISHED) #1981 to 1994

				logs41$OFFAREA = NULL	

				#oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

				a41 = rbind(off41,ziff41,logs41)
				a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)
				a41 = makePBS(a41,polygon=F)
				a41$ADJCATCH = a41$ADJCATCH / 2.2 #convert to kgs



#survey data within lfa41 boundaries

a = c('stratified.georges.Georges.Canada.base.length.83-300.male&female.sexed.rdata',
	  'stratified.summer.LFA41.restratified.length.83-300.male&female.sexed.rdata',
	  'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata',
	  'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'
	  )

#to set up a proportion of all animals over time


#boundaries for fishing data

			LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
			LFA41 = joinPolys(as.PolySet(LFA41),operation='UNION')
			LFA41 = subset(LFA41,SID==1)
			attr(LFA41,'projection') <- 'LL'


#areas surveyed
###########################################
#summer survey
#################################
			
			  b = find.bio.gis('strat.gf',return.one.match=F)
			  b = read.table(b)
			  names(b) <- c('X','Y','PID')
			  b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})

  		load(file.path(fp,'stratified.summer.LFA41.restratified.length.all.not.sexed.rdata'))
		
		all.out = out
	  	load(file.path(fp,'stratified.summer.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
		a = merge(all.out,out,by='yr')
		median(a$w.Yst.y/a$w.Yst.x)

		#0.876 proportion of total weight that comprises commercial animals....assuming constant over time....

		#  

##landings


			b = findPolys(completeFun(a41,c('X','Y')),b)$EID
			aS = subset(a41,EID %in% b)

			La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
			La$landings = La$ADJCATCH / 1000
			La = rename.df(La,'fishingYear','yr')


	

#full time series
			ao = all.out[,c('yr','w.Yst')]
			ao$w.Yst = ao$w.Yst * 0.876
			ao$w.Yst[ao$yr %in% out$yr] <- out$w.Yst[which(!is.na(out$yr))]


			La = rename.df(La,'fishingYear','yr')
			
			Lm = merge(ao,La)
			Lm$relF = Lm$landings / Lm$w.Yst
		

		save(Lm,file=file.path(fp,'BiomassLandingsSummer1981-2015.rdata'))

		rr = replacementRatio.relF(Lm$landings,Lm$w.Yst,savePlot=F, file.name='AIMDFOSummer.png',years.lagged.replacement=8,robust.reg=F) 


#detecting a change point in relative F
require(bcp)
t = which(is.finite(Lm$relF))
bt = bcp((Lm$relF[t]),w0=0.2,p0=0.05)
plot(bt,xaxlab=Lm$yr[t],xlab='Year')
savePlot(file.path(project.figuredirectory('bio.lobster'),'BCPrelFDFOSurvey.png'))

                              p$add.reference.lines = F
                              p$time.series.start.year = 1981
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFSummerRV.png'
                              p$ylim = c(0,13)
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=F
ii = which(Lm$yr<1996)
ii = intersect(t,ii)
ap = median(Lm$relF[ii])
apt = median(Lm$relF[t])

                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p,save=F)
abline(h=ap,col='blue',lwd=2)
abline(h=apt,col='green',lwd=2)
savePlot(file.path(project.figuredirectory('bio.lobster'),'relFDFOSurvey.png'))



############################################################


	#Georges
  load(file.path(fp,'stratified.georges.Georges.Canada.base.length.all.not.sexed.rdata'))
      aout = out
      aout = aout[,c('yr','w.Yst')]
      load(file.path(fp,'stratified.georges.Georges.Canada.base.length.83-300.male&female.sexed.rdata'))
      out = subset(out,yr>=2007)
      out = out[,c('yr','w.Yst')]

          aa=merge(out,aout,all.x=T,by='yr')
          sum(aa[,2])/sum(aa[,3]) #0.872 commercial
      aout$w.Yst = aout$w.Yst * 0.872
      aout$w.Yst[aout$yr %in% out$yr] <- out$w.Yst

			b = findPolys(completeFun(a41,c('X','Y')),gb)$EID
			aGB = subset(a41,EID %in% b)

			fy = unique(aGB$fishingYear)
			fy = 1987:2015

			
		#treating fishing year and survey year as same..
		La = aggregate(ADJCATCH~fishingYear,data=aGB,FUN=sum)
		La$landings = La$ADJCATCH / 1000
		La = rename.df(La,'fishingYear','yr')
		Lm = merge(aout,La,all.x=T)
		Lm$relF = Lm$landings / Lm$w.Yst
		
		
			rr = replacementRatio.relF(Lm$landings[-19],Lm$w.Yst[-19],savePlot=F, file.name='AIMDFOGeorges.png',years.lagged.replacement=8) 

#detecting a change point in relative F
require(bcp)
t = which(is.finite(Lm$relF))
bt = bcp((Lm$relF[t]),w0=0.2,p0=0.05)
plot(bt,xaxlab=Lm$yr[t],xlab='Year')
savePlot(file.path(project.figuredirectory('bio.lobster'),'BCPrelFDFOGeorgesSurvey.png'))

                              p$add.reference.lines = F
                              p$time.series.start.year = 1987
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFSummerRV.png'
                              p$ylim = c(0,13)
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=F
ii = which(Lm$yr<1998)
ii = intersect(t,ii)
ap = median(Lm$relF[ii])
apt = median(Lm$relF[t])

ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p,save=F)
abline(h=ap,col='blue',lwd=2)
abline(h=apt,col='green',lwd=2)
savePlot(file.path(project.figuredirectory('bio.lobster'),'relFDFOGeorges.png'))




             
#########################
##spring
	   b = importShapefile(find.bio.gis('bts',return.one.match=F))
	  load(file.path(fp,'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'))
	out = out[,c('yr','w.Yst')]

			b = findPolys(completeFun(a41,c('X','Y')),b)$EID
			aS = subset(a41,EID %in% b)
			fy = unique(aS$fishingYear)

		La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
		La$landings = La$ADJCATCH / 1000
		La = rename.df(La,'fishingYear','yr')
		
		Lm = merge(out,La,all.x=T)
		Lm = subset(Lm,yr>1980)
		#save(Lm, file=file.path(fp,'BiomassLandingsSpringNEF1981-2015.rdata'))
		Lm$relF = Lm$landings / Lm$w.Yst
		

	
			rr = replacementRatio.relF(Lm$landings[-25],Lm$w.Yst[-25],savePlot=F, file.name='AIMNEFSCSpring.png',years.lagged.replacement=8) 
			#r2 = 0.015 p= 0.97
#detecting a change point in relative F
require(bcp)
t = which(is.finite(Lm$relF))
bt = bcp((Lm$relF[t]),w0=0.2,p0=0.05)
plot(bt,xaxlab=Lm$yr[t],xlab='Year')
savePlot(file.path(project.figuredirectory('bio.lobster'),'BCPrelFNEFSCSPRINGSurvey.png'))

                              p$add.reference.lines = F
                              p$time.series.start.year = 1981
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFSummerRV.png'
                              p$ylim = c(0,3)
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=F
ii = which(Lm$yr<2002)
ii = intersect(t,ii)
ap = median(Lm$relF[ii])
apt = median(Lm$relF[t])

                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p,save=F)
abline(h=ap,col='blue',lwd=2)
abline(h=apt,col='green',lwd=2)
savePlot(file.path(project.figuredirectory('bio.lobster'),'relFNEFSCSPRING.png'))






	##fall
			   b = importShapefile(find.bio.gis('bts',return.one.match=F))
			  

	  load(file.path(fp,'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata'))

			out = out[,c('yr','w.Yst')]

			b = findPolys(completeFun(a41,c('X','Y')),b)$EID
			aS = subset(a41,EID %in% b)

	
		#treating fishing year and survey year as same..
		La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
		La$landings = La$ADJCATCH / 1000
		La = rename.df(La,'fishingYear','yr')
		Lm = merge(out,La,all.x=T)
		Lm = subset(Lm,yr>1980)

	#save(Lm,file=file.path(fp,'BiomassLandingsFall1981-2015.rdata'))


		Lm$relF = Lm$landings / Lm$w.Yst
		rr = replacementRatio.relF(Lm$landings[-25],Lm$w.Yst[-25],savePlot=T, file.name='AIMNEFSCFALL.png') 
		#r2 = 0.10, median Fref = 0.44 
		hist(na.omit(rr),'fd',main='',xlab='Fref',prob=T)

		savePlot(file.path(project.figuredirectory('bio.lobster'),'AIMFREFNEFSCFALLSurvey.png'))
			#r2 = 0.015 
#detecting a change point in relative F
require(bcp)
t = which(is.finite(Lm$relF))
bt = bcp((Lm$relF[t]),w0=0.2,p0=0.05)
plot(bt,xaxlab=Lm$yr[t],xlab='Year')
savePlot(file.path(project.figuredirectory('bio.lobster'),'BCPrelFNEFSCFALLSurvey.png'))

                              p$add.reference.lines = F
                              p$time.series.start.year = 1981
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFSummerRV.png'
                              p$ylim = c(0,3)
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=F
ii = which(Lm$yr<2002)
ii = intersect(t,ii)
ap = median(Lm$relF[ii])
apt = median(Lm$relF[t])
fref = 0.44

                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p,save=F)
abline(h=ap,col='blue',lwd=2)
abline(h=apt,col='green',lwd=2)
abline(h = fref,col='purple',lwd=2)
savePlot(file.path(project.figuredirectory('bio.lobster'),'relFNEFSCFALL.png'))

