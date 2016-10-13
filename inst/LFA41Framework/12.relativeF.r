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


	
	#shortened time series
			out.r = subset(out,yr>=1999)
			out.r = out.r[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]

			
		#treating fishing year and survey year as same..
		Lm = merge(out.r,La,all.x=T)
		Lm$relF = Lm$landings / Lm$w.Yst
		Lm$relF.u = Lm$landings / Lm$w.ci.Yst.l
		Lm$relF.l = Lm$landings / Lm$w.ci.Yst.u
		rr = replacementRatio.relF(Lm$landings,Lm$w.Yst) 



#full time series
			out = all.out
			#o = which(out$w.Yst==0)
			#out[o,c('w.Yst','w.ci.Yst.l','w.ci.Yst.u')] <- NA 
			#hh = interpolate.xy.robust(out[,'w.Yst'],method='simple.linear')
			#out$w.Yst[o] <- hh[o]


			#interpolating NA's

			out = out[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]
			out$w.Yst = out$w.Yst * 0.876 #ratio
			out$w.ci.Yst.l = out$w.ci.Yst.l * 0.876 #ratio
			out$w.ci.Yst.u = out$w.ci.Yst.u * 0.876 #ratio
			La = rename.df(La,'fishingYear','yr')

						
		Lm = merge(out,La)
		Lm$relF = Lm$landings / Lm$w.Yst
		Lm$relF.u = Lm$landings / Lm$w.ci.Yst.l
		Lm$relF.l = Lm$landings / Lm$w.ci.Yst.u

		# can not use zeros, using interpolation to fill in 0's

		save(Lm,file=file.path(fp,'BiomassLandingsSummer1981-2015.rdata'))

		rr = replacementRatio.relF(Lm$landings,Lm$w.Yst,savePlot=F) 





		Lm = Lm[,c('yr','relF','relF.l','relF.u')]

		            #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = 1999
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
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p)





############################################################


	#Georges

			load(file.path(project.datadirectory('bio.polygons'),'data','Science','PED','GeorgesBankStrata.rdata'))
			gb = joinPolys(subset(out,PID %in% c(1,2)),operation='UNION')

			load(file.path(fp,'stratified.georges.Georges.Canada.base.length.83-300.male&female.sexed.rdata'))
			out = subset(out,yr>=2007)
			out = out[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]

			b = findPolys(completeFun(a41,c('X','Y')),gb)$EID
			aGB = subset(a41,EID %in% b)

			fy = unique(aGB$fishingYear)
			fy = 2007:2015

			pdf(file.path(fn, "GBSeasonalFishingPattern.pdf"),8,11)

			for(i in 1:length(fy)) {
				if(i %in% seq(1,90,by=9)){
					#x11()
					par(mfrow=c(3,3),mar=c(0,0,0,0))
					}
				w = subset(aGB,fishingYear==fy[i])
					fishing.season(w[,c('DATE_FISHED','ADJCATCH')],smooth=0.05)
					}
				dev.off()

		#treating fishing year and survey year as same..
		La = aggregate(ADJCATCH~fishingYear,data=aGB,FUN=sum)
		La$landings = La$ADJCATCH / 1000
		La = rename.df(La,'fishingYear','yr')
	 png(file=file.path(fn,'GBSurveyLandings.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		
		plot(La$yr,La$landings,type='l',xlab='Year',ylab='Landings')
		dev.off()
		Lm = merge(out,La,all.x=T)
		Lm$relF = Lm$landings / Lm$w.Yst
		Lm$relF.u = Lm$landings / Lm$w.ci.Yst.l
		Lm$relF.l = Lm$landings / Lm$w.ci.Yst.u
		Lm = Lm[,c('yr','relF','relF.l','relF.u')]

		            #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = 2007
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFGeorges.png'

                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p)
                 

             

##spring
			   b = importShapefile(find.bio.gis('bts',return.one.match=F))
			  

	  load(file.path(fp,'stratified.nefsc.spring.LFA41.restratified.length.83-300.male&female.sexed.rdata'))

			out = out[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]

			b = findPolys(completeFun(a41,c('X','Y')),b)$EID
			aS = subset(a41,EID %in% b)

			fy = unique(aS$fishingYear)
			fy = 1999:2015

			pdf(file.path( project.figuredirectory("bio.lobster"), "NEFSCSeasonalFishingPattern.pdf"),8,11)

			for(i in 1:length(fy)) {
				if(i %in% seq(1,90,by=9)){
					#x11()
					par(mfrow=c(3,3),mar=c(0,0,0,0))
					}
				w = subset(aS,fishingYear==fy[i])
					fishing.season(w[,c('DATE_FISHED','ADJCATCH')],smooth=0.05)
					}
				dev.off()

		#treating fishing year and survey year as same..
		La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
		La$landings = La$ADJCATCH / 1000
		La = rename.df(La,'fishingYear','yr')
	 png(file=file.path(fn,'NEFSCSurveyLandings.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
	
		plot(La$yr,La$landings,type='l',xlab='Year',ylab='Landings')
		dev.off()
		
		Lm = merge(out,La,all.x=T)
		Lm$relF = Lm$landings / Lm$w.Yst
		Lm$relF.u = Lm$landings / (Lm$w.ci.Yst.l+0.00001)
		Lm$relF.l = Lm$landings / (Lm$w.ci.Yst.u+0.00001)
		Lm = Lm[,c('yr','relF','relF.l','relF.u')]
		Lm = subset(Lm,yr>1980)
		            #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = 1981
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$ylim=c(0,5)
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFNEFSCSpring.png'
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p)
             

##fall
			   b = importShapefile(find.bio.gis('bts',return.one.match=F))
			  

	  load(file.path(fp,'stratified.nefsc.fall.LFA41.restratified.length.83-300.male&female.sexed.rdata'))

			out = out[,c('yr','w.Yst','w.ci.Yst.l','w.ci.Yst.u')]

			b = findPolys(completeFun(a41,c('X','Y')),b)$EID
			aS = subset(a41,EID %in% b)

			fy = unique(aS$fishingYear)
			fy = 1999:2015

			pdf(file.path( project.figuredirectory("bio.lobster"), "NEFSCSeasonalFishingPattern.pdf"),8,11)

			for(i in 1:length(fy)) {
				if(i %in% seq(1,90,by=9)){
					#x11()
					par(mfrow=c(3,3),mar=c(0,0,0,0))
					}
				w = subset(aS,fishingYear==fy[i])
					fishing.season(w[,c('DATE_FISHED','ADJCATCH')],smooth=0.05)
					}
				dev.off()

		#treating fishing year and survey year as same..
		La = aggregate(ADJCATCH~fishingYear,data=aS,FUN=sum)
		La$landings = La$ADJCATCH / 1000
		La = rename.df(La,'fishingYear','yr')
		Lm = merge(out,La,all.x=T)
		Lm$relF = Lm$landings / Lm$w.Yst
		Lm$relF.u = Lm$landings / (Lm$w.ci.Yst.l+0.00001)
		Lm$relF.l = Lm$landings / (Lm$w.ci.Yst.u+0.00001)
		Lm = Lm[,c('yr','relF','relF.l','relF.u')]
		Lm = subset(Lm,yr>1980)
		            #Figure
                              p$add.reference.lines = F
                              p$time.series.start.year = 1981
                              p$time.series.end.year = 2015
                              p$metric = 'relF' #weights
                              p$measure = '' #'stratified.total'
                              p$figure.title = ""
                              p$ylim=c(0,5)
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = 'relFNEFSCFall.png'
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=F
                              p$error.bars=T


                       ref.out=   figure.stratified.analysis(x=Lm,out.dir = 'bio.lobster', p=p)
			

