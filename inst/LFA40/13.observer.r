#at sea observer Length frequs
options(stringsAsFactors=F)
#combining mature females abundance at length and fecundity at length
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()


		LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
				attr(LFA41,'projection') <- 'LL'
		l41 = as.data.frame(unique(cbind(LFA41$PID,LFA41$OFFAREA)))
		names(l41) = c('PID','Area')

#ISDB tables 
			lobster.db(DS = 'lfa41.observer.samples') #object called obs.samp
			obs.samp$X = obs.samp$LONE*-1
			obs.samp$Y = obs.samp$LAT
			obs.samp$EID = 1:nrow(obs.samp)
			obs.samp$yr = year(obs.samp$BOARD_DATE)
			obs.samp$qt = quarter(obs.samp$BOARD_DATE)
			obs.samp$mn = month(obs.samp$BOARD_DATE)

			obs.samp = obs.samp[which(!is.na(obs.samp$X)),]

				g = findPolys(obs.samp,LFA41,maxRows=3000000)

				obs.samp = merge(obs.samp,g,by='EID')
				obs.samp = merge(obs.samp,l41,by='PID')
		

				O = obs.samp[,c('SEXCD_ID','yr','qt','Area','FISH_LENGTH','mn')]
				names(O) = c('sex','yr','qt','Area','carlength','mn')


			



#CRIS tables 1979-2008
				lobster.db('cris')

				#ports  =1,2,3,4,5 for cris tables observer samples for offshore

				ct = subset(cris.trips,PORT %in% c(1,2,3,4,5) & TARGETSPECIES==1) #all trips are in cris.traps -- 280 trips

				ctr = subset(cris.traps,PORT %in% c(1,2,3,4,5))

				cts = subset(cris.samples,PORT %in% c(1,2,3,4,5)) #only 279 trips with lengths

				ct = merge(cts,ctr,all.x=T,by=c('TRIPNO','TRAPNO','PORT'))

				ct = subset(ct,SPECIESCODE==1)
				names(ct) <- tolower(names(ct))
				ct = makePBS(ct,polygon=F)

				ct = ct[which(!is.na(ct$X)),]

				g = findPolys(ct,LFA41,maxRows=3000000)

				ct = merge(ct,g,by='EID')
				ct = merge(ct,l41,by='PID')
				ct$yr = year(ct$trapdate)
				ct$qt = quarter(ct$trapdate)
				ct$mn = month(ct$trapdate)


				P = ct[,c('sex','yr','qt','Area','carlength','mn')]

#Combined data frame


	O = rbind(P,O)				
	i = which(O$carlength > 250) 
	O = O[-i,]
	i = which(O$carlength < 20) 
	O = O[-i,]
	 			#sample sizes

				a = aggregate(carlength~yr,data=O,FUN=median)
				am = aggregate(carlength~yr,data=O,FUN=min)
				ax = aggregate(carlength~yr,data=O,FUN=max)
				
				b = aggregate(mn~yr+sex,data=O,FUN=length)
				bbt = aggregate(mn~yr,data=b,FUN=sum)
				bbb = merge(b,bt,by=c('yr'),all=T)

				bbc = aggregate(mn~yr,data=subset(O,carlength<82),FUN=length)
				bbs = merge(bc,bt,by=c('yr'),all=T)

#shorts
				with(bbs,plot(yr,mn.x/mn.y))
				#Offshore from stasko 1978 .03 1972-1977


#proportion berried
				with(subset(bbb,sex==3),plot(yr,mn.x/mn.y,type='l'))
				#Offshore from stasko 1978 .19 1972-1977

#propotion female
				bbf = aggregate(mn.x~yr+mn.y,data=subset(bbb,sex %in% c(2,3)),FUN=sum)
				with(bbf,plot(yr,mn.x/mn.y,type='p'))
				#Offshore from stasko 1978 .60 1972-1977

##inshore

			lobster.db('atSea')
         
            # add columns for year, quarter
            atSeaData<-addSYEAR(subset(atSea,LFA%in%c('33','34')))
            atSeaData$YEAR<-year(atSeaData$STARTDATE)
			atSeaData = subset(atSeaData, SPECIESCODE==2550)

	a = aggregate(CARLENGTH~YEAR,data=atSeaData,FUN=median)
	am = aggregate(CARLENGTH~YEAR,data=atSeaData,FUN=min)
	ax = aggregate(CARLENGTH~YEAR,data=atSeaData,FUN=max)
				
				b = aggregate(TRAPNO~YEAR+SEX,data=atSeaData,FUN=length)
				bt = aggregate(TRAPNO~YEAR,data=b,FUN=sum)
				bb = merge(b,bt,by=c('YEAR'),all=T)

				bc = aggregate(TRAPNO~YEAR,data=subset(atSeaData,CARLENGTH<82),FUN=length)
				bs = merge(bc,bt,by=c('YEAR'),all=T)

#shorts
				with(bs,plot(YEAR,TRAPNO.x/TRAPNO.y))
				#Offshore from stasko 1978 .03 1972-1977


#proportion berried
				with(subset(bb,SEX==3),plot(YEAR,TRAPNO.x/TRAPNO.y,type='l'))
				#Offshore from stasko 1978 .19 1972-1977

#propotion female
				bf = aggregate(TRAPNO.x~YEAR+TRAPNO.y,data=subset(bb,SEX %in% c(2,3)),FUN=sum)
				with(bf,plot(YEAR,TRAPNO.x/TRAPNO.y,type='p'))
				#Offshore from stasko 1978 .60 1972-1977

op= hist(subset(O,yr<1980,select=carlength)[,1],breaks=seq(50,205,by=3))
 X11()
opp= hist(subset(atSeaData,YEAR<=1980,select=CARLENGTH)[,1],,breaks=seq(50,205,by=3))

plot(op$mids,op$density,type='l',lwd=2.3,col='black',xlab='Carapace length',ylab='Proportion',ylim=c(0,0.036))
lines(opp$mids,opp$density,type='l',lwd=2.3,col='red')



