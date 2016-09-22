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

				ct = subset(cris.trips,PORT %in% c(1,2,3,4,5)) #all trips are in cris.traps -- 280 trips

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
	i = which(O$carlength > 200) 
	O = O[-i,]
	i = which(O$carlength < 20) 
	O = O[-i,]
	 			#sample sizes

				a = aggregate(carlength~Area+yr+qt,data=O,FUN=length)

				q1 = reshape(a[which(a$qt==1),c('yr','Area','carlength')],idvar='yr',timevar='Area',direction='wide')
				q2 = reshape(a[which(a$qt==2),c('yr','Area','carlength')],idvar='yr',timevar='Area',direction='wide')
				q3 = reshape(a[which(a$qt==3),c('yr','Area','carlength')],idvar='yr',timevar='Area',direction='wide')
				q4 = reshape(a[which(a$qt==4),c('yr','Area','carlength')],idvar='yr',timevar='Area',direction='wide')

				a = aggregate(carlength~yr+mn+Area,data=O,FUN=length)

				a1 = reshape(a[which(a$Area=="Crowell.Basin"),c('yr','mn','carlength')],idvar='yr',timevar='mn',direction='wide')
				a1 = a1[order(a1$yr),]
				a2 = reshape(a[which(a$Area=="Georges.Bank"),c('yr','mn','carlength')],idvar='yr',timevar='mn',direction='wide')
				a2 = a2[order(a2$yr),]
				a3 = reshape(a[which(a$Area=="Georges.Basin"),c('yr','mn','carlength')],idvar='yr',timevar='mn',direction='wide')
				a3 = a3[order(a3$yr),]
				a4 = reshape(a[which(a$Area=="SE.Browns"),c('yr','mn','carlength')],idvar='yr',timevar='mn',direction='wide')
				a4 = a4[order(a4$yr),]
				a5 = reshape(a[which(a$Area=="SW.Browns"),c('yr','mn','carlength')],idvar='yr',timevar='mn',direction='wide')
				a5 = a5[order(a5$yr),]
				

	O$id = paste(O$Area,O$qt,sep='.')

#seasonal and annual changes by area
	l = unique(O$Area)
	pdf('~/tmp/Olenfeq.pdf')

	for(j in l) {
			P = subset(O,Area==j)
			P = P[order(P$yr),]
			y = unique(P$yr) 
	
				for(i in y) {
					J = subset(P,yr==i)
					s = unique(J$qt)

					plot(1,1,xlim=c(min(O$carlength),max(O$carlength)),type='n',main=paste(j,i,sep="-"),ylim=c(0,1))
					legend('topleft',c("Winter",'Spring','Summer','Fall'),lty=c(1,1,1,1),col=c(1,2,3,4),bty='n')
						for(m in s) {
							K = subset(J,qt==m)
							B = hist(K$carlength,plot=F,breaks=seq(20,200,1))
							lines(B$mids,cumsum(B$density)/sum(B$density),col=m)
						}


				}
		}
dev.off()



#not all seasons and areas have adequate data the combinations to use are as determined by ACook Sept 2016

              SWBs = subset(O,Area=='SW.Browns' & mn %in% c(6,7))
              SWBf = subset(O,Area=='SW.Browns' & mn %in% c(10,11,12,1))

              SEB = subset(O,Area=='SE.Browns' & mn %in% c(4,5,6,7))
              GBs = subset(O,Area=='Georges.Basin' & mn %in% c(1,2,3,4))
              GBsu = subset(O,Area=='Georges.Basin' & mn %in% c(5,6,7))

              GBa = subset(O,Area=='Georges.Bank' & mn %in% c(5,6,7))

              #Crowell Basin dropped as no fishing observed trips since 2005
	            ll = list(SWBs,SWBf,SEB,GBs,GBsu,GBa)
	      		ln=c('SW.Browns.Summer','SW.Browns.Autumn','SE.Browns.Spring','Georges.Basin.Spring','Georges.Basin.Summer','Georges.Bank.Summer')

for( i in 1:length(ll)) {
	g = ll[[i]]

			lm = aggregate(carlength~yr,data=g, FUN=function(x) quantile(x,c(0.5,0.25,0.75)))
				lm = data.frame(lm[,1],lm$carlength[,1],lm$carlength[,2],lm$carlength[,3])

			af = aggregate(carlength~yr,data=g, FUN=length)
			names(af) = c('x','y')
			
			names(lm) = c('yr','medL','medLlower','medLupper')
			
				p=list()
			                  p$add.reference.lines = F
                              p$time.series.start.year = min(g$yr)
                              p$time.series.end.year = max(g$yr)
                              p$metric = 'medianL' #weights
                              p$measure = 'stratified.mean' #'stratified.total'
                              p$figure.title = ""
                              p$reference.measure = 'median' # mean, geomean
                              p$file.name = paste('medianL',ln[i],'png',sep='.')
    		                  print(p$file.name)

                        p$y.maximum = NULL # NULL # if ymax is too high for one year
                        p$show.truncated.numbers = F #if using ymax and want to show the numbers that are cut off as values on figure
                        		p$ylim = c(60,155)
                                p$legend = FALSE
                                p$running.median = T
                                p$running.length = 3
                                p$running.mean = F #can only have rmedian or rmean
                               p$error.polygon=T
                              p$error.bars=F
                              p$y2.type='h'

                              p$ylim2 = c(0,5000)
                             
                       figure.stratified.analysis(x=lm,out.dir = 'bio.lobster', x2 = af, p=p,sampleSizes=T,save=T)
		}



