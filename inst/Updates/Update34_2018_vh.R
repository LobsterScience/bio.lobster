####### Update 34: May 2017
require(bio.lobster)
la()

require(RODBC)
p = bio.lobster::load.environment()

    
##### lumped function lobster.db
#be sure to update current.assessment.year.r 
p=  list()
p$current.assessment.year=2018
p$yrs = 1947:p$current.assessment.year

redo.data=T

#lobster map
 LobsterMap(xlim=c(-67.5,-64), ylim=c(42.5,46),mapRes='HR',labcex=1.1)

#if you have not recaptured or updated data recently run:
	if(redo.data) {
			lobster.db( DS = 'annual.landings.redo')
			lobster.db( DS = 'seasonal.landings.redo')
		    lobster.db( DS = "logs.redo",    p=p)        # Inshore logs summary documents
		    lobster.db( DS = "process.logs.redo",    p=p)        # Inshore logs summary documents
            lobster.db( DS = "survey.redo",  p=p)   # ITLS Lobster Survey
      
					}


	annual.landings     = lobster.db('annual.landings') #annual.landings
	seasonal.landings   = lobster.db('seasonal.landings')
	
	#historical.landings = lobster.db('historical.landings')
	historical.landings<-read.delim(file.path(project.datadirectory('bio.lobster'),"data","Commercial","LFA34_Landings_1892-2004.txt"))
	names(historical.landings)[1] = 'YR'	


	Annual34<-rbind(subset(historical.landings,YR<1947), subset(annual.landings,select=c("YR","LFA34")))
	seasonal.landings$YR<-as.numeric(substr(seasonal.landings$SYEAR,6,9))
	Seasonal34<-subset(seasonal.landings,YR<p$current.assessment.year,c("SYEAR","LFA34","YR"))

	USR = median(subset(Seasonal34,YR %in% 1985:2009)$LFA34)*0.8

#3-year running mean

	Rm = as.data.frame(cbind(YR=1978:(p$current.assessment.year-1),Run.Mean=apply(embed(Seasonal34$LFA34,3),1,mean)))
	Rmed = runmed(Seasonal34$LFA34,k=3)
# Plot Landings (1892-present) Figure 2
		png(file.path( project.figuredirectory("bio.lobster"), "LFA34Landings2017.png"),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')

		par(mar=c(3,5,2,2))
			with(Annual34[which(Annual34$YR<p$current.assessment.year),],plot(YR,LFA34, las=1,ylab=' ',type='n',lwd=4, ylim=c(2000,max(c(Seasonal34$LFA34,Annual34$LFA34))),xaxt='n'))
			mtext("Landings (t)", 2,3.5)
			with(Annual34[which(Annual34$YR %in% 1880:1975),],lines(YR,LFA34,xlab='Year',ylab='Landings (t)',type='h',lwd=10,col='black'))
			with(Seasonal34[which(Seasonal34$YR<p$current.assessment.year),],lines(YR,LFA34,type='h',lwd=10,col='grey'))
			lines(1976:2017,Rmed,col='green',lwd=4,lty=2)
			axis(side=1,at=round(seq(min(Annual34$YR),p$current.assessment.year-1,length.out=7),0))
			abline(h=USR,col='blue',lwd=4)
			with(Rm, lines(YR, Run.Mean, col='red',lwd=5,lty=3))

			#legend('topleft',lwd=3,col=c('grey','black','blue','red','green'),c('Annual','Seasonal','Upper Stock Reference','3-year Running Mean','3-year Running Median'),bty='n',cex=0.8,lty=c(1,1,1,3,2))
			
		dev.off()
			


	## Commercial catch rate index
				logDat <- lobster.db('process.logs')
				logDat <- subset(logDat,SYEAR %in% 2003:(p$current.assessment.year-1) & LFA == 34)
		
				catch.new  <- with(logDat,tapply(WEIGHT_KG,SYEAR,sum))
				effort.new <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,sum))
				n.new      <- with(logDat,tapply(NUM_OF_TRAPS,SYEAR,length))

			#OLD DATA required until we find out where it came from	
				LFA34logData = read.delim(file.path(project.datadirectory('bio.lobster'),"data","Commercial","LFA34_CPUE_Data_2015.05.12.txt"))
				LFA34logData <- subset(LFA34logData,SYEAR!="OOS"&SYEAR %in% 1999:2002)
				LFA34logData$WEIGHT_KG<-LFA34logData$WEIGHT_LBS*0.4536

				
				catch  <- with(LFA34logData,tapply(WEIGHT_KG,SYEAR,sum))
				effort <- with(LFA34logData,tapply(NUM_OF_TRAPS,SYEAR,sum))
				n      <- with(LFA34logData,tapply(NUM_OF_TRAPS,SYEAR,length))

			
			#merge old and new data
				cpueLFA34.dat <- data.frame(year=1999:(p$current.assessment.year-1),n=c(n,n.new),catch=c(catch,catch.new),effort=c(effort,effort.new),cpue=c(catch/effort,catch.new/effort.new))
				save(cpueLFA34.dat,file=file.path(project.datadirectory('bio.lobster'),'data','products',"LFA34CPUE2017.rdata"))

				KgPTH<- as.matrix(cpueLFA34.dat$cpue,nrow=1)
				yrs<-sort(as.numeric(cpueLFA34.dat$year))
				rmKgPTH = apply(embed(KgPTH,3),1,mean)
				rmed = runmed(KgPTH,3)
			
		# Plot Commercial CPUE Figure 3

				png(file.path(project.figuredirectory("bio.lobster"), "LFA34CommercialCPUE2017.png"),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
				plot(yrs,KgPTH,pch=16,ylim=c(0,1.4),ylab='CPUE (Kg/TH)',las=1, main="LFA 34 - Commercial Log CPUE",xaxt='n',xlab='Year')
				axis(1,yrs,lab=yrs)
				lines(yrs[-(1:2)],rmKgPTH[!is.na(rmKgPTH)],lty=3,col='red',lwd=4)
				lines(yrs,rmed,lty=2,col='green',lwd=3)			
				abline(h=median(KgPTH[1:11]*0.8),col="blue", lwd=2)
				#text(max(yrs)+.5,median(KgPTH[2:15]*0.8)*.85,"Upper Stock Reference",col="blue", pos=2,cex=1)
				#legend('topleft',lwd=2,col=c('blue','red','green'),c('Upper Stock Reference','3-year Running Mean','3-year Running Median'),bty='n',cex=0.8,lty=c(1,3,2))

				dev.off()
				
			
	## ILTS Survey
	


