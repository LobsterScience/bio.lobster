

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()

#whether subset weeks is true or fales from CohortAnalysis (ByWeeks)
sset = T
sset = F 
	
	#from cohort analysis

		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
   	if(sset)	   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksSummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
		
		   aS = outS 

		   	load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsCommercialSamplesLanded33.rdata')) #fsrs
		   	fS = outS
		   	fS$LFA=33

		   	load(file.path(project.datadirectory('bio.lobster'),'outputs','portSummaryLFA27-33.rdata'))
	if(sset)	   	load(file.path(project.datadirectory('bio.lobster'),'outputs','subsetweeksportSummaryLFA27-33.rdata'))
			pS = outS
			
			load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsrecruitmentSamplesLanded27-33.rdata'))

			fR = outS


			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))

if(!sset)			IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Proportion.Landings', out.dir='bio.lobster',mls=mls)
			
x =  IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Median.Size', out.dir='bio.lobster',mls=mls,subset=sset)

				
				y = as.data.frame(do.call(rbind,x))
				w = subset(y,ID %in% c('fsrs','port','atSea'))

				boxplot(Indi~ID,data=w,xaxt='n',ylab='Median Size',col= c('grey','orange','red'),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(1,2,3),labels=c('At-Sea','FSRS Rec','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodMedianSize.png'),type='png')


				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('grey','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Median.Size',ylim=c(70,115),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMedianSizeByLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMedianSizeByLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('grey','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Median.Size',ylim=c(70,115),notch=F,border=c('black','black','black','black'))
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				 if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMedianSizeByLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMedianSizeByLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Maximum.Size', out.dir='bio.lobster',mls=mls,subset=sset)
				y = as.data.frame(do.call(rbind,x))
				#ik = which(y$ID=='fsrs' & y$LFA %in% c(28,29,30,'31A'))
				#y = y[-ik,]

				w = subset(y,ID %in% c('fsrs','port','atSea'))

				boxplot(Indi~ID,data=w,xaxt='n',ylab='Maximum Size',col= c('grey','orange','red'),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(1,2,3),labels=c('At-Sea','FSRS Rec','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodMaxSize.png'),type='png')

				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('grey','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Maximum.Size',ylim=c(90,160),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMaxSizeByLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMaxSizeByLFA27-30.png'),type='png')


				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('grey','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Maximum.Size',ylim=c(90,140),notch=F,border=c('black','black','black','black'))
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMaxSizeByLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMaxSizeByLFA31A-33.png'),type='png')

x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = NULL, indicator = 'New.Recruits', out.dir='bio.lobster',mls=mls,subset=sset)
		

				y = as.data.frame(do.call(rbind,x))

						w = subset(y,ID %in% c('port','atSea'))
				boxplot(Indi~ID,data=w,xaxt='n',ylab='Proportion New Recruits',col= c('grey','red'),notch=F,border=c('black','black'))
				axis(side=1,at=c(1,2),labels=c('At-Sea','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodNewRec.png'),type='png')

				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,4,5,7,8,10,11)
				if(sset) ats = c(1,2,4,5,7,8)
				cc = c('grey','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Proportion New Recruits',ylim=c(0,1),notch=F,border=c('black','black','black'))
				if(!sset) axis(side=1,at=c(1.5,4.5,7.5,10.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				if(sset) axis(side=1,at=c(1.5,4.5,7.5),labels=c('LFA27','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropRecruitsLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropRecruitsLFA27-30.png'),type='png')
				
				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('grey','blue','red')
				boxplot(Indi~ID+LFA,data=db2,at=ats,col=cc,xaxt='n',ylab='Proportion New Recruits',ylim=c(0,1),notch=F,border=c('black','black','black','black'))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropRecruitsLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropRecruitsLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Sex.Ratio', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))

							w = subset(y,ID %in% c('fsrs','port','atSea'))
				boxplot(Indi~ID,data=w,xaxt='n',ylab='Sex Ratio',col= c('grey','orange','red'),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(1,2,3),labels=c('At-Sea','FSRS Rec','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodSexRat.png'),type='png')

				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('grey','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Sex Ratio',ylim=c(0.2,.8),notch=F,border=c('black','black','black'))
				abline(h=0.5,lty=3)
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropFemaleLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropFemaleLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('grey','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Sex Ratio',ylim=c(0.2,0.8),notch=F,border=c('black','black','black','black'))
				abline(h=0.5,lty=3)
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropFemaleLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropFemaleLFA31A-33.png'),type='png')




x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Proportion.Berried', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))

							w = subset(y,ID %in% c('fsrs','port','atSea'))
				boxplot(Indi~ID,data=w,xaxt='n',ylab='Proportion Berried',col= c('grey','orange','red'),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(1,2,3),labels=c('At-Sea','FSRS Rec','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodBerr.png'),type='png')

				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,4,5,7,8,10,11)
				cc = c('grey','orange')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Proportion.Berried',ylim=c(00,.6),notch=F,border=c('black','black'))
				axis(side=1,at=c(1.5,4.5,7.5,11.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropBerriedLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropBerriedLFA27-30.png'),type='png')
				
				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('grey','orange','blue')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Proportion.Berried',ylim=c(0,0.6),notch=F,border=c('black','black','black'))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropberriedLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropberriedLFA31A-33.png'),type='png')



x = IndicatorplotsMultDataSets(atSea=aS, port=pS,  indicator = 'Probability.Mature', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))
				w = subset(y,ID %in% c('port','atSea'))
				boxplot(Indi~ID,data=w,xaxt='n',ylab='Probability Mature',col= c('grey','red'),notch=F,border=c('black','black'))
				axis(side=1,at=c(1,2),labels=c('At-Sea','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodMated.png'),type='png')

				db1 = subset(y,LFA %in% c(27,28,29,30))
				if(sset) ats = c(1,2,4,5,7,8)
				if(!sset) ats = c(1,2,4,5,7,8,10,11)
 				cc = c('grey','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Probability Mature',ylim=c(0.1,1),notch=F,border=c('black','black'))
				if(sset) axis(side=1,at=c(1.5,4.5,7.5),labels=c('LFA27','LFA29','LFA30'))
				if(!sset) axis(side=1,at=c(1.5,4.5,7.5,10.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropMatedLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropMatedLFA27-30.png'),type='png')
				
				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,4,5,7,8,10,11)
				cc = c('grey','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Probability Mature',ylim=c(0,1),notch=F,border=c('black','black'))
				axis(side=1,at=c(1.5,4.5,7.5,10.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropMatedLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropMatedLFA31A-33.png'),type='png')

x = IndicatorplotsMultDataSets(atSea=aS, port=pS,  indicator = 'Reproductive.Potential', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))
				w = subset(y,ID %in% c('port','atSea'))
				boxplot(Indi~ID,data=w,xaxt='n',ylab='Reproductive Potential',col= c('grey','red'),notch=F,border=c('black','black'))
				axis(side=1,at=c(1,2),labels=c('At-Sea','Port'))
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','MethodRepPo.png'),type='png')

				db1 = subset(y,LFA %in% c(27,28,29,30))
				if(sset) ats = c(1,2,4,5,7,8)
				if(!sset) ats = c(1,2,4,5,7,8,10,11)
				
				cc = c('grey','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Reproductive.Potential',notch=F,border=c('black','black'))
				if(sset) axis(side=1,at=c(1.5,4.5,7.5),labels=c('LFA27','LFA29','LFA30'))
				if(!sset) axis(side=1,at=c(1.5,4.5,7.5,10.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedReproductivePotLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedReproductivePotLFA27-30.png'),type='png')
				
				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,4,5,7,8,10,11)
				cc = c('grey','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Reproductive.Potential',notch=F,border=c('black','black'))
				axis(side=1,at=c(1.5,4.5,7.5,10.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedReproductivePotLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedReproductivePotLFA31A-33.png'),type='png')



