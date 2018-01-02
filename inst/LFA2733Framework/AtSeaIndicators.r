

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()

sset = T
sset = F 
 overall.at.sea.indicators =T
 if(overall.at.sea.indicators) { 
	
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

			IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Proportion.Landings', out.dir='bio.lobster',mls=mls)
			
x =  IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Median.Size', out.dir='bio.lobster',mls=mls,subset=sset)


				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Median.Size',ylim=c(70,115))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMedianSizeByLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMedianSizeByLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('black','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Median.Size',ylim=c(70,115))
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				 if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMedianSizeByLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMedianSizeByLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Maximum.Size', out.dir='bio.lobster',mls=mls,subset=sset)
				y = as.data.frame(do.call(rbind,x))
				ik = which(y$ID=='fsrs' & y$LFA %in% c(28,29,30,'31A'))
				y = y[-ik,]
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Maximum.Size',ylim=c(90,160))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMaxSizeByLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMaxSizeByLFA27-30.png'),type='png')


				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('black','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Maximum.Size',ylim=c(90,160))
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMaxSizeByLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedMaxSizeByLFA31A-33.png'),type='png')

x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = NULL, indicator = 'New.Recruits', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,4,5,7,8,10,11)
				if(sset) ats = c(1,2,4,5,7,8)
				cc = c('black','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Proportion New Recruits',ylim=c(0,1))
				axis(side=1,at=c(1.5,4.5,7.5,10.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				if(sset) axis(side=1,at=c(1.5,4.5,7.5),labels=c('LFA27','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropRecruitsLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropRecruitsLFA27-30.png'),type='png')
				
				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','blue','red')
				boxplot(Indi~ID+LFA,data=db2,at=ats,col=cc,xaxt='n',ylab='Proportion New Recruits',ylim=c(0,1))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropRecruitsLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropRecruitsLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Sex.Ratio', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Sex Ratio',ylim=c(0.2,.8))
				abline(h=0.5,lty=3)
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropFemaleLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropFemaleLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('black','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Sex Ratio',ylim=c(0.2,0.8))
				abline(h=0.5,lty=3)
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropFemaleLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropFemaleLFA31A-33.png'),type='png')




x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Proportion.Berried', out.dir='bio.lobster',mls=mls,subset=sset)
		
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,4,5,7,8,10,11)
				cc = c('black','orange')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Proportion.Berried',ylim=c(00,.6))
				axis(side=1,at=c(1.5,4.5,7.5,11.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropBerriedLFA27-30.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropBerriedLFA27-30.png'),type='png')
				
				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','blue')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Proportion.Berried',ylim=c(0,0.6))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				if(sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropberriedLFA31A-33.png'),type='png')
				if(!sset) savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','CombinedPropberriedLFA31A-33.png'),type='png')



###for females only for reproductive potential
reproductive.potential = T
if(reproductive.potential){		
			atSea.clean = lobster.db('atSea.clean')
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR<2017 & LFA<34)
			atsea = atSea.clean
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
	
			vec = 30:250
	out = list()

		for(i in 1:nrow(ad)) {
			print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaWeightings(atSea = atsea, comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=T)
				op = weightedCLF(x=da,returnLF=T)
				

				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31A') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == '31B') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'LFA33'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR')}
				if(!is.null(op)){
				v0 = hist(op$vec, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				iw = sum(v0$bwts[v0$mids<mm])
				wi = sum(v0$bwts[v0$mids>=mm])
				le = subset(lp,YR == yo)[,2] * op$prop.female #only female landings
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] * op$prop.female #only female landings
				li = le * iw/wi

				v0$acWt = c(v0$bwts[v0$mids<mm] / iw * li, v0$bwts[v0$mids>=mm] / wi * le) #total weight of lobsters in each size bin based on proportion of total weight
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				
				eggProd = pMat(lfa=ll,v0$mids) * Fecundity(lle,v0$mids) * v0$N
				out[[i]] = c(LFA = po, Year=yo, MLS=mm, EggProd = sum(eggProd))
				rm(v0)
				}
			}
		}
		out = as.data.frame(do.call(rbind,out))
		out = toNums(out,2:ncol(out))
		save(out,file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))
#		load(file = file.path(project.datadirectory('bio.lobster'),'outputs','atSeaIndicatorsFemalesLFA27-33.rdata'))
names(mls)[1] <- 'Year'
			
			atSeaIndicatorPlots(x = out,lfa=27,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=28,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=29,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=30,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa='31A',mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa='31B',mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=32,mls=mls,indicators='ReproductivePotential')
			atSeaIndicatorPlots(x = out,lfa=33,mls=mls,indicators='ReproductivePotential')
}



####subset of weeks NEED TO FINISY THIS
   		   load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SubsetWksSummaryatSeaIndicatorsDataLFA27-33.rdata')) #at Sea
		   aS = outS 

		   	load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsCommercialSamplesLanded33.rdata')) #fsrs
		   	fS = outS
		   	fS$LFA=33
	
		   	load(file.path(project.datadirectory('bio.lobster'),'outputs','subsetweeksportSummaryLFA27-33.rdata'))
			pS = outS
			
			load(file = file.path(project.datadirectory('bio.lobster'),'outputs','SummaryfsrsrecruitmentSamplesLanded27-33.rdata'))
			fR = outS


			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))

			IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Proportion.Landings', out.dir='bio.lobster',mls=mls,subset=T)
			
x =  IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Median.Size', out.dir='bio.lobster',mls=mls,subset=T)
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Median.Size',ylim=c(70,115))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMedianSizeByLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('black','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Median.Size',ylim=c(70,115))
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMedianSizeByLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Maximum.Size', out.dir='bio.lobster',mls=mls,subset=T)
				y = as.data.frame(do.call(rbind,x))
				ik = which(y$ID=='fsrs' & y$LFA %in% c(28,29,30,'31A'))
				y = y[-ik,]
				db1 = subset(y,LFA %in% c(27,29,30))
				ats = c(1,2,3,5,6,7,9,10,11)
				cc = c('black','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Maximum.Size',ylim=c(90,160))
				axis(side=1,at=c(2,6,10),labels=c('LFA27','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMaxSizeByLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('black','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Maximum.Size',ylim=c(90,160))
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedMaxSizeByLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = NULL, indicator = 'New.Recruits', out.dir='bio.lobster',mls=mls,subset=T)
		
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,29,30))
				ats = c(1,2,4,5,7,8)
				cc = c('black','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Proportion New Recruits',ylim=c(0,1))
				axis(side=1,at=c(1.5,4.5,7.5),labels=c('LFA27','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropRecruitsLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','blue','red')
				boxplot(Indi~ID+LFA,data=db2,at=ats,col=cc,xaxt='n',ylab='Proportion New Recruits',ylim=c(0,1))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropRecruitsLFA31A-33.png'),type='png')


x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Sex.Ratio', out.dir='bio.lobster',mls=mls,subset=T)
		
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','red')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Sex Ratio',ylim=c(0.2,.8))
				abline(h=0.5,lty=3)
				
				axis(side=1,at=c(2,6,10,14),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropFemaleLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,4,6,7,8,9,11,12,13,14,16,17,18,19)
				cc = c('black','orange','blue','red')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Sex Ratio',ylim=c(0.2,0.8))
				abline(h=0.5,lty=3)
				axis(side=1,at=c(2.5,7.5,12.5,17.5),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm','Port'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropFemaleLFA31A-33.png'),type='png')




x = IndicatorplotsMultDataSets(atSea=aS, port=pS, fsrs=fS,fsrs.rec = fR, indicator = 'Proportion.Berried', out.dir='bio.lobster',mls=mls,subset=T)
		
				y = as.data.frame(do.call(rbind,x))
				db1 = subset(y,LFA %in% c(27,28,29,30))
				ats = c(1,2,4,5,7,8,10,11)
				cc = c('black','orange')
				boxplot(Indi~ID+LFA,data=db1,at=ats,col=cc,xaxt='n',ylab='Proportion.Berried',ylim=c(00,.6))
				axis(side=1,at=c(1.5,4.5,7.5,11.5),labels=c('LFA27','LFA28','LFA29','LFA30'))
				legend('topright',legend=c('AtSea','FSRSRec'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropBerriedLFA27-30.png'),type='png')

				db2 = subset(y,LFA %in% c('31A','31B','32','33'))
				ats = c(1,2,3,5,6,7,9,10,11,13,14,15)
				cc = c('black','orange','blue')
				boxplot(Indi~ID+LFA,data=na.omit(db2),at=ats,col=cc,xaxt='n',ylab='Proportion.Berried',ylim=c(0,0.6))
				axis(side=1,at=c(2,6,10,14),labels=c('LFA31A','LFA31B','LFA32','LFA33'))
				legend('topright',legend=c('AtSea','FSRSRec','FSRSComm'),pch=15,col=cc,cex=0.8,pt.cex=1.3,bty='n')
				savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','subsetCombinedPropberriedLFA31A-33.png'),type='png')





