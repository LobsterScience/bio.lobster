
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()


sensitivity.to.seasonality = T
if(sensitivity.to.seasonality){		
			lobster.db('fsrs.commercial.samples')
			fs = subset(fsrs.comm,SYEAR>2002)
			p = lobster.db('seasonal.landings')
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
 			dt  = DTs[grep('33',names(DTs))]
			ad = 2004:2016
			cH = lobster.db('community.to.grid.contemporary')		
			cH = subset(cH, LFA==33)
			cG = lobster.db('community.to.grid.historic')		
			cG = subset(cG, LFA==33)
			outN = list()

		outSall = list()
		grps = list(WOS = 1:4, WOS2=5:8,WOS3=9:12,WOS4 = 13:16,WOS5=17:20, WOS6=21:24, WOS7=25:28,WOS8 = 1:28)
		oAll = list()

	for(j in 1:length(grps)) {
				io = grps[j]
				names(io) = 'WOS'
						outS = out = list()	

			for(i in 1:length(ad)) {
					print(ad[i])
				po = 33
				yo = ad[i]
				mm = 82.5
				da = atSeaWeightings(atSea = fs, fsrs.commercial.samples=T, comGridHist =subset(cG,LFA==33),comGridCont = subset(cH,LFA==33 & SYEAR==ad[i]), year=ad[i],lfa=33,females.only=F,mls=10)
				op = weightedCLF(x=da,returnLF=T,grouping = io,fsrs.commercial.samples=T)
				os = op
				os$Grouping = os$vec<-NULL
			outS[[i]] <- unlist(os)
			
				ll = 'LFA33'
				lle = 'LFA33'
				yo = ad[i]
				mls=mm = 10.5
			
				lp = p[,c('SYEAR',names(p)[grep(ll,names(p))])]
				lp = rename.df(lp,'SYEAR','YR')
				dt = DTs[[grep('33W',names(DTs))]]
				dt = dt[which(names(dt)==85):which(names(dt)==130)]
				dt = c(dt[1],mean(dt[2:3]),mean(dt[4:5]),mean(dt[6:7]),mean(dt[8:9]))
				Tc = 0.3

	

				if(!is.null(op)){
				
				vec = c(10.5,11,12,13,14)
				oo = op$vec[op$vec>=mm & op$vec<15]
				v0 = table(oo)
				
				ho = as.numeric(names(v0))
				
				wts = lobLW(ho,fsrs=T)
				bwts = v0 * wts
				
				
				le = subset(lp,substr(YR,6,9) == yo)[,2] 
				acWt = bwts / sum(bwts) * le
				N = acWt / wts # tons / g = #'s in '000000
				outN[[i]]  = data.frame(N = N, Len = names(v0),Year = yo,MLS=mm)
 			outS[[i]] = c(outS[[i]], new.rec = as.numeric(v0[1] / sum(v0)))
 
				ca = cohortAnalysis(lens = as.numeric(names(N)), N = as.numeric(N), dt = c(dt[1],dt[2:length(dt)]*2)/365) #annual
				out[[i]] = c(YEAR=yo, MLS=mm, N = sum(N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
				}
				}
			out = as.data.frame(do.call(rbind,out))
			out = toNums(out,2:ncol(out))
			out$W = j
			outS = as.data.frame(do.call(rbind,outS))
			outS = toNums(outS,2:ncol(outS))
			outS$W = j
			oAll[[j]] = out
			outSall[[j]] = outS
			
		}

a = do.call(rbind,oAll)
h = do.call(rbind,outSall)


plot(2004:2016,rep(1,13),type='n',xlab='Year',ylab="Exploitation",ylim=c(0.2,00.9))
w = unique(a$W)
for(i in w){
			with(subset(a,W==i),lines(YEAR,expl,col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'),col=1:8,lty=1:8,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots','LFA33WOS.png'),type='png')


plot(2004:2016,rep(1,13),type='n',xlab='Year',ylab="Proportion Berried",ylim=c(0,0.04))
w = unique(h$W)
for(i in w){
			with(subset(h,W==i),lines(Year,prop.berried,col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'),col=1:8,lty=1:8,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33BerriedWOS.png'),type='png')



plot(2004:2016,rep(1,13),type='n',xlab='Year',ylab="Proportion Female",ylim=c(0.35,0.55))
w = unique(h$W)
for(i in w){
			with(subset(h,W==i),lines(Year,prop.female,col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'),col=1:8,lty=1:8,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33FemaleWOS.png'),type='png')



plot(2004:2016,rep(1,13),type='n',xlab='Year',ylab="Maximum Size",ylim=c(10,15))
w = unique(h$W)
h = rename.df(h , 'quants.97.5%','Max')
for(i in w){
			with(subset(h,W==i),lines(jitter(as.numeric(Year)),jitter(Max),col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'),col=1:8,lty=1:8,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33MaxSizejitter.png'),type='png')


plot(1:7,rep(1,7),type='n',xlab='Weeks of Season',ylab="Maximum Size",ylim=c(10,16),xaxt='n')
axis(side=1,at=1:7,labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28'))
w = unique(h$Year)
h = subset(h,W<8)
h = rename.df(h , 'quants.97.5%','Max')
for(i in 1:length(w)){
			with(subset(h,Year==w[i]),lines(jitter(as.numeric(W)),jitter(Max),col=cols[i],lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',w,col=cols,lty=1:16,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33MaxSizejitter.png'),type='png')



plot(2004:2016,rep(1,13),type='n',xlab='Year',ylab="Median Size",ylim=c(8,12))
w = unique(h$W)
h = rename.df(h , 'quants.50%','Med')
for(i in w){
			with(subset(h,W==i),lines(jitter(as.numeric(Year)),jitter(Med),col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'),col=1:8,lty=1:8,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33MaxSizejitter.png'),type='png')


####year lines not week lines
jet.colors <-  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
cols = jet.colors(16)


plot(1:8,rep(1,8),type='n',xlab='Weeks of Season',ylab="Exploitation",ylim=c(0.35,0.9),xaxt='n')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
w = unique(a$YEAR)
#a = subset(a,W<8)
for(i in 1:length(w)){
			with(subset(a,YEAR==w[i] & W %in% 1:7),lines(W,expl,col=cols[i],lty=i,lwd=2,type='b',pch=16))
			with(subset(a,YEAR==w[i] & W %in% 8),points(8,expl,col=cols[i],lty=i,lwd=2,type='b',pch=16,cex=1.2))
		}
legend('bottomleft',legend=w,col=cols,lty=1:16,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots','LFA33YearLines.png'),type='png')



plot(1:8,rep(1,8),type='n',xlab='Weeks of Season',ylab="Proportion Female",ylim=c(0.38,0.54),xaxt='n')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
w = unique(h$Year)
#a = subset(a,W<8)
for(i in 1:length(w)){
			with(subset(h,Year==w[i] & W %in% 1:7),lines(W,prop.female,col=cols[i],lty=i,lwd=2,type='b',pch=16))
			with(subset(h,Year==w[i] & W %in% 8),points(8,prop.female,col=cols[i],lty=i,lwd=2,type='b',pch=16,cex=1.2))
		}

legend('bottomleft',legend=w,col=cols,lty=1:16,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33ProportionFemaleYearLines.png'),type='png')



plot(1:8,rep(1,8),type='n',xlab='Weeks of Season',ylab="Proportion Berried",ylim=c(0.0,0.05),xaxt='n')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
w = unique(h$Year)
#a = subset(a,W<8)
for(i in 1:length(w)){
			with(subset(h,Year==w[i] & W %in% 1:7),lines(W,prop.berried,col=cols[i],lty=i,lwd=2,type='b',pch=16))
			with(subset(h,Year==w[i] & W %in% 8),points(8,prop.berried,col=cols[i],lty=i,lwd=2,type='b',pch=16,cex=1.2))
		}

legend('bottomleft',legend=w,col=cols,lty=1:16,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA33ProportionBerriedYearLines.png'),type='png')


#boxplots by weeks of season


boxplot(prop.female~W,data=h,ylab='Sex Ratio',xaxt='n',xlab='Weeks of Season')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','FSRSCommSensPropFemale.png'),type='png')


boxplot(prop.berried~W,data=h,ylab='Proportion Berried',xaxt='n',xlab='Weeks of Season')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','FSRSCommSensPropBerried.png'),type='png')

boxplot(expl~W,data=a,ylab='Exploitation',xaxt='n',xlab='Weeks of Season')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots','FSRSCommSensExploitation.png'),type='png')



h = rename.df(h,'quants.50%','Median')
boxplot(Median~W,data=h,ylab='Median Size',xaxt='n',xlab='Weeks of Season')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','FSRSCommSensMedianSize.png'),type='png')

h = rename.df(h,'quants.97.5%','Max')
boxplot(Max~W,data=h,ylab='Maximum Size',xaxt='n',xlab='Weeks of Season')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','FSRSCommSensMaxSize.png'),type='png')

boxplot(new.rec~W,data=h,ylab='Proportion New Recruits',xaxt='n',xlab='Weeks of Season')
axis(side=1,at=c(1:8),labels = c('1-4','5-8','9-12','13-16','17-20','21-24','25-28','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','FSRSCommSensNewRec.png'),type='png')

