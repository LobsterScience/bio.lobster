
require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
la()


sensitivity.to.seasonality = T
if(sensitivity.to.seasonality){		
			atSea.clean = lobster.db('atSea.clean')
			atSea.clean$PORT = ifelse(atSea.clean$PORT==-99,0,atSea.clean$PORT)
			g = lobster.db('annual.landings')
			p = lobster.db('seasonal.landings')
			mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
			load(file.path(project.datadirectory('bio.lobster'),'outputs','deltaTsSimBH.rdata')) #DTs
			ad =  as.data.frame(unique(cbind(atSea.clean$LFA,atSea.clean$SYEAR)))
			ad = ad[order(ad[,1],ad[,2]),]
			names(ad) = c('LFA','YEAR')
			names(mls)[1] <- 'YEAR'
			ad = merge(ad,mls)
			ad = subset(ad,YEAR%in% 2011:2015 & LFA==27)
			atsea = atSea.clean
			cG = lobster.db('community.to.grid.historic')		
			cH = lobster.db('community.to.grid.contemporary')		
		outN = list()


		outSall = list()
		grps = list(WOS = 1:3, WOS2=4:6,WOS3=7:9,WOS8 = 1:9)
		oAll = list()

	for(j in 1:length(grps)) {
				io = grps[j]
				names(io) = 'WOS'
						outS = out = list()	

			for(i in 1:nrow(ad)) {
					print(ad[i,])
				po = ad[i,'LFA']
				yo = ad[i,'YEAR']
				mm = ad[i,'MLS_MM']
				da = atSeaWeightings(atSea = atsea,  comGridHist =subset(cG,LFA==ad[i,'LFA']),comGridCont = subset(cH,LFA==ad[i,'LFA'] & SYEAR==ad[i,'YEAR']), year=ad[i,'YEAR'],lfa=ad[i,'LFA'],females.only=F,at.sea.samples=T)
				op = weightedCLF(x=da,returnLF=T,at.sea.samples=T,grouping=io)
				os = op
				os$vec<-NULL
			outS[[i]] <- unlist(os)
			#Tc is fractional year of catch
				if(po == 27) 	{ll = "LFA27-30"; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('27N',names(DTs))]]; Tc = 0.67}
				if(po == 28) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('28',names(DTs))]]; Tc = 0.67}
				if(po == 29) 	{ll = 'LFA29'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('29',names(DTs))]]; Tc = 0.67}
				if(po == 30) 	{ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('30',names(DTs))]]; Tc = 0.67}
				if(po == '31A') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31A',names(DTs))]]; Tc = 0.67}
				if(po == '31B') {ll = 'LFA28,30'; 	lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('31B',names(DTs))]]; Tc = 0.67}
				if(po == 32) 	{ll = 'LFA32'; 		lle = 'LFA27-30'; lp = g[,c('YR',names(g)[grep(po,names(g))])]; dt = DTs[[grep('32',names(DTs))]]; Tc = 0.67}
				if(po == 33) 	{ll = 'LFA33'; 		lle = 'LFA33'; lp = p[,c('SYEAR',names(p)[grep(po,names(p))])]; lp = rename.df(lp,'SYEAR','YR'); dt = DTs[[grep('33W',names(DTs))]]; Tc = 0.3} 

				if(!is.null(op)){
				
				vec = mm:250
				oo = op$vec[op$vec>mm & op$vec<250]
				v0 = hist(oo, breaks=vec,plot=F)
				
				v0$wts = lobLW(v0$mids)
				v0$bwts = v0$counts * v0$wts
				
				
				le = subset(lp,YR == yo)[,2] 
				if(po ==33) le = subset(lp,substr(YR,6,9) == yo)[,2] 
				v0$acWt = v0$bwts / sum(v0$bwts) * le
				v0$N = v0$acWt / v0$wts # tons / g = #'s in '000000
				outN[[i]]  = data.frame(N = v0$N, Len = v0$mids,LFA = po, Year = yo,MLS=mm)
 				
 				#newly recruited fraction
 				outS[[i]] = c(outS[[i]], new.rec = sum(v0$N[v0$mids %in% seq(mm,mm+11,by=0.5)]) / sum(v0$N))



				brks = seq(mm,max(as.numeric(names(dt))),by=5)
				dt = dt[which.min(abs(brks[1]-as.numeric(names(dt)))):length(dt)]
				dt =data.frame(dt=dt,brks = as.numeric(names(dt)))
				dt$dt = dt$dt / 365 #* (5 / (as.numeric(names(dt))*0.15))
				dt = dt[1:length(brks),]
				dt$brks = brks
				v0$LCA = brks[findInterval(v0$mids,vec=brks)]
				LCAN = aggregate(v0$N~v0$LCA,FUN=sum)
				
				LCA = merge(LCAN,dt,by.x='v0$LCA',by.y = 'brks')
				k = which(LCA[,2]==0)[1]
				
				LCA = LCA[1:(k-1),]
				###need to get dts

				ca = cohortAnalysis(lens = LCA[,1], N = LCA[,2], dt = LCA[,3]) #annual

				LCA$LFA = po
				LCA$Year = yo
				LCA$MLS = mm
				out[[i]] = c(LFA = po, YEAR=yo, MLS=mm, N = sum(v0$N),Land = le,expl =ca$expl, F = ca$wF,M = ca$M,tF = ca$termF)
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

plot(2011:2015,rep(1,5),type='n',xlab='Year',ylab="Exploitation",ylim=c(0.2,0.75))
w = unique(a$W)
for(i in w){
			with(subset(a,W==i),lines(YEAR,expl,col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-3','4-6','7-9','All'),col=1:4,lty=1:4,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots','LFA27WOS.png'),type='png')


plot(2011:2015,rep(1,5),type='n',xlab='Year',ylab="Proportion Female",ylim=c(0.2,0.75))
w = unique(h$W)
for(i in w){
			with(subset(h,W==i),lines(Year,prop.female,col=i,lty=i,lwd=2,type='b',pch=16))
		}

legend('bottomleft',c('1-3','4-6','7-9','All'),col=1:4,lty=1:4,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA27WOSFemale.png'),type='png')


plot(2011:2015,rep(1,5),type='n',xlab='Year',ylab="Proportion Berried",ylim=c(0.05,0.3))
w = unique(h$W)
for(i in w){
			with(subset(h,W==i),lines(Year,prop.berried,col=i,lty=i,lwd=2,type='b',pch=16))
		}
legend('bottomleft',c('1-3','4-6','7-9','All'),col=1:4,lty=1:4,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))

savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA27WOSBerried.png'),type='png')


plot(2011:2015,rep(1,5),type='n',xlab='Year',ylab="Maximum size",ylim=c(95,120))
h = rename.df(h , 'quants.97.5%','Max')
w = unique(h$W)
for(i in w){
			with(subset(h,W==i),lines(Year,Max,col=i,lty=i,lwd=2,type='b',pch=16))
		}
legend('bottomleft',c('1-3','4-6','7-9','All'),col=1:4,lty=1:4,title='Weeks of Season',bty='n',cex=0.8,ncol=2,pch=rep(16,4))

savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','LFA27WOSMaxSize.png'),type='png')


##by weeks
####year lines not week lines
jet.colors <-  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
cols = jet.colors(5)



plot(1:4,rep(1,4),type='n',xlab='Weeks of Season',ylab="Exploitation",ylim=c(0.2,0.7),xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
w = unique(a$YEAR)
#a = subset(a,W<8)
for(i in 1:length(w)){
			with(subset(a,YEAR==w[i] & W %in% 1:3),lines(W,expl,col=cols[i],lty=i,lwd=2,type='b',pch=16))
			with(subset(a,YEAR==w[i] & W %in% 4),points(4,expl,col=cols[i],lty=i,lwd=2,type='b',pch=16,cex=1.2))
		}
legend('bottomleft',legend=w,col=cols,lty=1:4,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots','SensitivityLFA27YearLines.png'),type='png')


plot(1:4,rep(1,4),type='n',xlab='Weeks of Season',ylab="Median Size",ylim=c(75,90),xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
h = rename.df(h,'quants.50%','Median')
w = unique(h$Year)
#a = subset(a,W<8)
for(i in 1:length(w)){
			with(subset(h,Year==w[i] & W %in% 1:3),lines(W,Median,col=cols[i],lty=i,lwd=2,type='b',pch=16))
			with(subset(h,Year==w[i] & W %in% 4),points(4,Median,col=cols[i],lty=i,lwd=2,type='b',pch=16,cex=1.2))
		}
legend('bottomleft',legend=w,col=cols,lty=1:4,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27MedianYearLines.png'),type='png')


plot(1:4,rep(1,4),type='n',xlab='Weeks of Season',ylab="Maximum Size",ylim=c(95,125),xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
h = rename.df(h,'quants.97.5%','Max')
w = unique(h$Year)
#a = subset(a,W<8)
for(i in 1:length(w)){
			with(subset(h,Year==w[i] & W %in% 1:3),lines(W,Max,col=cols[i],lty=i,lwd=2,type='b',pch=16))
			with(subset(h,Year==w[i] & W %in% 4),points(4,Max,col=cols[i],lty=i,lwd=2,type='b',pch=16,cex=1.2))
		}
legend('bottomleft',legend=w,col=cols,lty=1:4,title='Years',bty='n',cex=0.8,ncol=2,pch=rep(16,4))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27MaxYearLines.png'),type='png')


#by weeks boxplot


boxplot(expl~W,data=a,xlab='Weeks of Season',ylab="Exploitation",xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'CohortAnalysisPlots','SensitivityLFA27BoxplotsExpl.png'),type='png')

h = rename.df(h,'quants.50%','Median')
boxplot(Median~W,data=h,xlab='Weeks of Season',ylab="Median Size",ylim=c(75,90),xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27BoxplotsMedian.png'),type='png')

h = rename.df(h,'quants.97.5%','Max')
boxplot(Max~W,data=h,xlab='Weeks of Season',ylab="Maximum Size",xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27BoxplotsMax.png'),type='png')



boxplot(new.rec~W,data=h,xlab='Weeks of Season',ylab="Proportion New Recruits",xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27BoxplotsNewRec.png'),type='png')




boxplot(prop.female~W,data=h,xlab='Weeks of Season',ylab="Sex Ratio",xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27BoxplotsFemale.png'),type='png')



boxplot(prop.berried~W,data=h,xlab='Weeks of Season',ylab="Proportion Berried",xaxt='n')
axis(side=1,at=c(1:4),labels = c('1-3','4-6','7-9','All'))
savePlot(file = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors','SensitivityLFA27BoxplotsBerried.png'),type='png')
