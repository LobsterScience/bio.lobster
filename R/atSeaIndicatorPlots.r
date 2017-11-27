#' @export

atSeaIndicatorPlots <- function(x,lfa,mls,method = c('none','space','time','time.space'), indicators = c('mean.size',
	'median.size','prop.berried','prop.cull','prop.female','proportion.of.sampled.traps','prop.vnotched','min.size','max.size')) {	
	# x is the output from AtSeaIndictors.r and weightedCLF
	fdir = file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors')
	dir.create( fdir, recursive = TRUE, showWarnings = FALSE )

	a = subset(x,LFA == lfa)
	mls = subset(mls,LFA==lfa)
	#change years
	#i = c(1,(which(diff(mls$MLS_MM)>0)+1))
	#mls = mls[i,]
	mls$Year[which(mls$Year==2015)] <- 2017
			k = grep('Year',names(a))
			l = grep('TotalLobsters',names(a))
for(w in 1:length(indicators)){		
		if( indicators[w] == 'mean.size') {
					i = grep('mean',names(a))
					j = grep('sd',names(a))
					b = a[,c(i,j,k,l)]
					ylabb = 'Mean Carapace Length(mm)'
					fname = paste('MeanCarapaceLengthLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'median.size'){
					i = grep('50',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Median Carapace Length(mm)'
					fname = paste('MedianCarapaceLengthLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'max.size'){
					i = grep('97.5',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Maximum Carapace Length(mm)'
					fname = paste('MaximumCarapaceLengthLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'min.size'){
					i = grep('2.5',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Minimum Carapace Length(mm)'
					fname = paste('MinimumCarapaceLengthLFA',lfa,'.png',sep='')
					}
		
		if( indicators[w] == 'prop.berried'){
					i = grep('berried',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Proportion Berried'
					fname = paste('ProportionBerriedLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'prop.cull'){
					i = grep('cull',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Proportion Culls'
					fname = paste('ProportionCullsLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'prop.female'){
					i = grep('female',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Proportion Female'
					fname = paste('ProportionFemaleLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'proportion.of.sampled.traps'){
					i = grep('sampled',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Proportion of Sampled Traps'
					fname = paste('ProportionSampledTrapsLFA',lfa,'.png',sep='')
					}
		if(indicators[w] == 'prop.vnotched'){
					i = grep('notch',names(a))
					b = a[,c(i,k,l)]
					ylabb = 'Proportion V-Notched'
					fname = paste('ProportionVnotchedLFA',lfa,'.png',sep='')
					}


			cols = c('black')
			pchs = c(1)
			ylims = c(min(a[,i],na.rm=T)/1.05,max(a[,i],na.rm=T)*1.05)
			png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
			plot(b$Year,b[,1],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n',ylim=ylims)
			lines(mls$Year,mls$MLS_MM,type='l',col='black',lwd=3)
			if(any('none' %in% method)) {
						i = grep('none',names(b))
						points(b$Year,b[,i[1]],xlim=c(1980, 2016),pch=16,type='p',col='salmon')
				if(length(!is.na(b[,i[1]]))>5) {
						t = rmed(b$Year,b[,i[1]])
						lines(t$yr,t$x,col='salmon',lwd=3)
						}
						cols = c(cols,'salmon')
						pchs = c(pchs,16)
				}
			if(any('time' %in% method)) {
						i = grep('time',names(b))
						i = i[grep('space',names(b)[i],invert=T)]
						points(b$Year,b[,i[1]],xlim=c(1980, 2016),pch=17,type='p',col='green')
				if(length(!is.na(b[,i[1]]))>5) {
						t = rmed(b$Year,b[,i[1]])
						lines(t$yr,t$x,col='green',lwd=3)
					}
						cols = c(cols,'green')
						pchs = c(pchs,17)
				}
			if(any('space' %in% method)) {
						i = grep('space',names(b))
						i = i[grep('time',names(b)[i],invert=T)]
						points(b$Year,b[,i[1]],xlim=c(1980, 2016),pch=18,type='p',col='blue')
				if(length(!is.na(b[,i[1]]))>5) {
						t = rmed(b$Year,b[,i[1]])
						lines(t$yr,t$x,col='blue',lwd=3)
						}
						cols = c(cols,'blue')
						pchs = c(pchs,18)
				}
			if(any('time.space' %in% method)) {
						i = grep('time.space',names(b))
						points(b$Year,b[,i[1]],xlim=c(1980, 2016),pch=19,type='p',col='red')
					if(length(!is.na(b[,i[1]]))>5) {
						t = rmed(b$Year,b[,i[1]])
						lines(t$yr,t$x,col='red',lwd=3)
					}
						cols = c(cols,'red')
						pchs = c(pchs,19)
				}
				title(paste('LFA',lfa))
				legend('topleft',lty=rep(1,length(cols)),col=cols,pch=c(pchs),legend=c('MLS',method),bty='n',cex=0.8)
				dev.off()
				print(fname)
		}

	}
