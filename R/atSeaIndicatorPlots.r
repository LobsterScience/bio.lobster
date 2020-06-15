#' @export

atSeaIndicatorPlots <- function(x,lfa,mls, indicators = c('mean.size',
	'median.size','prop.berried','prop.cull','prop.female','proportion.of.sampled.traps','prop.vnotched','min.size','max.size','TotalLobsters')) {	
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
									b$upper = b$mean+b$sd
									b$lower = b$mean-b$sd
								png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
										plot(b$Year,b$mean,xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n',ylim=range(c(b$upper,b$lower)))
											lines(mls$Year,mls$MLS_MM,type='l',col='black',lwd=3)
											points(b$Year,b$mean,xlim=c(1980, 2016),pch=19,type='p',col='red')
											arrows(x0=b$Year,y0=b$upper,y1=b$lower,length=0,col='red')
										if(length(!is.na(b$mean))>5) {
													t = rmed(b$Year,b$mean)
											lines(t$yr,t$x,col='blue',lwd=3,lty=2)
											}
											title(paste('LFA',lfa))
											dev.off()
											print(fname)
											}

		if(indicators[w] == 'median.size'){
								i = grep('50',names(a))
								b = a[,c(i,k,l)]
								ylabb = 'Median Carapace Length(mm)'
								fname = paste('MedianCarapaceLengthLFA',lfa,'.png',sep='')
								png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
								plot(b$Year,b[,'quants.50%'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
									lines(mls$Year,mls$MLS_MM,type='l',col='black',lwd=3)
									points(b$Year,b[,'quants.50%'],xlim=c(1980, 2016),pch=19,type='p',col='red')
									if(length(!is.na(b[,'quants.50%']))>5) {
												t = rmed(b$Year,b[,'quants.50%'])
										lines(t$yr,t$x,col='blue',lwd=3,lty=2)
										}
										title(paste('LFA',lfa))
									dev.off()
									print(fname)
								}
		if(indicators[w] == 'max.size'){
									i = grep('97.5',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Maximum Carapace Length(mm)'
									fname = paste('MaximumCarapaceLengthLFA',lfa,'.png',sep='')
								png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'quants.97.5%'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													lines(mls$Year,mls$MLS_MM,type='l',col='black',lwd=3)
													points(b$Year,b[,'quants.97.5%'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'quants.97.5%']))>5) {
																t = rmed(b$Year,b[,'quants.97.5%'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
									}
		if(indicators[w] == 'min.size'){
									i = grep('2.5',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Minimum Carapace Length(mm)'
									fname = paste('MinimumCarapaceLengthLFA',lfa,'.png',sep='')
									png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'quants.2.5%'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													lines(mls$Year,mls$MLS_MM,type='l',col='black',lwd=3)
													points(b$Year,b[,'quants.2.5%'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'quants.2.5%']))>5) {
																t = rmed(b$Year,b[,'quants.2.5%'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
											}
		
		if( indicators[w] == 'prop.berried'){
									i = grep('berried',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Proportion Berried'
									fname = paste('ProportionBerriedLFA',lfa,'.png',sep='')
									png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'prop.berried'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													points(b$Year,b[,'prop.berried'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'prop.berried']))>5) {
																t = rmed(b$Year,b[,'prop.berried'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
							
									}
		if(indicators[w] == 'prop.cull'){
									i = grep('cull',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Proportion Culls'
									fname = paste('ProportionCullsLFA',lfa,'.png',sep='')
									png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'prop.cull'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													points(b$Year,b[,'prop.cull'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'prop.cull']))>5) {
																t = rmed(b$Year,b[,'prop.cull'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
												}
		if(indicators[w] == 'prop.female'){
									i = grep('female',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Proportion Female'
									fname = paste('ProportionFemaleLFA',lfa,'.png',sep='')
									png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'prop.female'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													points(b$Year,b[,'prop.female'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'prop.female']))>5) {
																t = rmed(b$Year,b[,'prop.female'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
							

					}
		if(indicators[w] == 'proportion.of.sampled.traps'){
									i = grep('sampled',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Proportion of Sampled Traps'
									fname = paste('ProportionSampledTrapsLFA',lfa,'.png',sep='')
									fname = paste('ProportionSampledTrapsLFA',lfa,'.png',sep='')
									png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'proportion.of.sampled.traps.w.lobster'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													points(b$Year,b[,'proportion.of.sampled.traps.w.lobster'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'proportion.of.sampled.traps.w.lobster']))>5) {
																t = rmed(b$Year,b[,'proportion.of.sampled.traps.w.lobster'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
							
					}
		if(indicators[w] == 'prop.vnotched'){
									i = grep('notch',names(a))
									b = a[,c(i,k,l)]
									ylabb = 'Proportion V-Notched'
									fname = paste('ProportionVnotchedLFA',lfa,'.png',sep='')
									png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
													plot(b$Year,b[,'prop.vnotched'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
													points(b$Year,b[,'prop.vnotched'],xlim=c(1980, 2016),pch=19,type='p',col='red')
													if(length(!is.na(b[,'prop.vnotched']))>5) {
																t = rmed(b$Year,b[,'prop.vnotched'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
														title(paste('LFA',lfa))
													dev.off()
													print(fname)
									
					}
	if(indicators[w] == 'TotalLobsters'){
							b = a[,c(k,l)]
							ylabb = 'Total Lobsters Sampled'
							fname = paste('TotalLobsterSampled',lfa,'.png',sep='')
							png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
											plot(b$Year,b[,'TotalLobsters'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
											points(b$Year,b[,'TotalLobsters'],xlim=c(1980, 2016),pch=19,type='b',col='red')
											title(paste('LFA',lfa))
											dev.off()
											print(fname)
							
					}
	if(indicators[w] == 'ReproductivePotential'){

							i = grep('Egg',names(a))
									b = a[,c(i,k)]
							ylabb = 'Total Eggs'
							fname = paste('ReproductivePotential',lfa,'.png',sep='')
							png(file=file.path(fdir,fname),units='in',width=15,height=12,pointsize=24, res=300,type='cairo')
							par(mar=c(4,5,2,5))
										plot(b$Year,b[,'EggProd'],xlim=c(1980, 2016),xlab='Year',ylab=ylabb,type='n')
										points(b$Year,b[,'EggProd'],xlim=c(1980, 2016),pch=19,type='p',col='red')
										if(length(!is.na(b[,'EggProd']))>5) {
														t = rmed(b$Year,b[,'EggProd'])
														lines(t$yr,t$x,col='blue',lwd=3,lty=2)
														}
							par(new=T)
							plot(mls$Year,mls$MLS_MM,type='l',lty=1,xaxt='n', yaxt='n',ylab='',xlab='',col='black',ylim=c(70,90),lwd=2,xlim=c(1980,2016))
							axis(side=4,at=seq(70,90,length=5))
							mtext(side=4,'Minumum Legal Size',line=3,col='black')
			
											
											title(paste('LFA',lfa))
											dev.off()
											print(fname)
							
					}
	
			
	}
}