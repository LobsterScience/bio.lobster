#' @export
figure.stratified.analysis <- function(x,p,out.dir='bio.lobster',sampleSizes=F,x2=NULL,save=T) {
	fn=file.path(project.datadirectory(out.dir),'figures')
	dir.create(fn,showWarnings=F)
	if(is.character(x)) {
		#if using file name
		load(file.path(project.datadirectory('out.dir'),'analysis',x))
		x = out; rm(out)
		}
	#default is to use the object directly

	with(p,{
		if(save) png(file=file.path(fn,file.name),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		m='Yst' ; mm = 'n'; lev='Stratified Total'; mt= 'Number'; div = 1000
		if(grepl('mean',measure)) {m = 'yst'; lev = 'Stratified Mean'; div =1}
		if(grepl('weight',metric)) {mm = 'w'; mt = 'Weight'}
		if(grepl('gini',metric)) {m = 'gini'; mt = 'Gini'; mm='gini'; div =1; ylim=c(0,1);lev=""}
		if(grepl('dwao',metric)) {m = 'dwao'; mt = 'Design Weighted Area Occupied'; mm='dwao'; div=1;lev=""}
		if(grepl('sexRatio',metric)) {m = 'pFem'; mt = 'Proportion Female'; mm = 'pFem';div=1;lev=""}
		if(grepl('medianL',metric)) {m = 'medL'; mt = 'Median Length'; mm = 'medL';div=1;lev=""}
		if(grepl('temper',metric)) {lev = 'Temperature'; mt=""}
		if(grepl('relF',metric)) { mm = m = metric; lev = 'Relative F'; div=1; mt=""}
		if(grepl('Fec',metric)) { mm = m = metric; lev = 'Reproductive Potential'; div=1; mt=""}
	
		n1 = names(x)[grep(m,names(x))]
		n2 = names(x)[grep(mm,names(x))]
		n = intersect(n1,n2)
		xp = x[,c('yr',n)]
		if(ncol(xp)==5) names(xp) = c('year','mean','se','lower','upper')
		if(ncol(xp)==4) names(xp) = c('year','mean','lower','upper')
		if(ncol(xp)==2) {names(xp) = c('year','mean'); xp$lower=NA; xp$upper=NA}
		if(metric == 'gini'){ xp = xp[,c('year','mean')] ; xp$lower = NA; xp$upper = NA}
		xp$mean = as.numeric(xp$mean) / div; xp$lower = as.numeric(xp$lower) / div; xp$upper = as.numeric(xp$upper) / div
		xpp = xp[which(xp$year>=time.series.start.year & xp$year<=time.series.end.year),  ]
		if(exists('ylim',p)) {
			ylim = ylim
			} 
		if(!exists('ylim',p)){
		ylim=c(min(xpp$lower),max(xpp$upper))
			}
		if(exists('y.maximum',p)) {
			yl = ylim[2];
			ylim[2] = y.maximum
			sll = xpp[which(xpp$upper>y.maximum),c('year','upper')]
		}
		
		if(any(is.na(ylim))) ylim = NULL
		if(sampleSizes) par(mar=c(5.1,4,4.1,4))
			plot(xpp$year,xpp$mean,type='n',xlab='Year',ylab = paste(lev,mt,sep=" "),ylim=ylim)
		if(error.polygon)	polygon(x=c(xpp$year,rev(xpp$year)),y=c(xpp$lower,rev(xpp$upper)),col='grey60', border=NA)
		
		if(error.bars)  	arrows(x0=as.numeric(xpp$year),x1 = as.numeric(xpp$year), y0 = xpp$upper, y1 = xpp$lower, lwd=1, angle=90, length= 0)

		points(xpp$year,xpp$mean,type='b',lty=1,pch=16,lwd=2)

		if(running.mean){
		  print(paste(running.length,'Year Running Mean'))
		  rmean = apply(embed(xpp$mean,running.length),1,mean)
      		  rmean.yr = xpp$year[running.length:length(xpp$year)]
		  lines(rmean.yr,rmean,lty=1,lwd=3,col='salmon')
		}
		if(running.median){
		#  print(paste(running.length,'Year Running Median'))
		  #rmean = smooth(xpp$mean,kind='3RS3R',endrule='Tukey')
		  	if(any(is.na(xpp$mean) | !is.finite(xpp$mean))) {
		  		ik  = which(is.na(xpp$mean) | !is.finite(xpp$mean))
				xpo = xpp$mean[-ik]
				xpy = xpp$year[-ik]
				rmean = runmed(xpo,k=running.length,endrule='median')
				yp = data.frame(mean = rmean, year=xpy)
				lines(yp$year,yp$mean,lty=1,lwd=3,col='salmon')
		  	} else {
      	rmean = runmed(xpp$mean,k=running.length,endrule='median')
        rmean.yr = xpp$year[1:length(xpp$year)]
		  lines(rmean.yr,rmean,lty=1,lwd=3,col='salmon')
		}
	}

			if(exists('y.maximum',p) &  exists('show.truncated.numbers',p)) {
				if(nrow(sll)>0){
				yym = rep(y.maximum*0.95,nrow(sll))
				if(nrow(sll>1)) {ap = sll$year[2:(length(sll$year))]-sll$year[1:(length(sll$year)-1)]}
				if(nrow(sll==1)) {ap = sll$year}
				if(any(ap<5)) {
					io = which(ap<5)+1
					for(i in 1:length(io)) {
						yym[io[i]] = y.maximum*(0.95-0.025)
					}
				}

				text(sll$year,yym,round(sll$upper,0),cex=0.85)
			}
		}

	if(exists('user.defined.references',p)) {
				lines(x=c(l.reference.start.year,l.reference.end.year),y=c(lref,lref),col='blue',lty=1,lwd=2)
				lines(x=c(u.reference.start.year,u.reference.end.year),y=c(uref,uref),col='blue',lty=1,lwd=3.5)
		}
	if(add.reference.lines) {
			me = xp[which(xp$year>=reference.start.year & xp$year<=reference.end.year), 'mean' ]
			if(reference.measure=='median')	xref = median(me)
			if(reference.measure=='mean')	xref = mean(me)
			if(reference.measure=='geomean') xref = geomean(me)
			if(add.primary.line) {
				lines(x=c(time.series.start.year,time.series.end.year),y=c(xref,xref),col='blue',lty=1,lwd=2)
				lines(x=c(reference.start.year,reference.end.year),y=c(xref,xref),col='blue',lty=1,lwd=3.5)
				}
			if(add.upper.lower) {
				uxref = xref*upper.reference.line
				lxref = xref*lower.reference.line
				lines(x=c(time.series.start.year,time.series.end.year),y=c(uxref,uxref),col='darkgreen',lty=2,lwd=2.5)
				lines(x=c(reference.start.year,reference.end.year),y=c(uxref,uxref),col='darkgreen',lty=1,lwd=4)
				lines(x=c(time.series.start.year,time.series.end.year),y=c(lxref,lxref),col='blue',lty=2,lwd=2.5)
				lines(x=c(reference.start.year,reference.end.year),y=c(lxref,lxref),col='blue',lty=1,lwd=4)
			}
		}
	if(legend){
	if(!running.mean & !running.median)	legend(legend.placement,lty=c(1,1),lwd=c(4,4),col=c('darkgreen','blue'),c('80% reference period','40% reference period'),bty='n',cex=0.8)
	if(running.mean)  legend(legend.placement,lty=c(1,1,1),lwd=c(4,4,4),col=c('darkgreen','blue','salmon'),c('80% reference period','40% reference period',paste(running.length,'yr Running Mean',sep="")),bty='n',cex=0.8)
	if(running.median)  legend(legend.placement,lty=c(1,1,1),lwd=c(4,4,4),col=c('darkgreen','blue','salmon'),c('80% reference period','40% reference period',paste(running.length,'yr Running Median',sep="")),bty='n',cex=0.8)
	}
  title(figure.title)

if(sampleSizes) {
		par(new=T)
		if(!exists('y2.type',p)) p$y2.type == 'l'
		with(x2,plot(x,y,type=p$y2.type,axes=F,xlab=NA,ylab=NA,col='blue',ylim=p$ylim2))
		axis(side=4)
		mtext(side = 4, line = 3, 'Sample Size')
		}

		print(file.path(fn,file.name))
if(exists('box',p)) {box(lwd=3)}
if(save) dev.off()
if(add.reference.lines) {return(c(Reference=xref,Reflow=lxref,Refhi=uxref))}
if(exists('return.running',p)) {return(yp)}
	})
}


