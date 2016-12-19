#' @export
#// dat is list of julian dates (doy)
#// i.e dat=list(120:150,180:225,211:300)


annualTimeCircle<-function(dat,scale=2,off=330,outer=F,clrs=2:(length(dat)+1),space=0.2,title='', pointer=NULL,...){
 

	xt<-sin(seq(0, 2 * pi, length = 365))
	yt<--cos(seq(0, 2 * pi, length = 365))
	
	x<-c(xt[off:365],xt[1:(off-1)])
	y<-c(yt[off:365],yt[1:(off-1)]) 
	
	# seasons 
	ssn<-as.Date(paste("2009-",seq(3,12,3),"-21",sep=""))
	si<-julian(ssn,origin=as.Date("2008-12-31"))
	
	# months 
	mths<-as.Date(paste("2009-",1:12,"-01",sep=""))
	mi<-julian(mths,origin=as.Date("2008-12-31"))
	
	# plot circle
	plot(x,y,asp=1,type='l', axes = FALSE, xlim = c(-scale, scale), ylim = c(-scale, scale), xlab = "",  ylab = "")


	# plot months

	if(outer==T){
		points(x[mi],y[mi],pch=16)
		text(x[mi+15]*1.15,y[mi+15]*1.15,months(mths,ab=T),cex=0.8)
	}
	if(outer==F){
		points(x[mi]*0.9,y[mi]*0.9,pch=16)
		text(x[mi+15]*0.75,y[mi+15]*0.75,months(mths,ab=T),cex=0.8)
	}
	
	# plot seasons
	polygon(c(x[si[1]:si[2]]*0.9,x[si[2]:si[1]]),c(y[si[1]:si[2]]*0.9,y[si[2]:si[1]]),col='yellow') # spring
	polygon(c(x[si[2]:si[3]]*0.9,x[si[3]:si[2]]),c(y[si[2]:si[3]]*0.9,y[si[3]:si[2]]),col='lightgreen') # summer
	polygon(c(x[si[3]:si[4]]*0.9,x[si[4]:si[3]]),c(y[si[3]:si[4]]*0.9,y[si[4]:si[3]]),col='goldenrod2') # fall
	polygon(c(x[c(si[4]:365,1:si[1])]*0.9,x[c(si[1]:1,365:si[4])]),c(y[c(si[4]:365,1:si[1])]*0.9,y[c(si[1]:1,365:si[4])]),col='skyblue') # winter
	
	
	# plot time lines
	for(i in 1:length(dat)){
		#browser()
		lines(x[dat[[i]]]*(1+space*i), y[dat[[i]]]*(1+space*i), col=clrs[i],lwd=2,...)
	}

		    
	if(!is.null(pointer))segments(x[pointer]*0.9,y[pointer]*0.9,x[pointer]*1.1,y[pointer]*1.1,col='red',lwd=2)
	
	title(main=title)	
}
