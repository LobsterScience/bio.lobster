# source("fn/fishing.season.r")

#// dat is two columns date and variable to plot

fishing.season<-function(dat,scale=2,off=330,outer=F,clr='grey80',fun=sum,smooth=0,title='', pointer=NULL){
 
	
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

	names(dat)<-c('date','variable')
	dat$date<-as.Date(dat$date)
	years<-unique(as.numeric(format(dat$date,"%Y")))
	for(i in 1:length(years)){
       DAY<-julian(as.Date(dat$date[as.numeric(format(dat$date,"%Y"))==years[i]]),origin=as.Date(paste(years[i]-1,"-12-31",sep="")))
    }
    
    metric<-with(dat,tapply(variable,DAY,fun,na.rm=T))
    met.dat<-merge(data.frame(day=1:365),data.frame(day=as.numeric(names(metric)),metric),all=T)
    met.dat$metric[is.na(met.dat$metric)]<-0
    if(smooth>0) met.dat<-merge(data.frame(day=1:365),data.frame(day=lowess(met.dat$metric,f=smooth)$x,metric=lowess(met.dat$metric,f=smooth)$y),all=T)

    met.dat$metric[is.na(met.dat$metric)]<-0
    met<-met.dat$metric
    dy<-met/max(met)*0.5*scale+1.1
    lines(x*dy,y*dy)
    polygon(c(x*dy,x*1.1),c(y*dy,y*1.1),col=clr)
	
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
	
    
	if(!is.null(pointer))segments(x[pointer]*0.9,y[pointer]*0.9,x[pointer]*1.1,y[pointer]*1.1,col='red',lwd=2)
	
	text(0,0,years,cex=1.2)
	title(main=title)	
	summary(metric)
}
