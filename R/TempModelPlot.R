#' @export
TempModelPlot = function(TempModel,graphic='R',depths=c(5,25,50,100),wd=12,ht=5,Area="33W",lfa=F,colors=tim.colors(length(Area)),yrs,idate="06-01",type=1:3,...){

	require(fields)

	if(1%in%type){
		# Data map
		if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','TempDataMap.png'),width=7,height=7,units='in',res=200)
		if(graphic=='R')x11()
		LobsterMap('27-33')
		with(TempModel$Data,points(X,Y,pch='.',col=rgb(1,0,0,0.1)))
		if(graphic!="R")dev.off()
	}

	if(2%in%type){
		# Seasonal trend
		for(a in 1:length(Area)){

			if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures',paste0('TempModel',Area[a],'.png')),width=wd,height=ht,units='in',res=200)
			if(graphic=='R')x11( width=wd, height=ht)

			y =	seq(min(TempModel$Data$y),max(TempModel$Data$y),0.01)


			plot(TEMPERATURE~y,data=TempModel$Data,pch='.',col=rgb(0.5,0.5,0.5,0.05),xlab='',main=Area[a],...)
		

			points(TEMPERATURE~y,data=subset(TempModel$Data,area==Area[a]),pch=16,cex=0.2,col=scales::alpha(colors[a],0.2))

			for(i in 1:length(depths)){

				t=predict(TempModel$Model,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=depths[i],area=Area[a]),type='response')
				lines(y,t,lty=i+1,col=colors[a])

			}
			legend('topright',legend=depths,lty=1:length(depths)+1,col=colors[a],bty='n',title="Depth (m)")
			if(graphic!="R")dev.off()

		}
	}

	if(3%in%type){
		# Annual trend
		if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','TempModelAnnual.png'),width=7,height=10,units='in',res=200)
		if(graphic=='R')x11( width=7, height=8)

		if(missing(yrs))yrs=floor(min(TempModel$Data$y)):floor(max(TempModel$Data$y))
		y =	decimal_date(as.Date(paste(yrs,idate,sep='-')))

		par(mfrow=c(length(Area),1),mar=c(0,5,0,5),omi=c(0.75,0,0.5,0),las=1)

		temp=list()

		for(a in 1:length(Area)){
			y =	decimal_date(as.Date(paste(yrs,idate,sep='-')))
			if(Area[a]=="29"&&yrs[1]<1997)y =	decimal_date(as.Date(paste(1997:max(yrs),idate,sep='-')))

			p=predict(TempModel$Model,data.frame(y=y,cos.y=cos(2*pi*y),sin.y=sin(2*pi*y),DEPTH=25,area=Area[a]),type='response',se.fit=T)

			temp[[a]]=data.frame(Area=Area[a],y=y,t=p$fit,t.se=p$se.fit)

			plot(y,p$fit,xlab='',ylab='',type='l',ylim=c(0,9.5),col=colors[1],xaxt='n',xlim=range(yrs))
			polygon(c(y,rev(y)),c(p$fit+p$se.fit,rev(p$fit-p$se.fit)),col=alpha(colors[1],0.5),border=NA)
			axis(1,lab=F)
			axis(4)
			if(a==length(Area))axis(1)
			grid(lty=3,col='grey')
			mtext(Area[a],3,line=-1.5,adj=0.03)


		}
		mtext("Temperature (C)",2,outer=T,line=-2,las=0)
		mtext("Year",1,outer=T,line=3)

		if(graphic!="R")dev.off()
		return(do.call("rbind",temp))
	}


	if(4%in%type){
		if(graphic=='png')png(file.path(project.datadirectory('bio.lobster'),'figures','TempDataAnnual.png'),width=7,height=10,units='in',res=200)
		if(graphic=='R')x11( width=7, height=10)
	 	LobTempData=read.csv(file.path( project.datadirectory("bio.lobster"),"Temperature Data","LobsterTempData2.csv"))
	 	if(lfa)LobTempData$subarea = LobTempData$LFA
		if(missing(yrs))yrs=sort(unique(LobTempData$SYEAR))
			
		par(mfrow=c(length(Area),1),mar=c(0,5,0,5),omi=c(0.75,0,0.5,0),las=1)

		fsrsTemp=list()
	 	for(i in 1:length(Area)){
	 		t=with(subset(LobTempData,subarea==Area[i]),tapply(TEMP,SYEAR,mean))
	 		t.sd=with(subset(LobTempData,subarea==Area[i]),tapply(TEMP,SYEAR,sd))
	 		y=as.numeric(names(t))
	 		fsrsTemp[[i]] = data.frame(area=Area[i],year=y,t=t,t.sd=t.sd)
			plot(y,t,xlab='',ylab='',type='b',pch=16,ylim=c(0,13),col=colors[1],xaxt='n',xlim=range(yrs))
				segments(y,t-t.sd,y,t+t.sd,col=colors[1])
				axis(1,lab=F)
				axis(4)
				if(i==length(Area))axis(1)
				grid(lty=3,col='grey')
				mtext(Area[i],3,line=-1.5,adj=0.03)

	 	}
		mtext("Temperature (C)",2,outer=T,line=-2,las=0)
		mtext("Year",1,outer=T,line=3)

		if(graphic!="R")dev.off()
		fsrsTemp = do.call("rbind",fsrsTemp)
		return(fsrsTemp)
	}



	

}
	