# source("fn/Relief.plots.r")

Relief.plots<-function(tows,MBdata='from.file',expd=1,graphic="pdf",digits=4,file="GerTow",kms=1,gerfiles=1:77,key=file.key,tracks=F,trackPath=file.path( project.directory("lobster"),"data","log files","TE17","German")){

	require(PBSmapping)
	require(CircStats)
	dpkm<-0.008983346
	aspr=1/cos(rad(mean(tows$Y)))
	key<-read.csv(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","fileKey.csv"))
	
	
	for(i in 1:nrow(tows)){
		print(tows$EID[i])
	
		if(graphic=="pdf")pdf(paste(file,tows$EID[i],".pdf",sep=''),8.5,8.5)
		if(graphic=="R")x11(11,11)
	
		if(MBdata=='from.file'){
					
			gets<-gerfiles[key$X1<tows$X[i]&key$X2>tows$X[i]&key$Y1<tows$Y[i]&key$Y2>tows$Y[i]]
			locdata.lst<-list()
			if(length(gets)>0){
				for(f in 1:length(gets)){
					tmp <-read.table(file.path( project.datadirectory("bathymetry"),"data","GermanBathy","gerbk_5",paste("german_",gets[f],".txt",sep='')),header=T)
					locdata.lst[[f]]<-subset(tmp,X<(tows$X[i]+dpkm*aspr*kms)&X>(tows$X[i]-dpkm*aspr*kms)&Y<(tows$Y[i]+dpkm*kms)&Y>(tows$Y[i]-dpkm*kms))
				}
				rm(tmp)
				loc.data<-do.call('rbind',locdata.lst)[,2:4]
			}
			else loc.data<-data.frame()
			
		}
		else {
			names(MBdata)<-c("X","Y","Z")
			loc.data<-subset(MBdata,X<(tows$X[i]+dpkm*kms*aspr)&X>(tows$X[i]-dpkm*kms*aspr)&Y<(tows$Y[i]+dpkm*kms)&Y>(tows$Y[i]-dpkm*kms))
		}
		if(nrow(loc.data)>0){
			towMB.lst<-makeTopography(loc.data,digits=digits)
			
			if(length(towMB.lst$x)>1&&length(towMB.lst$y)>1){
		#browser()
				res<-persp(towMB.lst$x,towMB.lst$y,towMB.lst$z,col="lightblue",shade=0.5,border=NA,zlim=c(-120,0),phi=50,expand=expd,ticktype="detailed",zlab="Depth",xlab='',ylab='')
				if('slat'%in%names(tows)){
					if(tracks){
						dis<-dist.coef(tows$EID[i]-1000,path=trackPath,w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[2]]
						with(dis,lines(trans3d(X,Y,-tows$depth[i],pmat=res),col='red',lwd=2))
					}
					else with(tows,lines(trans3d(c(slon[i],elon[i]),c(slat[i],elat[i]),-depth[i],pmat=res)))
					with(tows,points(trans3d(slon[i],slat[i],-depth[i],pmat=res),pch=16,col='red'))
					with(tows,points(trans3d(elon[i],elat[i],-depth[i],pmat=res),pch=16))
				}
				else with(tows,points(trans3d(X[i],Y[i],mean(towMB.lst$z,na.rm=T),pmat=res),pch=16,col='red'))
				title(paste("Station",tows$STATION[i]),cex=1.5,adj=0.9)
				title(paste("EID",tows$EID[i]),cex=1.5,adj=0.1)
			}
		}
		if(graphic!="R")dev.off()
		print(Sys.time())
		gc()
	}
}


