#	source("Y:/INSHORE SCALLOP/Survey/2010/r/fn/genran")

# Generates random points in a polygon (PBSmapping format)

genran<-function(npoints,bounding.poly,projection="LL",mindist=NULL){

	require(spatstat)|| stop("Install spatstat Package")
	require(splancs)|| stop("Install splancs Package")
	require(PBSmapping)|| stop("Install PBSmapping Package")
	

	# create pool of random points
	bound.pts<-as.points(list(x=bounding.poly$X,y=bounding.poly$Y))
	
	
	if(is.null(mindist)){
		pool.EventData<-data.frame(1:npoints,csr(bound.pts,npoints))
		attr(pool.EventData,"projection")<-projection
		names(pool.EventData)<-c("EID","X","Y")
	}
	
	if(!is.null(mindist)){
		pool.EventData<-data.frame(1:npoints,csr(bound.pts,npoints))
		attr(pool.EventData,"projection")<-projection
		names(pool.EventData)<-c("EID","X","Y")
		
		if(projection=="LL")pool.EventData<-convUL(pool.EventData)
		W<-owin(range(pool.EventData$X),range(pool.EventData$Y))
		pool.ppp<-as.ppp(subset(pool.EventData,select=c('X','Y')),W)
		pool.EventData$nndist<-nndist(pool.ppp)
		if(projection=="LL")pool.EventData<-convUL(pool.EventData)
	
			
		for(i in 1:npoints){
			if(pool.EventData$nndist[i]>mindist) next
			else if(pool.EventData$nndist[i]<mindist){
				repeat{
					pool.EventData[i,c("X","Y")]<-csr(bound.pts,1)
					if(projection=="LL")pool.EventData<-convUL(pool.EventData)
					W<-owin(range(pool.EventData$X),range(pool.EventData$Y))
					pool.ppp<-as.ppp(subset(pool.EventData,select=c('X','Y')),W)
					pool.EventData$nndist<-nndist(pool.ppp)
					if(projection=="LL")pool.EventData<-convUL(pool.EventData)
					if(pool.EventData$nndist[i]>mindist) break
				}
			}
		}
	}

	pool.EventData
		
}
