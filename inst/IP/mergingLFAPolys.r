require(PBSmapping)

a = importShapefile(file.path(project.datadirectory('bio.lobster'),'data','maps','MaineLobsterAreas/StatAreas_Lobster_SmoothCoast'),readDBF=T) 
          attr(a,'projection') <- "LL"
           l = attributes(a)$PolyData[,c('PID','Id')]
           l = subset(l,PID %in% c(3,4,5,6))
USareas = merge(a,l,by='PID')
USareas = USareas[order(USareas$PID,USareas$SID,USareas$POS),]
USareas = subset(USareas,SID==1)       
USareas$PID=USareas$Id
USareas$Id = NULL

	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		sLFAs = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","sGSLLFAPolys.csv"))
		sLFAs$X.1 = NULL

		L = LFAs = rbind(rbind(LFAs,sLFAs),USareas)

L$label = paste(L$PID,L$SID,sep="-")
r = pbs.2.gis(L,make.sf = T,env.object = T,type='polygon',spdf = F)
r = bio.utilities::list.names.to.columns(r)
r$LFA =as.data.frame(do.call(rbind,(strsplit(r$V2,'-'))))[,1]
o = list()
i = unique(r$LFA)
xy = aggregate(V2~LFA,data=r,FUN=length)
xx = xy[which(xy$V2>1),1]

for(l in 1:length(xx)){
g = subset(r,LFA==xx[l])
g1 = st_union(g,by_feature=F,is_coverage=T)
g1$LFA=xx[l]
o[[l]] = g1
}

r1 = do.call(rbind,o)

#saveRDS(r2,file.path( project.datadirectory("bio.lobster"), "data","maps","bathy100SF.rds"))
