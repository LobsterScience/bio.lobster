#NOAA presentation

ggLobsterMap('27-38')
lobster.db('survey.redo')

g = ggLobsterMap('34')

x = subset(surveyCatch, YEAR==2022)
xx = st_as_sf(x,coords = c('SET_LONG','SET_LAT'),crs=4326)

g+geom_sf(data=subset(xx,LFA=='L34'))
xx = subset(partLandings,LFA %in% c(33,34,35) & SYEAR==2021)
xx
xxx = aggregate(BL~GRID_NUM+LFA,data=xx,FUN=sum)
xxx$Z = xxx$BL/1000
ux = c(min(xxx$Z),max(xxx$Z))

g = ggLobsterMap('34',bathy=T,attrData = subset(xxx,LFA==34),legLab='Landings (t)',addLFALabels = F,scaleTrans = 'identity' ,brks=ux)

l = readRDS(file.path( layerDir,"LFAPolysSF.rds"))


b = file.path( project.datadirectory("bio.lobster"), "data","maps","summerstrata.csv")
b = read.csv(b)
names(b) <- c('PID','X','Y')
b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
attr(b,'projection') <- "LL"
#addPolys(b,lty=1,border='blue',col=adjustcolor('white',alpha.f=1))
r = pbs.2.gis(b,make.sf = T,env.object = T,type='polygon',spdf = F,layer.names = 'PID')
r = bio.utilities::list.names.to.columns(r)
rr = aggregate(X~PID,data=b,FUN=max)
r$Label = rr$PID
saveRDS(r,file.path( project.datadirectory("bio.lobster"), "data","maps","RVSurveyPolys.rds"))



g+geom_sf(data=r)+coord_sf(xlim=c(-67.8,-65), ylim=c(42.5,45))

b = importShapefile(file.path( project.datadirectory("bio.lobster"), "data","maps","BTS_Strata"), readDBF=F)
b$Label = paste(b$PID,b$SID,sep="-")
r = pbs.2.gis(b,make.sf = T,env.object = T,type='polygon',spdf = F,layer.names = 'Label')
r = bio.utilities::list.names.to.columns(r)
r$Label = r$V2
r$V2=NULL
saveRDS(r,file.path( project.datadirectory("bio.lobster"), "data","maps","NEFSCSurveyPolys.rds"))

g+geom_sf(data=r,alpha=0.8)+coord_sf(xlim=c(-67.8,-65), ylim=c(42.5,45))


#adding in stations
x = groundfish.db('gsinf.odbc')
x
unique(year(x$sdate))
x = subset(x,year(sdate)==2021 & month(sdate) %in% c(7,8))
x$X = convert.dd.dddd(x$slong)
x$Y = convert.dd.dddd(x$slat)
x$X = x$X * -1


x = st_as_sf(x,coords=c('X','Y'),crs=4326)

#ILTS
g1 = g+geom_sf(data=subset(xx,LFA %in% c('L34','L35','L36','L38')))

g2 = g1+geom_sf(data=x,shape=8)+coord_sf(xlim=c(-67.8,-65), ylim=c(42.5,45))

#nefsc
nefsc.db('usinf.odbc.redo')
y=nefsc.db('usinf.clean.redo')
y = subset(y,GMT_YEAR==2021)
y = st_as_sf(y,coords = c('X','Y'),crs=4326)

g2 + geom_sf(data=y,shape=2)+coord_sf(xlim=c(-67.8,-65), ylim=c(42.5,45))