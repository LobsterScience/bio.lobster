require(bio.lobster)
require(bio.utilities)
require(devtools)
require(sf)
la()

a = compileAbundPresAbs(redo=F,size=F)
a = subset(a,SOURCE %in% c('FSRS_RECRUITMENT_TRAPS','FSRS_COMMERCIAL_TRAPS','AT_SEA_SAMPLES'))
a$ID = 1:nrow(a)
p = bio.lobster::load.environment()
p = spatial_parameters(type='canada.east')

attr(a,'projection') = "LL"
aA = lonlat2planar(a,input_names=c('LONGITUDE','LATITUDE'),proj.type =  "lambert.conic.canada.east")

#allocating depth to location
ba = lobster.db('bathymetry')
locsmap = match( 
  array_map( "xy->1", aA[,c("plon","plat")], gridparams=p$gridparams ), 
  array_map( "xy->1", ba[,c("plon","plat")], gridparams=p$gridparams ) )
baXY = planar2lonlat(ba,proj.type=p$internal.projection)
aA$Depth = ba$z[locsmap]

aS = subset(aA,Depth>0 & Depth <500,select=c('DATE','LONGITUDE','LATITUDE','WEIGHT_KG','SOURCE','Depth'))
aS = st_as_sf(aS,coords = c('LONGITUDE','LATITUDE'),crs=4326)

l = readRDS(file.path( layerDir,"LFAPolysSF.rds"))
idf = st_join(aS,l)
idf=subset(idf,LFA %in% c(27,29,30,311,312,32))
deps = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.99)))
deps95 = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.95)))
deps75 = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.75)))

#just prune the polys
load(file=file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-33100mIsobath.rdata')) #Isob100 the 100 m isobath for 27-33

load(file=file.path(project.datadirectory('bio.lobster'),'data','maps','bathy','bathy1Poly1.rdata')) 


LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
LFAgrid$UID = paste(LFAgrid$PID,LFAgrid$SID,sep="-")
LG =subset(LFAgrid,PID %in% c(27,29,30,311,312,32))
UP = 1:1000000
out=list()
j=0
#io = c(27,29,30,311,312,32,33)
io = unique(LG$UID)
for(i in io){
  j=j+1
  ij = subset(LG,UID==i)
  d = subset(deps,LFA==unique(ij$PID))
  a= joinPolys(ij,subset(bathy.poly,Z==d$Depth),operation='INT')
  a$LFA=unlist(strsplit(i,'-'))[[1]]
  a$GRID_NO=unlist(strsplit(i,'-'))[[2]]
  attr(a,'projection') <- "LL"
  if(j==1){
    out=a
    UP = setdiff(UP,unique(out$PID))
  } else {
   UIP = unique(out$PID)
   ap = unique(a$PID)
   if(any(ap %in% UIP)){
     m = which(ap %in% UIP)
     UP = setdiff(UP,ap[m])
      for(v in 1:length(m)){
        browser()
        z = which(a$PID==ap[m[v]])
        a$PID[z]=sample(UP,size=1)
      }
   }
   out = rbind(out,a)
  }
}

