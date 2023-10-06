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

deps = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.99)))
deps95 = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.95)))
deps75 = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.75)))


