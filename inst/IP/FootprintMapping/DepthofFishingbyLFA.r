require(bio.lobster)
require(bio.utilities)
require(devtools)
require(sf)
la()
require(ggplot2)

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

idf$RDepth = round(idf$Depth)
deps = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.99)))

idf1 = idf
idf1$LFA = ifelse(idf1$LFA==311,'31A',idf1$LFA)
idf1$LFA = ifelse(idf1$LFA==312,'31B',idf1$LFA)
ii = aggregate(DATE~LFA,data=idf1,FUN=length)
names(ii)[2] = 'n_Traps'
idf1=merge(idf1,ii)
ggplot(subset(idf1,Depth<150),aes(x=RDepth))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  facet_wrap(~LFA)+
  geom_text(aes(label=paste('nTraps= ',n_Traps)),x=110,y=0.100)+
  


deps = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.99)))
deps95 = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.95)))
deps75 = aggregate(Depth~LFA,data=idf,FUN=function(x)round(quantile(x,0.75)))

#replace depths with those from Effort survey 
deps$Depth[which(deps$LFA==29)] = round(1.8288*40)
deps$Depth[which(deps$LFA==30)] = round(1.8288*25)


#just prune the polys

load(file=file.path(project.datadirectory('bio.lobster'),'data','maps','bathy','bathy1Poly1.rdata')) 


LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
LFAgrid$UID = paste(LFAgrid$PID,LFAgrid$SID,sep="-")
LG =subset(LFAgrid,PID %in% c(27,29,30,311,312,32))
#LG =subset(LFAgrid,PID %in% c(27))

UP = 1:1000000
out=list()
j=0
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
        z = which(a$PID==ap[m[v]])
        a$PID[z]=sample(UP,size=1)
      }
   }
   out = rbind(out,a)
  }
}

LobsterMap('27')
addPolys(out,col='red')
coast<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs","shorelineHR.csv"))
#stmpa = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","StAnnsMPA.csv"))

coast$label=paste(coast$PID,coast$SID,sep="-")
#b = pbs.2.gis(b,spdf = F,env.object = T,type='polygon',make.sf = T)
#b = bio.utilities::list.names.to.columns(b)
#saveRDS(coa,file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds"))


#joining to coast
UP = 1:1000000
#LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
#LFAgrid$UID = paste(LFAgrid$PID,LFAgrid$SID,sep="-")

out$UID = paste(out$PID,out$SID,sep="-")
ui = unique(out$UID)

for(i in 1:length(ui)){
  oj = subset(out,UID==ui[i]) 
  oj = oj[order(oj$POS),]
  l = unique(oj$LFA)
  g = unique(oj$GRID_NO)
  v=joinPolys(oj,coast,operation = 'DIFF')
  if(length(v)>0){
  if(i==1){
    nout=v
    nout$LFA=l
    nout$GRID_NO=g
    
    UP = setdiff(UP,unique(nout$PID))
  } else {
    UIP = unique(nout$PID)
    ap = unique( v$PID)
  if(any(ap %in% UIP)){
      m = which(ap %in% UIP)
      UP = setdiff(UP,ap[m])
      for(f in 1:length(m)){
        z = which(v$PID==ap[m[f]])
        v$PID[z]=sample(UP,size=1)
      }
  }
    v$LFA = l
    v$GRID_NO = g
    nout = rbind(nout,v)
    rm(v)
    
  }
}
}

g = subset(nout,LFA==311 & Y<45.1)

nout = subset(nout,PID %ni% unique(g$PID))
g = subset(nout,LFA==27 & X> -59.3)
nout = subset(nout,PID %ni% unique(g$PID))
g = subset(nout,LFA==312 & Y<44.7)
nout = subset(nout,PID %ni% unique(g$PID))

nout$label = paste(nout$LFA,nout$GRID_NO,sep="-")
g = pbs.2.gis(nout,spdf = F,env.object = T,type='polygon',make.sf = T)
g = bio.utilities::list.names.to.columns(g)
g$GRID_NO=sapply(g$V2,function(x) unlist(strsplit(x,"-"))[[2]])
g$LFA=sapply(g$V2,function(x) unlist(strsplit(x,"-"))[[1]])
saveRDS(g,file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-32GridsPrunedtoDepth-sf.rds'))
g1=readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-32GridsPrunedtoDepth-sf.rds'))

#rest of lfas
LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
LFAgrid$UID = paste(LFAgrid$PID,LFAgrid$SID,sep="-")
LG =subset(LFAgrid,PID %in% c(28,33:38))
UP = 1:1000000
coast<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","gshhs","shorelineHR.csv"))
out=LG
out$LFA=out$PID
out$GRID_NO=out$SID

ui = unique(out$UID)

for(i in 1:length(ui)){
  oj = subset(out,UID==ui[i]) 
  oj = oj[order(oj$POS),]
  l = unique(oj$LFA)
  g = unique(oj$GRID_NO)
  v=joinPolys(oj,coast,operation = 'DIFF')
  if(length(v)>0){
    if(i==1){
      nout=v
      nout$LFA=l
      nout$GRID_NO=g
      
      UP = setdiff(UP,unique(nout$PID))
    } else {
      UIP = unique(nout$PID)
      ap = unique( v$PID)
      if(any(ap %in% UIP)){
        m = which(ap %in% UIP)
        UP = setdiff(UP,ap[m])
        for(f in 1:length(m)){
          z = which(v$PID==ap[m[f]])
          v$PID[z]=sample(UP,size=1)
        }
      }
      v$LFA = l
      v$GRID_NO = g
      nout = rbind(nout,v)
      rm(v)
      
    }
  }
}

nout$label = paste(nout$LFA,nout$GRID_NO,sep="-")
g = pbs.2.gis(nout,spdf = F,env.object = T,type='polygon',make.sf = T)
g = bio.utilities::list.names.to.columns(g)
g$GRID_NO=sapply(g$V2,function(x) unlist(strsplit(x,"-"))[[2]])
g$LFA=sapply(g$V2,function(x) unlist(strsplit(x,"-"))[[1]])

gFINAL = rbind(g,g1)
#dealing with 37

g37 = subset(gFINAL,LFA==37)
g37$LFA=38

g38 = subset(gFINAL,LFA==37)
g38$LFA=36

gFINAL = rbind(rbind(gFINAL,g37),g38)

saveRDS(gFINAL,file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
gFINAL= readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))



