#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA33-35.R
rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA33-35.rds')

aT = u$data
bspde = u$grid
be = u$preds

#LFAs for prediction grids
aT$PA = ifelse(aT$JonahWt>0,1,0)
aT$lZ = log(aT$Depth)
 fitH = sdmTMB(PA~s(lZ,k=3)+DID,
 				data=aT,
 				time='WOS',
 				extra_time=33,
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				spatialtemporal='ar1'
 				)


aT2 = subset(aT,JonahWt>0)
bspde2 = make_mesh(aT2,xy_cols=c('X1000','Y1000'),mesh=bspde$mesh)

 fitD = sdmTMB(JonahWt~
 				s(lZ,k=5)+DID,
 				data=aT2,
 				#time='WOS', 
 				mesh=bspde2, 
 				#extra_time=33,
 				family=Gamma(link='log'),
 				spatial='on',
 				#spatialtemporal='ar1'
 				)


g = predict(fitH,newdata=be,nsim=50)
g2 = predict(fitD,newdata=be,nsim=50)


g_bin = fitH$family$linkinv(g)
g2_bin = fitD$family$linkinv(g2)


be$predBin = apply(g_bin,1,median)
be$predBinL = apply(g_bin,1,quantile,0.25)
be$predBinU = apply(g_bin,1,quantile,0.75)


be$predGam = apply(g2_bin,1,median)
be$predGamL = apply(g2_bin,1,quantile,0.25)
be$predGamU = apply(g2_bin,1,quantile,0.75)



be$predC = be$predBin * be$predGam
be$predCL = be$predBinL*be$predGamL
be$predCU = be$predBinU*be$predGamU


saveRDS(list(fitH,fitD,be),file='jonahsdmTMB.rds')


reload=F
if(reload){
  r = readRDS(file='jonahsdmTMB.rds')
  fitH=r[[1]]
  fitD=r[[2]]
  be=r[[3]]
}

gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = rL[rL$LFA %in% c(33:35),]
st_crs(rL) <- 4326
crs_utm20 <- 32620
rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -67.5, ymin = 42, xmax = -62.1, ymax = 46))))
rL <- st_transform(rL, crs_utm20)



png('Figures/ModelOutput/JonahsdmTMB.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
mm = c(0.,max(gsf$predC))
ggplot(subset(gsf,WOS %in% c(10))) +
			geom_sf(aes(fill=predC,color=predC)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			geom_sf(data=rL, size=1,color='black',fill=NA)+
			#facet_wrap(~WOS) +
			geom_sf(rL,size=1,color='black',fill=NA ) +
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()


ag = aggregate(cbind(predC,predCL,predCU)~SID+PID+WOS,data=be,FUN=mean)


ef = readRDS('results/BumpedUpEffortByGridNUM.rds')
ef = subset(ef,LFA %in% 33:35)
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+WOS+LFA,data=ef,FUN=mean)
ef$WOS  = ifelse(ef$LFA %in% 33:34,ef$WOS+6,ef$WOS)
names(ef)[c(1,3)] = c('SID','PID')

ff = merge(ag,ef)

ff$L = ff$predC*ff$BTTH
ff$LL = ff$predCL*ff$BTTH
ff$LU = ff$predCU*ff$BTTH

L = aggregate(cbind(L,LU,LL)~PID,data=ff,FUN=sum)

