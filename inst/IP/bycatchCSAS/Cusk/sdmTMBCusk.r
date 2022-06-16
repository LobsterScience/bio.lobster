#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA33-35.R
rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA33-35.rds')

aT = u$data
bspde = u$grid
be = u$pred

 fit = sdmTMB(CuskWt~
 				s(lZ,k=5)+DID,
 				data=aT,
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				)


g = predict(fit,newdata=be,nsim=50)
g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)

gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)


saveRDS(list(fit,be),file='CusksdmTMBsims.rds')
if(reload){
	r = readRDS(file='CusksdmTMBsims.rds')
	fit=r[[1]]
	be=r[[2]]
	}


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = rL[rL$LFA %in% c(33:35),]
st_crs(rL) <- 4326
crs_utm20 <- 32620
#rL = rL[-which(!(st_is_valid(rL))),]
rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -67.5, ymin = 42, xmax = -62.1, ymax = 46))))
rL <- st_transform(rL, crs_utm20)
gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)


png('Figures/ModelOutput/CusksdmTMB.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
mm = c(0.,max(gsf$pred))
ggplot(subset(gsf,WOS %in% 1)) +
			geom_sf(aes(fill=pred,color=pred)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			geom_sf(data=rL,color='black',size=1,fill=NA) +
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()

ag = aggregate(cbind(pred,lQ,uQ)~SID+PID,data=be,FUN=mean)



ef = readRDS('results/BumpedUpEffortByGridNUM.rds')
ef = subset(ef,LFA %in% 33:35)
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+LFA,data=ef,FUN=function(x) sum(x)/3)
names(ef)[c(1,2)] = c('SID','PID')

ff = merge(ag,ef)

ff$L = ff$pred*ff$BTTH
ff$Ll = ff$lQ*ff$BTTH
ff$Lu = ff$uQ*ff$BTTH

L = aggregate(cbind(L,Ll,Lu)~PID,data=ff,FUN=sum)
