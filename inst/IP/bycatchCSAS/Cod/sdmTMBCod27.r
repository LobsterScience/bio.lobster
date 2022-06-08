#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA27.R

rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA27.rds')

aT = u$data
bspde = u$grid
be = u$preds

 fit = sdmTMB(CodWt~
 				s(lZ,k=5),
 				data=aT,
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				)

 
go =predict(fit) 
go$pred = fit$family$linkinv(go$est)
saveRDS(go,file='results/codFullPred27.rds')
be = subset(be,WOS==1)
g = predict(fit,newdata=be,nsim=50)
g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)


saveRDS(list(fit,be),file='codsdmTMB27.rds')
if(reload){
  r = readRDS(file='lobstersdmTMB.rds')
  fit=r[[1]]
  be=r[[2]]
}

gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = rL[rL$LFA==27,]
st_crs(rL) <- 4326
crs_utm20 <- 32620
rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -62, ymin = 45.5, xmax = -58, ymax = 47.5))))
rL <- st_transform(rL, crs_utm20)

png('Figures/ModelOutput/codsdmTMB27.png')
mm = c(0.0,.08)
ggplot(subset(gsf)) +
			geom_sf(aes(fill=pred,color=pred)) + 
  		scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
	    geom_sf(data=rL, size=1, color='black',fill=NA)+
  			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()

ag = aggregate(cbind(pred,lQ,uQ)~SID+PID+WOS,data=be,FUN=median)
#ag = aggregate(cbind(pred)~SID+PID+WOS,data=be,FUN=median)

ef = readRDS('results/BumpedUpEffortByGridNUM.rds')
ef = subset(ef,LFA %in% 27)
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+WOS+LFA,data=ef,FUN=mean)
names(ef)[c(1,3)] = c('SID','PID')

ff = merge(ag,ef)

ff$L = ff$pred*ff$BTTH
ff$Ll = ff$lQ*ff$BTTH
ff$Lu = ff$uQ*ff$BTTH

L = aggregate(cbind(L,Ll,Lu)~PID,data=ff,FUN=sum)

#residual plots from slips v predicted to show gaps

el = readRDS('results/LandingsByGridNUM.rds')
names(el)[c(1,3)] = c('SID','PID')
el = aggregate(WEIGHT_KG~SID+WOS+PID,data=el,FUN=mean)

fl = merge(ff,el)
fl$err = (fl$L-fl$WEIGHT_KG)/(fl$WEIGHT_KG)
fl$Rediduals=fl$L-fl$WEIGHT_KG

saveRDS(fl,file='results/LandingsPredictedActual27.rds')

