
#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA31A31B.R

rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA31A31B.rds')

aT = u$data
bspde = u$grid
be = u$preds
rL = u$rL

#LFAs for prediction grids

aT$lZ = log(aT$Depth)
 fit = sdmTMB(LegalWt~
 				s(lZ,k=5),
 				data=aT,
 				time='SYEAR', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				spatiotemporal='ar1'
 				)

be = subset(be,WOS%in%1:2)
be$SYEAR = ifelse(be$WOS==1,2018,2019)
be$WOS=NULL
g = predict(fit,newdata=be,nsim=50)

g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)


saveRDS(list(fit,be),file='lobstersdmTMB31A32B.rds')
if(reload){
      r = readRDS(file='lobstersdmTMB31A32B.rds')
      fit=r[[1]]
      be=r[[2]]

}


gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)

png('Figures/ModelOutput/lobstersdmTMBwk1-12.png')
mm = c(0.,max(gsf$pred))
ggplot(gsf) +
			geom_sf(aes(fill=pred,color=pred)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			facet_wrap(~SYEAR) +
      geom_sf(data=rL, color='black',fill=NA,size=.5)+
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()

ag = aggregate(cbind(pred,lQ,uQ)~SID+PID+SYEAR,data=be,FUN=mean)
#ag = aggregate(cbind(pred)~SID+PID+WOS,data=be,FUN=median)

ef = readRDS('results/BumpedUpEffortByGridNUM.rds')
ef = subset(ef,LFA %in% c('31A','31B'))
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+SYEAR+LFA,data=ef,FUN=sum)
names(ef)[c(1,3)] = c('SID','PID')
ag$PID = ifelse(ag$PID==311,'31A','31B')
ff = merge(ag,ef)

ff$L = ff$pred*ff$BTTH
ff$Ll = ff$lQ*ff$BTTH
ff$Lu = ff$uQ*ff$BTTH

L = aggregate(cbind(L,Ll,Lu)~PID+SYEAR,data=ff,FUN=sum)

#residual plots from slips v predicted to show gaps

el = readRDS('results/LandingsByGridNUM.rds')
names(el)[c(1,3)] = c('SID','PID')
el = aggregate(WEIGHT_KG~SID+PID+SYEAR,data=el,FUN=sum)

fl = merge(ff,el)
fl$err = (fl$L-fl$WEIGHT_KG)/(fl$WEIGHT_KG)
fl$Rediduals=fl$L-fl$WEIGHT_KG

saveRDS(fl,file='results/LandingsPredictedActual31ab.rds')

