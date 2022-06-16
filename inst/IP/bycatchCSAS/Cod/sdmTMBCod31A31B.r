
#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA31A31B.R

rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA31A31B.rds')

aT = u$data
bspde = u$grid
be = u$preds
rL = u$rL

aT$lZ = log(aT$Depth)
 fit = sdmTMB(CodWt~
 				s(lZ,k=5),
 				data=aT,
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on'
 				)


be = subset(be,WOS==1 & SYEAR==2019)

g = predict(fit,newdata=be,nsim=50)
g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)



saveRDS(list(fit,be),file='codsdmTMB31A31B.rds')
if(reload){
    r = readRDS(file='codsdmTMB31A31B.rds')
    fit=r[[1]]
    be=r[[2]]
}
gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)

png('Figures/ModelOutput/codsdmTMB31ab.png')

ggplot(gsf) +
			geom_sf(aes(fill=pred,color=pred),size=1.8) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
      geom_sf(data=rL,fill=NA,colour='black',size=1)+
			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
      guides(size="none") +
 			coord_sf()
dev.off()

ag = aggregate(cbind(pred,lQ,uQ)~SID+PID,data=be,FUN=mean)
#ag = aggregate(cbind(pred)~SID+PID+WOS,data=be,FUN=median)

ef = readRDS('results/BumpedUpEffortByGridNUM.rds')
ef = subset(ef,LFA %in% c('31A','31B'))
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+SYEAR+LFA,data=ef,FUN=sum)
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+LFA,data=ef,FUN=mean)

names(ef)[c(1,2)] = c('SID','PID')
ag$PID = ifelse(ag$PID==311,'31A','31B')
ff = merge(ag,ef)

ff$L = ff$pred*ff$BTTH
ff$Ll = ff$lQ*ff$BTTH
ff$Lu = ff$uQ*ff$BTTH

L = aggregate(cbind(L,Ll,Lu)~PID,data=ff,FUN=sum)

#residual plots from slips v predicted to show gaps

el = readRDS('results/LandingsByGridNUM.rds')
names(el)[c(1,3)] = c('SID','PID')
el = aggregate(WEIGHT_KG~SID+PID+SYEAR,data=el,FUN=sum)

fl = merge(ff,el)
fl$err = (fl$L-fl$WEIGHT_KG)/(fl$WEIGHT_KG)
fl$Rediduals=fl$L-fl$WEIGHT_KG

saveRDS(fl,file='results/LandingsPredictedActual31ab.rds')

