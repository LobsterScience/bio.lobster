#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA33-35.R
rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA33-35.rds')

aT = u$data
bspde = u$grid
be = u$preds

fit = sdmTMB(LegalWt~
 				s(lZ,k=5)+DID,
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
				extra_time=c(33,41),
 				family=tweedie(link='log'),
 				spatial='on',
 				spatiotemporal='ar1')


go =predict(fit) 
go$pred = fit$family$linkinv(go$est)
saveRDS(go,file='results/lobsterFullPred.rds')

g = predict(fit,newdata=be,nsim=50)
g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)


saveRDS(list(fit,be),file='lobstersdmTMB.rds')

reload=F
if(reload){
  r = readRDS(file='lobstersdmTMB.rds')
  fit=r[[1]]
  be=r[[2]]
}

gsf = st_as_sf(be,coords = c("X","Y"),crs=32620,remove=F)

rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
st_crs(rL) <- 4326
crs_utm20 <- 32620
rL = rL[-which(!(st_is_valid(rL))),]
rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -67.5, ymin = 42, xmax = -62.5, ymax = 46))))

ns_coast <- st_transform(ns_coast, crs_utm20)
rL <- st_transform(rL, crs_utm20)



#Maps
png('Figures/ModelOutput/lobstersdmTMBwk1-12.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,WOS %in% 1:12)) +
			geom_sf(aes(fill=pred,color=pred)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			facet_wrap(~WOS) +
			geom_sf(data=rL,size=.7,colour='black',fill=NA)+
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()



png('Figures/ModelOutput/lobstersdmTMBwk13-24.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
ggplot(subset(gsf,WOS %in% 13:24)) +
			geom_sf(aes(fill=pred,color=pred)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			facet_wrap(~WOS) +
 			geom_sf(data=rL,size=.7,colour='black',fill=NA)+
 
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()

	png('Figures/ModelOutput/lobstersdmTMBwk25-36.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
	ggplot(subset(gsf,WOS %in% 25:36)) +
				geom_sf(aes(fill=pred,color=pred)) + 
				scale_fill_viridis_c(trans='sqrt',limits=mm) +
				scale_color_viridis_c(trans='sqrt',limits=mm) +
				facet_wrap(~WOS) +
	 			geom_sf(data=rL,size=.7,colour='black',fill=NA)+
 
	 			theme( axis.ticks.x = element_blank(),
	        		   axis.text.x = element_blank(),
					   axis.title.x = element_blank(),
					   axis.ticks.y = element_blank(),
	        		   axis.text.y = element_blank(),
	        		   axis.title.y = element_blank()
	        		   ) +
	 			coord_sf()
	dev.off()


	png('Figures/ModelOutput/lobstersdmTMBwk37-40.png', width = 10, height = 12,units='in',pointsize=12, res=300,type='cairo')
	ggplot(subset(gsf,WOS %in% 37:40)) +
				geom_sf(aes(fill=pred,color=pred)) + 
				scale_fill_viridis_c(trans='sqrt',limits=mm) +
				scale_color_viridis_c(trans='sqrt',limits=mm) +
				facet_wrap(~WOS) +
	geom_sf(data=rL,size=.7,colour='black',fill=NA)+
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

ef = readRDS('results/BumpedUpEffortByGridNUM.rds')
ef = subset(ef,LFA %in% 33:35)
ef = aggregate(cbind(BTTH, BlTH,BuTH)~GRID_NUM+WOS+LFA,data=ef,FUN=mean)
ef$WOS  = ifelse(ef$LFA %in% 33:34,ef$WOS+6,ef$WOS)
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

saveRDS(fl,file='results/LandingsPredictedActual.rds')

