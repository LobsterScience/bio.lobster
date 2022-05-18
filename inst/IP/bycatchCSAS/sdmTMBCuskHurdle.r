setting up for sdmTMB

require(sdmTMB)
require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(devtools)
require(dplyr)
require(ggplot2)
require(INLA)
options(stringAsFactors=F)
	require(PBSmapping)
require(SpatialHub)
require(sf)
la()

p = bio.lobster::load.environment()
p = spatial_parameters(type='canada.east')
wd = ('~/dellshared/Bycatch in the Lobster Fishery')

setwd(wd)




	ba = lobster.db('bathymetry')
baXY = planar2lonlat(ba,proj.type=p$internal.projection)
	

aT = readRDS(file=file.path('results','CompliedDataForModellingjit.rds'))
####making mesh
						map_data <- rnaturalearth::ne_countries(
								   scale = "medium",
						        	returnclass = "sf", country = "canada")
						  
						     # Crop the polygon for plotting and efficiency:
						      st_bbox(map_data)
						      ns_coast <- suppressWarnings(suppressMessages(
						        st_crop(map_data,
						          c(xmin = -67, ymin = 42, xmax = -53, ymax = 46))))
						 crs_utm20 <- 2961
						     
						     st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
						     ns_coast <- st_transform(ns_coast, crs_utm20)
						     
						     # Project our survey data coordinates:
						     survey <- aT %>%   st_as_sf(crs = 4326, coords = c("X", "Y")) %>%
						       st_transform(crs_utm20)
						     
						     # Plot our coast and survey data:
						     ggplot(ns_coast) +
						       geom_sf() +
						       geom_sf(data = survey, size = 0.5)
						     
						     # Note that a barrier mesh won't don't much here for this
						     # example data set, but we nonetheless use it as an example.
						     
						     # Prepare for making the mesh
						     # First, we will extract the coordinates:
						     surv_utm_coords <- st_coordinates(survey)
						     
						     # Then we will scale coordinates to km so the range parameter
						     # is on a reasonable scale for estimation:
						
						  aT$X1000 <- surv_utm_coords[,1] / 1000
						     aT$Y1000 <- surv_utm_coords[,2] / 1000
						     
						     spde <- make_mesh(aT, xy_cols = c("X1000", "Y1000"),
						       n_knots = 200, type = "kmeans")
						     plot(spde)
						     
						     # Add on the barrier mesh component:
						     bspde <- add_barrier_mesh(
						       spde, ns_coast, range_fraction = 0.1,
						       proj_scaling = 1000, plot = TRUE
						     )
						     
						     # In the above, the grey dots are the centre of triangles that are in the
						     # ocean. The red crosses are centres of triangles that are over land. The
						     # spatial range will be assumed to be 0.1 (`range_fraction`) over land compared
						     # to over water.
						     
						     # We can make a more advanced plot if we want:
						     mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
						     mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
						     ggplot(ns_coast) +
						       geom_sf() +
						       geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
						       geom_sf(data = mesh_df_land, size = 1, colour = "green")
						  
						 # the land are barrier triangles..



##prediction grids

     	gr<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		attr(gr,'projection') <- "LL"
		gr = subset(gr,PID %in% 33:35)
				baXY$EID = 1:nrow(ba)
				baXY$X = baXY$lon
				baXY$Y = baXY$lat
		ff = findPolys(baXY,gr,maxRows=dim(baXY)[1])
		baXY = merge(baXY,ff,by='EID')

				baXY$Depth = baXY$z	
	     baT <- baXY %>%     st_as_sf(crs = 4326, coords = c("lon", "lat")) %>%
							   #st_crop(c(xmin = -68, ymin = 42, xmax = -53, ymax = 47)) %>%						
						       st_transform(crs_utm20) 
		b = st_coordinates(baT)
		baT$X1000 = b[,1]/1000
		baT$Y1000 = b[,2]/1000
		baT$X = b[,1]
		baT$Y = b[,2]

		ba = baT[,c('X','Y','Depth','X1000','Y1000','SID','PID')]
		ba = subset(ba,Depth>5)
		ba$geometry <- NULL
		be = as.data.frame(sapply(ba,rep.int,41))
			beR = as.data.frame(rbind(be,be))
		beR$DID = rep(c('OBS','ASSOC'), each=nrow(be))
		be=beR
		be$lZ = log(be$Depth)
		be$WOS = rep(1:41,each=dim(ba)[1])
#LFAs for prediction grids
aT$PA = ifelse(aT$CuskWt>0,1,0)
aT$lZ = log(aT$Depth)
 fitH = sdmTMB(PA~s(lZ,k=3)+DID,
 				data=aT,
 			#	time='WOS',
 			#	extra_time=33,
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				spatialtemporal='ar1'
 				)


aT2 = subset(aT,CuskWt>0)
bspde2 = make_mesh(aT2,xy_cols=c('X1000','Y1000'),mesh=bspde$mesh)
h = setdiff(1:41,unique(aT2$WOS))

 fitD = sdmTMB(CuskWt~
 				s(lZ,k=5)+DID,
 				data=aT2,
 				#time='WOS', 
 				mesh=bspde2, 
 				#extra_time=h,
 				family=Gamma(link='log'),###need to do the sims
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



gsf = st_as_sf(be,coords = c("X","Y"),crs=32619,remove=F)


plot_map <- function(dat,column='est'){
		ggplot(dat,aes_string("X","Y",fill=column)) +
			geom_raster() + 
		#	facet_wrap(~WOS) +
			coord_fixed()
	}

saveRDS(list(fitH,fitD,be),file='cusksdmTMB.rds')
#r = readRDS(file='lobstersdmTMB.rds')
#fit=r[[1]]
#g=r[[2]]
png('Figures/ModelOutput/CusksdmTMBHurdleBin.png')
mm = c(0.,max(gsf$predBin))
ggplot(subset(gsf,WOS %in% 1)) +
			geom_sf(aes(fill=predBin,color=predBin)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			#facet_wrap(~WOS) +
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()

png('Figures/ModelOutput/CusksdmTMBHurdleCom.png')
mm = c(0.,max(gsf$predC))

ggplot(subset(gsf,WOS %in% 1)) +
			geom_sf(aes(fill=predC,color=predC)) + 
			scale_fill_viridis_c(trans='sqrt',limits=mm) +
			scale_color_viridis_c(trans='sqrt',limits=mm) +
			#facet_wrap(~WOS) +
 			theme( axis.ticks.x = element_blank(),
        		   axis.text.x = element_blank(),
				   axis.title.x = element_blank(),
				   axis.ticks.y = element_blank(),
        		   axis.text.y = element_blank(),
        		   axis.title.y = element_blank()
        		   ) +
 			coord_sf()
dev.off()

	png('Figures/ModelOutput/CusksdmTMBwk25-36.png')
	ggplot(subset(gsf,WOS %in% 25:36)) +
				geom_sf(aes(fill=preds,color=preds)) + 
				scale_fill_viridis_c(trans='sqrt',limits=mm) +
				scale_color_viridis_c(trans='sqrt',limits=mm) +
				facet_wrap(~WOS) +
	 			theme( axis.ticks.x = element_blank(),
	        		   axis.text.x = element_blank(),
					   axis.title.x = element_blank(),
					   axis.ticks.y = element_blank(),
	        		   axis.text.y = element_blank(),
	        		   axis.title.y = element_blank()
	        		   ) +
	 			coord_sf()
	dev.off()


	png('Figures/ModelOutput/CusksdmTMBwk37-40.png')
	ggplot(subset(gsf,WOS %in% 37:40)) +
				geom_sf(aes(fill=preds,color=preds)) + 
				scale_fill_viridis_c(trans='sqrt',limits=mm) +
				scale_color_viridis_c(trans='sqrt',limits=mm) +
				facet_wrap(~WOS) +
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


##Kfold cv

aT$IDS = paste(aT$WOS,aT$GridGroup,sep="-")
aT = cv_SpaceTimeFolds(aT,idCol = 'IDS',nfolds=5)

 fit_cv = sdmTMB_cv(PA~
 				s(Depth,k=5),
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				spatialtemporal='ar1',
 				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
			
 				)


fit1_cv1 = sdmTMB_cv(PA~
 				s(Depth,k=5),
 				data=aT,
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				#fold_ids = 'fold_id',
				k_folds=5,
				#constant_mesh=F
			
 				)

fit2_cv = sdmTMB_cv_nomesh(PA~
 				s(Depth,k=5),
 				data=aT,
 					family=binomial(link='logit'),
 				#fold_ids = aT$fold_id,
				#k_folds=5		
 				)



mae<- function(x,y){
	sum(abs(x-y))/length(x)
}

rmse = function(x,y){
	sqrt((sum(y-x)^2)/length(x))

}

require(pROC)

roc(fit_cv$data$PA,fit_cv$data$cv_predicted)
roc(fit1_cv1$data$PA,fit1_cv1$data$cv_predicted)
roc(fit2_cv$data$PA,fit2_cv$data$cv_predicted)



#pa model model 1 best

aT2$IDS = paste(aT2$WOS,sep="-")
aT2 = cv_SpaceTimeFolds(aT2,idCol = 'IDS',nfolds=5)

bspde2 = make_mesh(aT2,xy_cols=c('X1000','Y1000'),mesh=bspde$mesh)
h = setdiff(1:41,unique(aT2$WOS))
 fitD_cv = sdmTMB_cv(CuskWt~
 				s(lZ,k=5)+DID,#+s(WOS,k=5),
 				data=aT2,
 				time='WOS', 
 				mesh=bspde2, 
 				family=Gamma(link='log'),
 				spatial='on',
 				extra_time =h,
 				spatialtemporal='ar1',
 				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
			 				)


fitD1_cv1 = sdmTMB_cv(CuskWt~
 				s(lZ,k=5)+DID,#+s(WOS,k=5),
 				data=aT2, 
 				mesh=bspde2, 
 				family=Gamma(link='log'),
 				spatial='on',
 #				fold_ids = 'fold_id',
				k_folds=5,
#				constant_mesh=F
			
 				)

fitD2_cv = sdmTMB_cv_nomesh(
				CuskWt~
 				s(lZ,k=5)+DID,#+s(WOS,k=5),
 				data=aT2,
 				family=Gamma(link='log'),
 				fold_ids = aT2$fold_id,
				k_folds=5		
 				)


with(fitD_cv$data,mae(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fitD1_cv1$data,mae(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fitD2_cv$data,mae(as.numeric(CuskWt),as.numeric(fitD$family$linkinv(cv_predicted))))
with(fitD_cv$data,rmse(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fitD1_cv1$data,rmse(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fitD2_cv$data,rmse(as.numeric(CuskWt),as.numeric(fitD$family$linkinv(cv_predicted))))



fit_cvTT = sdmTMBcv_tntpreds(fitD_cv)

fitTT = dplyr::bind_rows(fit_cvTT)
fitTT$sqR = fitTT$CuskWt - fitTT$pred
with(subset(fitTT,tt=='train'),mae(as.numeric(CuskWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),mae(as.numeric(CuskWt),as.numeric(pred)))
with(subset(fitTT,tt=='train'),rmse(as.numeric(CuskWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),rmse(as.numeric(CuskWt),as.numeric(pred)))

require(ggplot2)

ggplot(fitTT,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CuskGammaTrainTestDepthSpaceTime.png')

fit_cvTT2 = sdmTMBcv_tntpreds(fitD2_cv)
fitTT2 = dplyr::bind_rows(fit_cvTT2)
fitTT2$sqR = fitTT2$CuskWt - fitD$family$linkinv(fitTT2$pred)
with(subset(fitTT2,tt=='train'),mae(as.numeric(CuskWt),as.numeric(fitD$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),mae(as.numeric(CuskWt),as.numeric(fitD$family$linkinv(pred))))
with(subset(fitTT2,tt=='train'),rmse(as.numeric(CuskWt),as.numeric(fitD$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),rmse(as.numeric(CuskWt),as.numeric(fitD$family$linkinv(pred))))



require(ggplot2)
ggplot(fitTT2,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CuskGammaTrainTestDepth.png')


fit_cvTT1 = sdmTMBcv_tntpreds(fitD1_cv1)
fitTT1 = dplyr::bind_rows(fit_cvTT1)
fitTT1$sqR = fitTT1$CuskWt - fitTT1$pred
with(subset(fitTT1,tt=='train'),mae(as.numeric(CuskWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),mae(as.numeric(CuskWt),as.numeric((pred))))
with(subset(fitTT1,tt=='train'),rmse(as.numeric(CuskWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),rmse(as.numeric(CuskWt),as.numeric((pred))))


require(ggplot2)
ggplot(fitTT1,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CuskGammaTrainTestDepthSpace.png')

