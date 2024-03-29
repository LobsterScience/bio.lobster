
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


aA = bycatch.db('GCIFA',wd=wd)

#aA = subset(aA,X< -30 | Y>30)
aA$DATE_FISHED = as.Date(aA$DATE_FISHED)
attr(aA,'projection') = "LL"
aA = lonlat2planar(aA,input_names=c('X','Y'),proj.type = p$internal.projection)

	ba = lobster.db('bathymetry')
	locsmap = match( 
	array_map( "xy->1", aA[,c("plon","plat")], gridparams=p$gridparams ), 
	array_map( "xy->1", ba[,c("plon","plat")], gridparams=p$gridparams ) )

baXY = planar2lonlat(ba,proj.type=p$internal.projection)
	
aA$Depth = ba$z[locsmap]
i = which(aA$Depth<0)
aA = aA[-i,] 

i = which(aA$Y<44.5)
aA = aA[-i,] 

i = which(aA$Y>45.5)
aA = aA[-i,] 
 i=c(2013:2076 ,2077:2097,8533:8545,5109:5123,5155:5191,5244:5250, 10265:10328)
aA = aA[-i,] 

i=c(10078:10107,9049:9062)
aA = aA[-i,] 

k = unique(aA$SYEAR)
aA$DOS = NA
for(i in 1:length(k)){
	l = which(aA$SYEAR==k[i])
	aA$DOS[l] = aA$DATE_FISHED[l] -min(aA$DATE_FISHED[l])
}

aT = as_tibble(aA)
aT$WOS = ceiling(aT$DOS/7)
aT$WOS = ifelse(aT$WOS==0,aT$WOS+1,aT$WOS)

####making mesh
						map_data <- rnaturalearth::ne_countries(
								   scale = "medium",
						        	returnclass = "sf", country = "canada")
						  
						     # Crop the polygon for plotting and efficiency:
						      st_bbox(map_data)
						      ns_coast <- suppressWarnings(suppressMessages(
						        st_crop(map_data,
						          c(xmin = -62.2, ymin = 44, xmax = -60, ymax = 45.5))))
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
						       n_knots = 100, type = "kmeans")
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



#

aT$IDS = 'I'
aT = cv_SpaceTimeFolds(aT,idCol = 'IDS',nfolds=5)
aT$lZ = log(aT$Depth)
 fit_cv = sdmTMB_cv(JonahWt~
 				s(lZ,k=5),
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				fold_ids = 'fold_id',
 				spatialtemporal='ar1',
 				k_folds=5,
 				#constant_mesh=F
 				)
 				

 fit_cv = sdmTMB_cv(JonahWt~
 				s(lZ,k=5),
 				data=aT,
 				time='SYEAR', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				fold_ids = 'fold_id',
 				spatialtemporal='ar1',
 				k_folds=5,
 				#constant_mesh=F
 				)
 				


fit1_cv1 = sdmTMB_cv(JonahWt~
				s(lZ,k=5),
				data=aT,
				mesh=bspde, 
 				spatial='on',
 				family=tweedie(link='log'),
				fold_ids = 'fold_id',
				k_folds=5,
				#constant_mesh=F
				)

fit2_cv = sdmTMB_cv_nomesh(JonahWt~
				s(lZ,k=5),
				data=aT,
				family=tweedie(link='log'),
				fold_ids = aT$fold_id,
				k_folds=5
				)

fit2 = sdmTMB(JonahWt~
				s(lZ,k=5),
				data=aT,
				mesh=bspde, 
 				spatial='off',
 				family=tweedie(link='log'),
				)




mae<- function(x,y){
	sum(abs(x-y))/length(x)
}

rmse = function(x,y){
	sqrt((sum(y-x)^2)/length(x))

}

with(fit_cv$data,mae(as.numeric(JonahWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,mae(as.numeric(JonahWt),as.numeric(cv_predicted)))
with(fit2_cv$data,mae(as.numeric(JonahWt),as.numeric(fit2$family$linkinv(cv_predicted))))
with(fit_cv$data,rmse(as.numeric(JonahWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,rmse(as.numeric(JonahWt),as.numeric(cv_predicted)))
with(fit2_cv$data,rmse(as.numeric(JonahWt),as.numeric(fit2$family$linkinv(cv_predicted))))

#train rmse v test rmse

fit_cvTT = sdmTMBcv_tntpreds(fit_cv)

fitTT = dplyr::bind_rows(fit_cvTT)
fitTT$sqR = fitTT$JonahWt - fitTT$pred
with(subset(fitTT,tt=='train'),mae(as.numeric(JonahWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),mae(as.numeric(JonahWt),as.numeric(pred)))
with(subset(fitTT,tt=='train'),rmse(as.numeric(JonahWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),rmse(as.numeric(JonahWt),as.numeric(pred)))

require(ggplot2)

ggplot(fitTT,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/JonahTrainTestDepthSpaceTime31ab.png')

fit_cvTT2 = sdmTMBcv_tntpreds(fit2_cv)
fitTT2 = dplyr::bind_rows(fit_cvTT2)
fitTT2$sqR = fitTT2$JonahWt - fit2$family$linkinv(fitTT2$pred)
with(subset(fitTT2,tt=='train'),mae(as.numeric(JonahWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),mae(as.numeric(JonahWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='train'),rmse(as.numeric(JonahWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),rmse(as.numeric(JonahWt),as.numeric(fit2$family$linkinv(pred))))



require(ggplot2)
ggplot(fitTT2,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/JonahTrainTestDepth31ab.png')


fit_cvTT1 = sdmTMBcv_tntpreds(fit1_cv1)
fitTT1 = dplyr::bind_rows(fit_cvTT1)
fitTT1$sqR = fitTT1$JonahWt - fitTT1$pred
with(subset(fitTT1,tt=='train'),mae(as.numeric(JonahWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),mae(as.numeric(JonahWt),as.numeric((pred))))
with(subset(fitTT1,tt=='train'),rmse(as.numeric(JonahWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),rmse(as.numeric(JonahWt),as.numeric((pred))))


require(ggplot2)
ggplot(fitTT1,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/JonahTrainTestDepthSpace31ab.png')


######end MM lobsters


##highest elpd has predictions closest to those from true generating process
#
#fit_cv$elpd
#fit1_cv$elpd
#fit2_cv$elpd
#
#	sfit = simulate(fit,nsim=100) 
#	rf = dharma_residuals(sfit,fit)
#
#r <- dharma_residuals(sfit, fit, plot = FALSE)
#
#     plot(r$expected, r$observed)
#     abline(a = 0, b = 1)
#
#
# r1 = fit$family$linkinv(predict(fit))
# r2 = DHARMa::createDHARMa(simulatedResponse=sfit,
# 									observedResponse=fit$data$JonahWt,
# 									fittedPredictedResponse=r1)
#
# plot(r2)
#aT$SRS = r2$scaledResiduals
#	ggplot(data=ns_coast) + geom_sf() +
#			geom_point(data = subset(aT,WOS %in% 1:42),aes(x = X1000*1000, y = Y1000*1000,colour = SRS), shape = 19,size=0.3) +
#			facet_wrap(~WOS) +
#			scale_colour_gradient2(midpoint=.5,low='blue',mid='white',high='red',space='Lab')
#
#
#