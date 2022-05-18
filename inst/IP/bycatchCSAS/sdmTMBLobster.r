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




aA = read.csv(file=file.path('results','CompliedDataForModelling.csv'))
aA$X.1 = NULL
aA = subset(aA,X< -30 | Y>30)
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
i = which(aA$plon>0)
aA = aA[-i,] 
aA$DOS =  NA
i = which(aA$LFA %in% c(33,34,35))
j = which(aA$SYEAR %in% 2019)

k = intersect(i,j)
aA$DOS[k] = aA$DATE_FISHED[k] - min(aA$DATE_FISHED[k])

j = which(aA$SYEAR %in% 2020)

k = intersect(i,j)
aA$DOS[k] = aA$DATE_FISHED[k] - min(aA$DATE_FISHED[k])

j = which(aA$SYEAR %in% 2021)

k = intersect(i,j)
aA$DOS[k] = aA$DATE_FISHED[k] - (min(aA$DATE_FISHED[k])-30) #sampling did not occur until later this year


aT = as_tibble(aA)
aT$WOS = ceiling(aT$DOS/7)
aT$WOS = ifelse(aT$WOS==0,aT$WOS+1,aT$WOS)


aT = readRDS(file=file.path('results','CompliedDataForModellingjit.rds'))

						jittering =F
						if(jittering){
							redo=F
							if(redo){
								aT$II = paste(aT$X,aT$Y,sep="-")
								j = split(aT,f=list(aT$II))
						for(i in 1:length(j)){
									if(nrow(j[[i]])==1) next
											for(k in 2:nrow(j[[i]])){
													j[[i]][k,'X'] = j[[i]][k,'X']+runif(1,-0.0001,0.0001)
													j[[i]][k,'Y'] = j[[i]][k,'Y']+runif(1,-0.0001,0.0001)
													}	
												}
											aTj = as_tibble(do.call(rbind,j))
											aTl = st_as_sf(aTj,coords = c("X", "Y"),crs= 4326)
									saveRDS(aTj,file=file.path('results','CompliedDataForModellingjit.rds'))
									}
								aT = readRDS(file=file.path('results','CompliedDataForModellingjit.rds'))
						}

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
						       n_knots = 400, type = "kmeans")
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
					baXY$EID = 1:nrow(baXY)
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
			be$WOS = rep(1:42,each=dim(ba)[1])
			beR = as.data.frame(rbind(be,be))
			beR$DID = rep(c('OBS','ASSOC'), each=nrow(be))
			be=beR
			be$lZ = log(be$Depth)
#LFAs for prediction grids

aT$lZ = log(aT$Depth)
 fit = sdmTMB(LegalWt~
 				s(lZ,k=5)+DID,
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				extra_time=c(33,41),
 				family=tweedie(link='log'),
 				spatial='on',
 				spatialtemporal='ar1')

 tidy(fit, conf.int = TRUE)
tidy(fit, effects = "ran_pars", conf.int = TRUE)
plot_smooth(fit, ggplot = TRUE)

go =predict(fit) 
go$pred = fit$family$linkinv(go$est)
saveRDS(go,file='results/lobsterFullPred.rds')

g = predict(fit,newdata=be,nsim=50)
g1 = fit$family$linkinv(g)

be$pred = apply(g1,1,median)
be$sd = apply(g1,1,sd)
be$lQ = apply(g1,1,quantile,0.25)
be$uQ = apply(g1,1,quantile,0.75)

gsf = st_as_sf(be,coords = c("X","Y"),crs=32619,remove=F)

plot_map <- function(dat,column='est'){
		ggplot(dat,aes_string(fill=column)) +
			geom_sf() + 
			facet_wrap(~WOS) +
			coord_fixed()
	}

saveRDS(list(fit,be),file='lobstersdmTMB.rds')
#r = readRDS(file='lobstersdmTMB.rds')
#fit=r[[1]]
#g=r[[2]]

png('Figures/ModelOutput/lobstersdmTMBwk1-12.png')
mm = c(0.001,max(gsf$pred))
ggplot(subset(gsf,WOS %in% 1:12)) +
			geom_sf(aes(fill=pred,color=pred)) + 
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

png('Figures/ModelOutput/lobstersdmTMBwk13-24.png')
ggplot(subset(gsf,WOS %in% 13:24)) +
			geom_sf(aes(fill=pred,color=pred)) + 
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

	png('Figures/ModelOutput/lobstersdmTMBwk25-36.png')
	ggplot(subset(gsf,WOS %in% 25:36)) +
				geom_sf(aes(fill=pred,color=pred)) + 
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


	png('Figures/ModelOutput/lobstersdmTMBwk37-40.png')
	ggplot(subset(gsf,WOS %in% 37:40)) +
				geom_sf(aes(fill=pred,color=pred)) + 
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


ag = aggregate(cbind(pred,lQ,uQ)~SID+PID+WOS,data=be,FUN=median)
#ag = aggregate(cbind(pred)~SID+PID+WOS,data=be,FUN=median)

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
