#biasCorrection Glorys Forecasts.r

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

require(bio.lobster)
require(bio.utilites)
require(sf)
require(lubridate)
require(dplyr)

setwd(file.path(bio.datadirectory,'bio.lobster','analysis','ClimateModelling'))
	g = lobster.db('temperature.data')
	g$yr = year(g$T_DATE)
	g$W = ceiling(yday(g$T_DATE)/366*25)
	g$DOY = yday(g$T_DATE)
	#g = aggregate(TEMP~T_UID+LON_DD+LAT_DD+W+yr,data=g,FUN=median)
	g = g %>% st_as_sf(coords=c('LON_DD','LAT_DD'),crs=4326) %>% st_transform(32620)
	g$yr = year(g$T_DATE)
	g$W = ceiling(yday(g$T_DATE)/366*25)
	st_geometry(g) = st_geometry(g)/1000
	st_crs(g) = 32620

#############
##glorys only by day

###glorys
 
s = file.path(project.datadirectory('bio.lobster'),'Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k = k[-grep('2020_',k)]
#k= k[c(23:27,29,30,31)]
#k = k[grep('fc',k)]
ol = list()
m=0
for(i in 1:length(k)){
			h = readRDS(k[i])
			h$Date = as.Date(h$Date)
			y = unique(year(h$Date))
			h$DOY = yday(h$Date)
		#	h$W  = ceiling(yday(h$Date)/366*25)
			h = st_as_sf(h,coords =c('X',"Y"),crs=4326)
			h = h %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
			st_geometry(h) = st_geometry(h)/1000
			st_crs(h) = 32620

			x1 = subset(g,yr == y)
			uW = unique(x1$DOY)
					for(j in 1:length(uW)){
				m=m+1
			
						nn = subset(x1,DOY==uW[j] )
						hh = st_as_sf(subset(h,DOY==uW[j] & !is.na(bottomT)))
						nn = st_as_sf(nn)

			gg = gstat(id='bottomT',formula = bottomT~1,data=hh,nmax=5,set=list(idp=.5))
				ggp = predict(gg,newdata=nn)
				st_geometry(ggp) <- NULL
			nn$gloInt=ggp[,1]
    	ol[[m]] = nn       	

			}
}

xx = dplyr::bind_rows(ol)


xx$gloDiff = xx$gloInt - xx$TEMP


ggplot(xx,aes(Gldiff)) + geom_histogram(aes(y=..density..)) + facet_wrap(~yr)	


aggregate(cbind(gloDiff)~yr,data=xx,FUN=summary)



rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620


glo = st_join(xx,rL,join=st_within)
glo$mon = month(glo$T_DATE)

boxplot(gloDiff~mon,data=subset(glo,LFA %in% c(35,36,38) & yr %in% 1993:2019))
boxplot(gloDiff~mon,data=subset(glo,LFA %in% c(35,36,38) & yr %in% 2020:2022))
#under predicting

saveRDS(glo,'GlorysToObserved.rds')

#bias correction surface BoF GOM
sf_use_s2(FALSE) #needed for cropping

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
sf_use_s2(FALSE) #needed for cropping

ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620

gloGOM <- subset(glo,LFA %in% c(33,34,35,36,38,37,41,40)) %>%   
  st_as_sf()
 gloGOM$fc = as.factor(ifelse(gloGOM$yr %in% 2020:2022,1,0))
    
    ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

				 ss = st_nearest_feature(gloGOM,ba)
       	 ds = st_distance(gloGOM,ba[ss,],by_element=T)
       	 st_geometry(ba) = NULL
       	 gloGOM$z = ba$z[ss]
       	 gloGOM$z_dist = as.numeric(ds)


surv_utm_coords <- st_coordinates(gloGOM)

gloGOM$X1000 <- surv_utm_coords[,1] 
gloGOM$Y1000 <- surv_utm_coords[,2] 
gloGOM$lz = log(gloGOM$z)
gloGOM = subset(gloGOM,!is.na(lz))
spde <- make_mesh(as_tibble(gloGOM), xy_cols = c("X1000", "Y1000"),
                   n_knots=200,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)


fitBias = sdmTMB(Gldiff~ fc+ s(lz),
             data=as_tibble(gloGOM),
             time='mon', 
             mesh=bspde,
             family=gaussian(link='identity'),
             spatial='on',
             spatiotemporal='ar1')

saveRDS(list(data=gloGOM,mesh=bspde,model=fitBias),file='ClaimteModelBias.rds')
ou = readRDS('ClaimteModelBias.rds')
Glsur = readRDS('GlorysPredictSurfaceMonth.rds')
x = Glsur
x



x = bio.utilities::rename.df(x,c('bottomT','yr'),c('BT','YEAR'))
x = subset(x,z>0)
x$lZ = log(x$z)
x$X1000 = st_coordinates(x)[,1]
x$Y1000 = st_coordinates(x)[,2]
x = subset(x,exp(lZ)<400)

x = as_tibble(subset(x,select=c(Q,YEAR,BT,X1000,Y1000,lZ)))
x$geometry=NULL

g = predict(fitpa,newdata=subset(x,YEAR>1999))
