
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
require(sf)
la()

dat = ILTS_ITQ_All_Data(redo_base_data = F, species = 2550, size = c(82,200),aggregate = T,biomass = T )
dir.create(fd,showWarnings=F)
setwd(fd)
#survey = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','BaseDataForClimateModel.rds'))
survey= st_as_sf(subset(dat,YEAR>2007),coords = c('SET_LONG','SET_LAT'),crs=4326)
survey = st_transform(survey,32620)
survey$x1000 = st_coordinates(survey)[,1]/1000
survey$y1000 = st_coordinates(survey)[,2]/1000
st_geometry(survey) <- NULL
survey=st_as_sf(survey,coords=c('x1000','y1000'),crs=32620)

sf_use_s2(FALSE) #needed for cropping
#survey <- suppressWarnings(suppressMessages(
##  st_crop(survey,
#          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))

survey = subset(survey,month(SET_DATE) %in% 6:8)

#survey=subset(survey,SOURCE %in% c('DFO_RV','ILTS_ITQ') &month(survey$DATE) %in% c(6,7,8) )

# Project our survey data coordinates:

survey$lZ = log(survey$SET_DEPTH)
survey = subset(survey,!is.na(lZ))
#survey = subset(survey,!is.na(Lobster))

#survey$LO = log(survey$OFFSET)
#survey = subset(survey,OFFSET>quantile(survey$OFFSET,0.01) & OFFSET< quantile(survey$OFFSET,0.99))
#survey$BT = survey$GlT



ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)

st_crs(ns_coast) <- 32620 # 'WGS84'; necessary on some installs
crs_utm20 <- 32620

survey <- survey %>%   
  st_as_sf()
    
#survey = subset(survey, !is.na(BT) & survey$z<400)
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 
#survey = subset(survey,!is.na(Legal))

spde <- make_mesh(as_tibble(survey), xy_cols = c("X1000", "Y1000"),
                   n_knots=300,type = "cutoff_search")
plot(spde)

# Add on the barrier mesh component:
bspde <- add_barrier_mesh(
  spde, ns_coast, range_fraction = 0.1,
  proj_scaling = 1000, plot = TRUE
)
 mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
     mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
     ggplot(ns_coast) +
       geom_sf() +
       geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
       geom_sf(data = mesh_df_land, size = 1, colour = "green")+
       coord_sf()
  


#run oct 16 2023
ss = as_tibble(survey)
fit = sdmTMB(SA_CORRECTED_PRORATED_N~
               s(lZ,k=4),
             data=ss,
       #     offset = ss$LO,
             time='YEAR', 
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on',
            spatiotemporal = 'ar1'
             )

stripDLLs()
saveRDS(fit,'sdmTMBbyQtw0Oct2023.rds')
fit<-readRDS('sdmTMBbyQtw0Oct2023.rds')


Glsur = readRDS(file.path(fdc,'GlorysPredictSurface.rds'))
x = Glsur

#plot_smooth_sdmtmb(fit,select=2)
x = bio.utilities::rename.df(x,c('bottomT','yr'),c('BT','YEAR'))

x=subset(x,YEAR>1998)
x = subset(x,z>0)
x$lZ = log(x$z)
x <- suppressWarnings(suppressMessages(
  st_crop(x,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))

x$X1000 = st_coordinates(x)[,1]
x$Y1000 = st_coordinates(x)[,2]
x = subset(x,exp(lZ)<400 & x$Q==3)
x = as_tibble(subset(x,select=c(YEAR,BT,X1000,Y1000,lZ)))
  x$geometry=NULL
x$SOURCE = 'ILTS_ITQ'
  

g = predict(fit,newdata=x,return_tmb_object = T)
ind = get_index(g,bias_correct = T)
cg = get_cog(g,bias_correct = F)
cg1 = subset(cg,coord=='X')
 cg2 = subset(cg,coord=='Y')
 names(cg2)[2:ncol(cg2)]= paste(names(cg2)[2:ncol(cg2)],'y',sep=".")
 cg1 = merge(cg1,cg2) 
 cg1$est1000 = cg1$est*1000
 cg1$est.y1000 = cg1$est.y*1000
 
 cg1 = st_as_sf(cg1,coords=c('est1000','est.y1000'),crs=crs_utm20)
 b = ggLobsterMap('west')
 b+geom_sf(data=cg1)

 g = predict(fit,newdata=x)
 
   g$preds = fit$family$linkinv(g$est)

  g$X = g$X1000*1000
  g$Y = g$Y1000*1000
  gsf = st_as_sf(g,coords = c("X","Y"),crs=32620,remove=F)
  
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- (rL$geometry)
st_crs(rL) <- 32620


ff = st_join(gsf,rL,join=st_within)
gsf = subset(ff,!is.na(LFA) & LFA >33)

saveRDS(list(fit,gsf),'preds_sdmTMBbyQbinoMay32023.rds')
xx=readRDS('preds_sdmTMBbyQbinoMay32023.rds')
fit=xx[[1]]
gsf=xx[[2]]

coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
coa = st_transform(coa,crs=crs_utm20)
c_utm_coords <- st_coordinates(coa)


#Maps

mm = c(0.,max(quantile(gsf$preds,0.999)))
pl = ggplot(subset(gsf, YEAR %in% 2008:2012)) +
  geom_sf(aes(fill=preds,color=preds),size=2.1) + 
  scale_fill_viridis_c(trans='sqrt',limits=mm) +
  scale_color_viridis_c(trans='sqrt',limits=mm) +
  facet_wrap(~YEAR) +
  geom_sf(data=rL,size=2,colour='white',fill=NA)+
  geom_sf(data=ns_coast,fill='wheat')+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf(xlim = c(st_bbox(ns_coast)$xmin,st_bbox(gsf)$xmax),
            ylim = c(st_bbox(gsf)$ymin,st_bbox(gsf)$ymax),
            expand = FALSE)

nm = paste('SurveyCommercialAbundance_RV_ITLS.png')
ggsave(nm, plot = pl, width =8, height = 11, units = "in") 


# no point in projecting
