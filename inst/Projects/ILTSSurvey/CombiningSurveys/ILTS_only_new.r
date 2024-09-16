
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
fd=file.path(project.datadirectory('bio.lobster'),'analysis','CombiningSurveys')
dir.create(fd,showWarnings=F)
setwd(fd)
crs_utm20 <- 32620


y = ILTS_ITQ_All_Data(redo_base_data = F,size=c(82,300),aggregate=T,species=2550,biomass = T)
y = subset(y,select=c(TRIP_ID,SET_NO, SET_LONG,SET_LAT,SET_DATE,SA_CORRECTED_PRORATED_N))    
y$Survey='ILTS'

survey = (y)
survey = subset(survey, month(SET_DATE) %in% c(6:8))
survey$YEAR = year(survey$SET_DATE)
survey = st_as_sf(survey,coords = c('SET_LONG','SET_LAT'),crs=4326)

ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs

saveRDS(survey,'Commercial_Biomass_Summer_ILTS.rds')
survey$Legal_wt = survey$SA_CORRECTED_PRORATED_N
s23 = subset(survey,YEAR==2024)
s23$group = s23$Survey
v = ggLobsterMap('west',addPoints=T,pts=s23)

survey = st_transform(survey,crs_utm20)

surv_utm_coords = st_coordinates(survey)
survey$X1000 <- surv_utm_coords[,1] /1000
survey$Y1000 <- surv_utm_coords[,2] /1000
st_geometry(survey) <- NULL
survey = st_as_sf(survey,coords = c('X1000','Y1000'),crs=crs_utm20)

sf_use_s2(FALSE) #needed for cropping
survey <- suppressWarnings(suppressMessages(
  st_crop(survey,
         c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


# Project our survey data coordinates:


ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))

ns_coast <- st_transform(ns_coast, crs_utm20)
st_geometry(ns_coast) = st_geometry(ns_coast)
st_crs(ns_coast) <- crs_utm20

survey <- survey %>%   
  st_as_sf()
    
surv_utm_coords <- st_coordinates(survey)

survey$X1000 <- surv_utm_coords[,1] 
survey$Y1000 <- surv_utm_coords[,2] 
survey = subset(survey,!is.na(Legal_wt))


#add in bathy 
        #allocating depth to location
        ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
        ba = ba %>% st_as_sf() 
        st_geometry(ba) = st_geometry(ba)/1000
        st_crs(ba) = 32620
        
        ss = st_nearest_feature(survey,ba)
        ds = st_distance(survey,ba[ss,],by_element=T)
        st_geometry(ba) = NULL
        survey$z = ba$z[ss]
        survey$z_dist = as.numeric(ds)

i = quantile(survey$Legal_wt,0.999)
survey$Legal_wt[which(survey$Legal_wt>i)]=i #windsorize extreme catches
ss = as_tibble(survey)
spde <- make_mesh(ss, xy_cols = c("X1000", "Y1000"),
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
  
ss$SOURCE = as.factor(ss$Survey)
ss$year_scaled = (ss$YEAR-mean(ss$YEAR))/sd(ss$YEAR)

##keep reml=F for comparing fixed effects, use reml=T from comparing models with different random effects
##AICs are really for fixed effects, 
fit = sdmTMB(Legal_wt~s(z)+year_scaled,
             data=ss,
             spatial_varying = ~0 +year_scaled, #random slope on the spatial field
             time='YEAR', 
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on',
            spatiotemporal = 'ar1'
             )

stripDLLs()
saveRDS(fit,'sdmTMBJuly2024_spatvarYear_ILTSonly.rds')
fit = readRDS('sdmTMBJuly2024_spatvarYear_ILTSonly.rds')

##AICs are really for fixed effects, 
fit2 = sdmTMB(Legal_wt~s(z),
             data=ss,
             time='YEAR', 
             mesh=bspde,
             family=tweedie(link='log'),
             spatial='on',
             spatiotemporal = 'ar1'
)

stripDLLs()
saveRDS(fit2,'sdmTMBJuly2024_spatvarYear_ILTSonly_smoothTime.rds')
fit = readRDS('sdmTMBJuly2024_spatvarYear_ILTSonly_smoothTime.rds')


#add in bathy 
#allocating depth to location
ba = readRDS('~/git/bio.lobster.data/mapping_data/bathymetrySF.rds')
ba = ba %>% st_as_sf() 
st_geometry(ba) = st_geometry(ba)/1000
st_crs(ba) = 32620

G <- suppressWarnings(suppressMessages(
  st_crop(ba,
          c(xmin = -82, ymin = 4539, xmax = 383, ymax = 5200))))


rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = st_as_sf(rL)
st_crs(rL) <- 4326
rL = st_transform(rL,32620) 
st_geometry(rL) <- st_geometry(st_as_sf(rL$geometry/1000)) 
st_crs(rL) <- 32620

bathy= ba
LFApolys = rL
saveRDS(list(survey,ns_coast,bathy,LFApolys),file='full_package_survey_coast_bathy_LFApolys_ILTSonly.rds')


#survey cvs
surs = st_join(survey,rL,join=st_within)
surs = subset(surs,!is.na(LFA) & LFA %in% c(34, 35, 36, 38, 40, 41))
x1 = aggregate(Legal_wt~Survey+YEAR+LFA,data=surs,FUN=mean)  
x2 = aggregate(Legal_wt~Survey+YEAR+LFA,data=surs,FUN=sd)  
x3 = aggregate(Legal_wt~Survey+YEAR+LFA,data=surs,FUN=mad)  
names(x2)[4]='sd'
names(x3)[4]='mad'
x4=merge(merge(x1,x2),x3)


x4$cv = x4$sd/x4$Legal_wt
x4 = subset(x4, is.finite(cv))

ggplot(x3,aes(x=YEAR,y=mad,group=Survey,colour=Survey))+geom_point()+geom_line()+facet_wrap(~LFA)
###sampling different densities of lobster, hence the need to combine

##bathy and LFA
#ff = st_join(G,rL,join=st_within)
#ff = subset(ff,!is.na(LFA)
#saveRDS(ff,file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy_LFAPolysSF.rds"))
ff = subset(ff,z<max(ss$z))
yy = unique(ss$YEAR)
o = list()
for(i in 1:length(yy)){
  f = ff
  f$YEAR=yy[i]
  o[[i]]=f
}
ff = do.call(rbind,o)

sg = st_coordinates(ff)
ff$X1000 = sg[,1]
ff$Y1000 = sg[,2]
ff$SOURCE='ILTS'
f = as_tibble(ff)
f$year_scaled = (f$YEAR-mean(f$YEAR))/sd(f$YEAR)
g = predict(fit,newdata=f,return_tmb_object = T)
ga = predict(fit,newdata=f,se_fit = T,nsim=200)
gg = fit$family$linkinv(ga)

g$data$se = apply(gg,1,sd)
ind = get_index(g,bias_correct = F)


with(ind,plot(YEAR,est/1000,lwd=2,type='b',ylim=c(0,max(upr)/1000),main='overall'))
with(ind,lines(YEAR,lwr/1000,lty=2))
with(ind,lines(YEAR,upr/1000,lty=2))


f34 = subset(f,LFA==34)
g34 = predict(fit,newdata=f34,return_tmb_object = T)
ind34 = get_index(g34,bias_correct = T)
with(ind34,plot(YEAR,est/1000,lwd=2,type='b',ylim=c(0,max(upr)/1000),main='LFA34'))
with(ind34,lines(YEAR,lwr/1000,lty=2))
with(ind34,lines(YEAR,upr/1000,lty=2))

k = lobster.db('seasonal.landings')
k$yr = as.numeric(substr(k$SYEAR,6,9))
lines(k$yr,k$LFA34,type='h',col='red')

f3538 = subset(f,LFA %in% 35:38)
g3538 = predict(fit,newdata=f3538,return_tmb_object = T)
ind3538 = get_index(g3538,bias_correct = T)
with(ind3538,plot(YEAR,est/1000,lwd=2,type='b',ylim=c(0,max(upr)/1000),main='LFA35-38'))
with(ind3538,lines(YEAR,lwr/1000,lty=2))
with(ind3538,lines(YEAR,upr/1000,lty=2))


f41 = subset(f,LFA %in% 41)
g41 = predict(fit,newdata=f41,return_tmb_object = T)
ind41 = get_index(g41,bias_correct = T)
with(ind41,plot(YEAR,est/1000,lwd=2,type='b',ylim=c(0,max(upr)/1000),main='LFA41'))
with(ind41,lines(YEAR,lwr/1000,lty=2))
with(ind41,lines(YEAR,upr/1000,lty=2))

###
saveRDS(list(ind, ind34,ind3538, ind41,f,g),'SpatiallyVarying_outputs.rds')

pp = readRDS('SpatiallyVarying_outputs.rds')

ind=pp[[1]]
ind34=pp[[2]]
ind3538=pp[[3]]
ind41=pp[[4]]
f=pp[[5]]
g=pp[[6]]
 



ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -68, ymin = 41, xmax = -64, ymax = 47.5))))

ns_coast = st_transform(ns_coast,32620) 
st_geometry(ns_coast) <- st_geometry(ns_coast)/1000
st_crs(ns_coast) <- 32620



h = st_as_sf(g$data)

#spatially varying coefficient of random field for this case random slope across time
ggplot(subset(h,YEAR==2016 )) +
  geom_sf(aes(fill=zeta_s_year_scaled ,color=zeta_s_year_scaled ),size=2.1) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  geom_sf(data=ns_coast,fill='black')

#spatial random field  
ggplot(subset(h,YEAR==2022)) +
  geom_sf(aes(fill=omega_s ,color=omega_s ),size=2.1) +
  scale_fill_viridis_c() +
  scale_color_viridis_c()+
  geom_sf(data=ns_coast,fill='black')


h$pred = fit$family$linkinv(h$est)
gsf = h

mm = c(0.,max(quantile(gsf$pred,.999)))
gsf$predM = ifelse(gsf$pred>mm[2],mm[2],gsf$pred)
 ggplot(subset(gsf, YEAR %in% c(2023))) +
  geom_sf(aes(fill=predM,color=predM),size=2.1) + 
  scale_fill_viridis_c(trans='sqrt',limits=mm) +
  scale_color_viridis_c(trans='sqrt',limits=mm) +
  facet_wrap(~YEAR) +
geom_sf(data=ns_coast,fill='black')+
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


## standard dev on the response scale
 mm = c(0.0001,max(quantile(gsf$se,.995)))
 gsf$seM = ifelse(gsf$se>mm[2],mm[2],gsf$se)
 ggplot(subset(gsf, YEAR %in% c(seq(1970,2010,10),2012,2015,2018,2021,2023))) +
   geom_sf(aes(fill=seM,color=seM),size=2.1) + 
   scale_fill_viridis_c(trans='sqrt',limits=mm) +
   scale_color_viridis_c(trans='sqrt',limits=mm) +
   facet_wrap(~YEAR) +
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
 

 ## cv on the response scale
 gsf$cv = gsf$se/gsf$pred
 mm = c(0.0001,max(quantile(gsf$cv,.95)))
 
 gsf$cvM = ifelse(gsf$cv>mm[2],mm[2],gsf$cv)
 ggplot(subset(gsf, YEAR %in% c(2023))) +
   geom_sf(aes(fill=cvM,color=cvM),size=2.1) + 
   scale_fill_viridis_c(trans='sqrt',limits=mm) +
   scale_color_viridis_c(trans='sqrt',limits=mm) +
   facet_wrap(~YEAR) +
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
 
##new LFA 41 survey polygon
sp =  st_read(file.path(project.datadirectory('bio.lobster'),'data','polygons','data','Science','Surveyarea_region41exp.shp'))
sp = st_transform(sp,32620) 
st_geometry(sp) <- st_geometry(sp)/1000
st_crs(sp) <- 32620
spp = st_join(gsf,sp,join=st_within)
spp = subset(spp,ID==0)



 vv =aggregate(cv~YEAR+LFA,data=gsf,FUN=mean)
 v4 = aggregate(cv~YEAR,data=spp,FUN=mean)
 v4$LFA = 'NewSurveyBounds_41'
 vv4 = rbind(v4,vv)
 
 ggplot(subset(vv4,LFA %in% c(34,40,41,'NewSurveyBounds_41')& YEAR>1990),aes(x=YEAR,y=cv,group=LFA,colour=LFA))+geom_line()+geom_vline(xintercept = 2013,color='black',linewidth=1.1)
 
 mv = merge(subset(vv4,LFA==34),subset(vv4,LFA=='NewSurveyBounds_41'),by=c('YEAR'))
 mv$pd = percentDifference(mv[,c('cv.x','cv.y')])
 
 ggplot(mv,aes(x=YEAR,y=pd))+geom_line()+geom_point()+ylab('Percent Difference in CV')
 
 vv =aggregate(cv~LFA,data=subset(gsf,YEAR>2000),FUN=mean)
 v4 = mean(subset(spp,YEAR>2000)$cv)
 
 
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
