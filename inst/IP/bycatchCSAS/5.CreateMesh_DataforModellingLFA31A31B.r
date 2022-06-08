#make sure you run 0.RunMeFirst.r

aA = bycatch.db('GCIFA',wd=wd)
aA$EID = 1:nrow(aA)
k = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maps','LFAgridPolys.csv'))
kk = findPolys(aA,k)

aA = subset(aA,EID %in% kk$EID)

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
aA$DOS =  NA
aA$II = paste(aA$SYEAR,aA$LFA,sep="-")
aW = split(aA,f=aA$II)

for(i in 1:length(aW)){
  aW[[i]]$DOS = aW[[i]]$DATE_FISHED - min(aW[[i]]$DATE_FISHED)
}
aA = as.data.frame(do.call(rbind,aW))

i = which(aA$Y>45.5)
aA = aA[-i,]

i = which(round(aA$X,5)== -61.10717 & round(aA$Y,5)==45.01967)
aA = aA[-i,]

i = which(round(aA$X,5)== -61.04715 & round(aA$Y,5)==45.11178)
aA = aA[-i,]

i = which(round(aA$X,5)== -61.04447 & round(aA$Y,5)==45.11067)
aA = aA[-i,]

aT = as_tibble(aA)
aT$WOS = ceiling(aT$DOS/7)
aT$WOS = ifelse(aT$WOS==0,aT$WOS+1,aT$WOS)


sf_use_s2(FALSE) #needed for cropping
ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
rL = rL[rL$LFA %in% c(311,312),]
st_crs(ns_coast) <- 4326 # 'WGS84'; necessary on some installs

st_crs(rL) <- 4326
crs_utm20 <- 32620
ns_coast <- suppressWarnings(suppressMessages(
  st_crop(ns_coast,
          c(xmin = -62, ymin = 44.9, xmax = -60.5, ymax = 45.45))))

rL <- suppressWarnings(suppressMessages(
  st_crop(rL,
          c(xmin = -62, ymin = 44.9, xmax = -60.5, ymax = 45.45))))

ns_coast <- st_transform(ns_coast, crs_utm20)
rL <- st_transform(rL, crs_utm20)
# Project our survey data coordinates:
survey <- aT %>%   st_as_sf(crs = 4326, coords = c("X", "Y")) %>%
  st_transform(crs_utm20)

# Plot our coast and survey data:
    ggplot(data=rL) +
      geom_sf(size=1) +
      geom_sf(data = survey, size = 0.5,col='red')
    
surv_utm_coords <- st_coordinates(survey)

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


mesh_df_water <- bspde$mesh_sf[bspde$normal_triangles, ]
mesh_df_land <- bspde$mesh_sf[bspde$barrier_triangles, ]
ggplot(rL) +
  geom_sf() +
  geom_sf(data = mesh_df_water, size = 1, colour = "blue") +
  geom_sf(data = mesh_df_land, size = 1, colour = "green")

##prediction grids
gr<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
attr(gr,'projection') <- "LL"
gr = subset(gr,PID %in% c(311,312))
baXY$EID = 1:nrow(baXY)
baXY$X = baXY$lon
baXY$Y = baXY$lat
ff = findPolys(baXY,gr,maxRows=dim(baXY)[1])
baXY = merge(baXY,ff,by='EID')
baXY = subset(baXY,z<70 & z >0)

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
ba = subset(ba,Y>4960000)
ba = subset(ba,X<690000)

be = as.data.frame(sapply(ba,rep.int,9))
be$WOS = rep(1:9,each=dim(ba)[1])
be = as.data.frame(sapply(be,rep.int,2))
be$SYEAR = rep(2018:2019,each=dim(be)[1]/2)

be$lZ = log(be$Depth)
aT$lZ = log(aT$Depth)

saveRDS(list(data=aT,grid=bspde,preds=be),file='results/dataForLFA31A31B.rds')

