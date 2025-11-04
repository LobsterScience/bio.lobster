#cod condition Cod

lobster.db('atSea')
a = subset(atSea, LFA %in% c(33,34,35,36,38,40,41) & SPECIESCODE==10 & lubridate::year(STARTDATE)>2016 & !is.na(CONDITION) & CONDITION> -1)
a$COND = ifelse(a$CONDITION==0,'Healthy','Injured/Dead')
a$Healthy = ifelse(a$COND=='Healthy',1,0)
a$Injured_Dead = ifelse(a$COND=='Injured/Dead',1,0)
table(a$COND)

a$RDepth = round(a$DEPTH/20)*20+10

ar = aggregate(cbind(Healthy,Injured_Dead)~RDepth,data=a,FUN=sum)

ar$prop = ar$Injured_Dead/(ar$Injured_Dead+ar$Healthy)
ar$SS = ar$Injured_Dead+ar$Healthy




#fishing depth 
gr = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','GridPolys_DepthPruned_37Split.rds'))
gr41 = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41_grid_polys.rds')))
coa = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','CoastSF.rds')))
coa= st_make_valid(coa)
coa = subset(coa,PROVINCE=='Nova Scotia')
coa = subset(coa,st_area(coa)==max(st_area(coa))) # remove islands and cape breton

gr$GRID_NO = as.numeric(gr$GRID_NO)
#remove the islands or multipart polygons and keep only the biggest ones
a4 = lobster.db('process.logs41')
a4 = subset(a4,!is.na(GRID_NO) & yr>2005 & yr<2025)
a4$SYEAR = a4$yr
a4$DATE_FISHED = a4$FV_FISHED_DATETIME
a4$DOY = lubridate::yday(a4$FV_FISHED_DATETIME)
a4$WEIGHT_KG = a4$ADJCATCH_KG
u4 = unique(a4$GRID_NO)

gr <- gr %>%
  mutate(area = st_area(geometry)) %>%
  group_by(GRID_NO, LFA) %>%
  slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-area)
gr41$LFA = as.character(gr41$LFA)
gr41 = subset(gr41,GRID_NO %in% u4 & GRID_NO %ni% c(860,1199))

gall = gtot = bind_rows(gr,gr41)
l = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','bathy_by_lfa.rds'))
st_geometry(l) = st_geometry(l) *1000
st_crs(l) = 32620
l = st_transform(l,crs=4326)

gl = st_join(l,gall,join=st_within)
gl = subset(gl,!is.na(GRID_NO))
gl = as_tibble(gl)          
gl$LFA.y = gl$geometry = NULL
gl = aggregate(z~LFA.x+GRID_NO,data=gl,FUN=mean)
glm = merge(gall,gl,by.x=c('LFA','GRID_NO'),by.y = c('LFA.x','GRID_NO'))
glm = subset(glm,LFA %in% c(33,34,35,36,38,41))

Tot = readRDS('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping\\DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
to = subset(Tot,FishingYear==2023)
gt = merge(glm,to,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'))
gt$RDepth = round(gt$z/20)*20+10

b =aggregate(TrapHauls~RDepth,data=gt,FUN=sum)
b$prop = b$TrapHauls/ sum(b$TrapHauls)



  ggplot()+
    geom_point(data=b,aes(x=RDepth,y=prop),shape=16)+
    labs(x='Depth (m)',y='Proportion Trap Hauls')
  
  
  
  
  ggplot(ar,aes(x=RDepth,y=prop,size=SS))+geom_point()+
    #scale_color_viridis_c(trans='log')+
    #scale_fill_continuous(trans='log')+
    scale_size_continuous(range=c(1,10))+
    labs(x='Depth (m)',y='Proportion Injured / Dead')
  
  
  ba = merge(b,ar,by='RDepth')
  ba$effSS = (ba$SS)*ba$prop.x #effective sample size
  
  sum(ba$prop.y*ba$effSS) / sum(ba$effSS)
  