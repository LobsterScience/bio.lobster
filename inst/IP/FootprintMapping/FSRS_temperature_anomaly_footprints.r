require(bio.lobster)
require(bio.utilities)
require(devtools)
require(ggplot2)
require(lubridate)
la()

lobster.db('fsrs')

fsrs$yd = yday(fsrs$HAUL_DATE)
fsrs$woy = week(fsrs$HAUL_DATE)

fsrs$ts = fsrs$HAUL_YEAR+fsrs$woy/52
#unique per trap
ft = aggregate(TEMP~HAUL_DATE+woy+ts+LFA_GRID+LFA+SYEAR+VESSEL_CD,data=subset(fsrs,!is.na(TEMP) & TEMP> -1 & DEPTH<40),FUN=mean)

#mean per grid and week
ft = aggregate(TEMP~woy+LFA_GRID+LFA+ts+SYEAR,data=ft,FUN=mean)

cli = aggregate(TEMP~LFA+LFA_GRID+woy,data=subset(ft,SYEAR<2010),FUN=mean)
names(cli)[4]='Climatology'

cl= merge(ft,cli)

cl$ano = cl$TEMP - cl$Climatology

##footprints
GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))
gTot = merge(GrMap,cl,by.x=c('LFA','GRID_NO'),by.y=c('LFA','LFA_GRID'),all.x=T)


makeFSRSTempFootPrints <- function(x,data,grmap=GrMap){
  LFA1=x 

  g27 = subset(data,LFA==27 & SYEAR==2023 )
  g27 = g27[order(g27$ts),]
  g27$wk_yr = paste(g27$SYEAR,g27$woy,sep="-")
  g27$wk_yr = ifelse(g27$LFA>32 & g27$woy>40, paste(g27$SYEAR-1,g27$woy,sep="-"),g27$wk_yr)
  g27$yr = ifelse(g27$LFA>32 & g27$woy>40, g27$SYEAR-1,g27$SYEAR)
  g27 = g27[order(g27$yr,g27$woy),]
  uy = unique(g27$wk_yr)
  
  g27$fwy_yr = factor(g27$wk_yr,levels=uy)
  ok1 = ggplot(g27,aes(fill=ano))+
    geom_sf() +
    scale_fill_distiller(trans='identity',palette='Spectral') +
    facet_wrap(~fwy_yr)+
    geom_sf(data=coa,fill='grey')+
    geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
    coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
             ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
             expand = FALSE)+
    scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
    scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
  
  
  width_inch <- 8.5 * 0.7  # 80% of the paper width
  height_inch <- 11 * 0.7  # 80% of the paper height
  
  # Save the plot with the specified dimensions
  nm = paste('LFA', LFA1,'- Logbook reported number of trap hauls by grid.png',sep="")
  ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
  }


wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping\\Figures\\')
setwd(wd)


makeNewFootPrints(x='27',data=gTot)
makeNewFootPrints(x='29',data=gTot)
makeNewFootPrints(x='30',data=gTot)
makeNewFootPrints(x='32',data=gTot)
makeNewFootPrints(x='33',data=gTot)
makeNewFootPrints(x='34',data=gTot)
makeNewFootPrints(x='35',data=gTot)
makeNewFootPrints(x='36',data=gTot)
makeNewFootPrints(x='38',data=gTot)
makeNewFootPrints(x='311',data=gTot)
makeNewFootPrints(x='312',data=gTot)

r<-readRDS(file.path( layerDir,"GridPolysSF.rds"))

