require(devtools)
require(bio.lobster)
require(bio.utilities)
require(sf)
load_all('C:/Users/Cooka/Documents/git/bio.utilities')


la()
wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\CanUsCollabEffortMapping')
setwd(wd)


layerDir=file.path(project.datadirectory("bio.lobster"), "data","maps")


Tot = readRDS('DiscretizedData/PrivacyScreened_TrapHauls_Landings_Trips_Gridand.rds')
Tot$LFA = ifelse(Tot$LFA=='31B',312,Tot$LFA)
Tot$LFA = ifelse(Tot$LFA=='31A',311,Tot$LFA)


#making plots of Tot

GrMap = readRDS(file.path(project.datadirectory('bio.lobster'),'data','maps','LFA27-38GridsPrunedtoDepth-sf.rds'))
coa = st_as_sf(readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastlineSF_NY_NL.rds")))

GrMap1 = GrMap
GrMap1$area = st_area(GrMap1)/1000000
GrMap1$V2 = paste(GrMap1$LFA, GrMap1$GRID_NO,sep="-")
st_geometry(GrMap1)<- NULL
gg = aggregate(area~LFA+GRID_NO,data=GrMap1,FUN=function(x) abs(sum(x)))

GrMap2 =merge(GrMap,gg)

gTot = merge(GrMap2,Tot,by.x=c('LFA','GRID_NO'),by.y=c('LFA','Grid'),all.x=T)
###################################################################################################################

makeNewFootPrints <- function(x,data,grmap=GrMap){
  LFA1=x 
  gTot=data
  gTot$LandingsT = gTot$Landings/1000
  g27 = subset(gTot,LFA==LFA1 & FishingYear>2018 )
        g27p = subset(gTot,LFA==LFA1 & FishingYear>2018 &PrivacyScreen==1)
        g27n = subset(gTot,LFA==LFA1 & FishingYear>2018 &PrivacyScreen==0)
        
        ok1 = ggplot(g27p,aes(fill=TrapHauls))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
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

        g27p$TrapHaulspkm2 = as.numeric(g27p$TrapHauls/g27p$area)
        
        ok1 = ggplot(g27p,aes(fill=TrapHaulspkm2))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
                  coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        
        
        # Save the plot with the specified dimensions
        nm = paste('LFA', LFA1,'- Logbook reported number of trap hauls (TH) by grid corrected by the area of the grid (km2).png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
        
        
          ok1 = ggplot(g27p,aes(fill=LandingsT))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
            geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
            coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
            scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
            scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
          
        
        
        # Save the plot with the specified dimensions
          nm = paste('LFA',LFA1,'- Logbook reported Landings (t) by grid.png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
          
        g27p$Landingspkm2 = as.numeric(g27p$LandingsT/g27p$area)
        
        ok1 = ggplot(g27p,aes(fill=Landingspkm2))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
                    coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        
        
        
        # Save the plot with the specified dimensions
        nm = paste('LFA',LFA1,'- Logbook reported Landings (t) by grid adjusted by area (km2).png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
        
        
        ok1 = ggplot(g27p,aes(fill=Trips))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
                    coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        
        
        
        # Save the plot with the specified dimensions
        nm = paste('LFA',LFA1,'- Logbook reported number of trips by grid.png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
          
        g27p$Tripspkm2 = as.numeric(g27p$Trips/g27p$area)
        
        ok1 = ggplot(g27p,aes(fill=Tripspkm2))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
          coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        
        
        # Save the plot with the specified dimensions
        nm = paste('LFA',LFA1,'- Logbook reported number of trips by grid adjusted by area (km2).png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
        
        ok1 = ggplot(g27p,aes(fill=NLics))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
          coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        
        
        # Save the plot with the specified dimensions
        nm = paste('LFA', LFA1,'- Logbook reported number of licences fishing by grid.png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
        
        g27p$Licencespkm2 = as.numeric(g27p$NLics/g27p$area)
        
        ok1 = ggplot(g27p,aes(fill=Licencespkm2))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
          coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
                             
        
        # Save the plot with the specified dimensions
        nm = paste('LFA',LFA1,'- Logbook reported number of licences fishing by grid corrected by area (km2).png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
        
        g27p$CPUE = g27p$Landings/g27p$TrapHauls
        ok1 = ggplot(g27p,aes(fill=CPUE))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~FishingYear)+
          geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
            coord_sf(xlim = c(st_bbox(g27)$xmin,st_bbox(g27)$xmax),
                   ylim = c(st_bbox(g27)$ymin,st_bbox(g27)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        
        nm = paste('LFA',LFA1,'- Logbook reported catch (kg) per unit effort (trap haul) by grid.png',sep="")
        ggsave(nm, plot = ok1, width = width_inch, height = height_inch, units = "in") 
        
        
#Trends
        g27 = subset(gTot,LFA==LFA1 & FishingYear>2005 )
        g27p = subset(gTot,LFA==LFA1 & FishingYear>2005 &PrivacyScreen==1)
        g27n = subset(gTot,LFA==LFA1 & FishingYear>2005 &PrivacyScreen==0)
        
        g27$CPUE = g27$Landings/g27$TrapHauls
        
        ui = unique(g27$GRID_NO)
        out = list()
        j=0
        for(i in 1:length(ui)){
            b=subset(g27,GRID_NO==ui[i])
            b1 = subset(g27,GRID_NO==ui[i]& FishingYear==max(b$FishingYear))
            bp=aggregate(CPUE~FishingYear,data=b,FUN=mean)
            
            base = mean(bp$CPUE[which(bp$FishingYear %in% 2006:2010)])
            p4b = p3b = p2b = p1b = b1
            p1b$PercentDiff = percentDifference(c(mean(bp$CPUE[6:8]),base))
            p2b$PercentDiff = percentDifference(c(mean(bp$CPUE[9:11]),base))
            p3b$PercentDiff = percentDifference(c(mean(bp$CPUE[12:14]),base))
            p4b$PercentDiff = percentDifference(c(mean(bp$CPUE[15:17]),base))
            p1b$label = '2011-2013'
            p2b$label = '2014-2016'
            p3b$label = '2017-2019'
            p4b$label = '2020-2022'
            if(nrow(bp)<14){
              p1b$PercentDiff =p2b$PercentDiff = p3b$PercentDiff = p4b$PercentDiff = NA
              }
            bp = do.call(rbind,list(p1b,p2b,p3b,p4b))
            out[[i]] = bp
            
        }
        o = do.call(rbind,out)
      
        ok1 = ggplot(o,aes(fill=PercentDiff))+
          geom_sf() +
          scale_fill_distiller(trans='identity',palette='Spectral') +
          facet_wrap(~label)+
          #geom_sf(data=g27n,fill='white')+  
          geom_sf(data=coa,fill='grey')+
          geom_sf(data=subset(grmap,LFA==LFA1),fill=NA)+
          coord_sf(xlim = c(st_bbox(o)$xmin,st_bbox(o)$xmax),
                   ylim = c(st_bbox(o)$ymin,st_bbox(o)$ymax),
                   expand = FALSE)+
          scale_x_continuous(breaks = c(round(seq(st_bbox(g27)$xmin,st_bbox(g27)$xmax,length.out=2),2)))+
          scale_y_continuous(breaks = c(round(seq(st_bbox(g27)$ymin,st_bbox(g27)$ymax,length.out=2),2)))
        nm = paste('LFA',LFA1,'- Logbook reported grid estimates of percent difference in catch per unit effort (CPUE) relative to mean CPUE between 2006 and 2010.png',sep="")
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

b=subset(r,LFA %in% c(27,28,29,30,311,312,32))
o=subset(GrMap,LFA %in% c(27,28,29,30,311,312,32))

ggplot(b)+
         geom_sf()+
  geom_sf(data=coa,fill='grey')+
  geom_sf(data=o,fill='red')+
  coord_sf(xlim = c(st_bbox(b)$xmin,st_bbox(b)$xmax),
           ylim = c(st_bbox(b)$ymin,st_bbox(b)$ymax),
           expand = FALSE)
       


