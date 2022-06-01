#' @export

ggLobsterMap <- function(area='custom',ylim=c(40,52),xlim=c(-74,-47),
                         attrData=NULL, addGrids=T,bathy=T,fw=NULL,legLab="",
                         addLFALabels=F,scaleTrans='identity',brks,return.object=F, ...){
  
  if(area=='all')		{ ylim=c(41.1,48); 		xlim=c(-67.8,-57.8)	}
  if(area=='27-38')	{ ylim=c(42.5,48); 		xlim=c(-67.4,-57.8)	}
  if(area=='27-33')	{ ylim=c(42.5,48); 		xlim=c(-66.6,-57.8)	}
  if(area=='27-32')	{ ylim=c(43.5,48); 		xlim=c(-63.6,-57.8)	}
  if(area=='ENS')	  { ylim=c(44.0,45.7); 	xlim=c(-62.2,-59.8)	}
  if(area=='34-38')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-63.5)	}
  if(area=='35-36')	{ ylim=c(44.5,46);	 	xlim=c(-67.2,-63.2)	}
  if(area=='west')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-64)	}
  if(area=='27')		{ ylim=c(44.9,47.9); 	xlim=c(-61,-57.8)	}
  if(area=='27.Crop')		{ ylim=c(45.4,47.6); 	xlim=c(-61.1,-58.8)	}
  if(area=='28')		{ ylim=c(45.3,46);	 	xlim=c(-61.6,-60.3)	}
  if(area=='29')		{ ylim=c(45.3,46); 		xlim=c(-61.6,-60.3)	}
  if(area=='30')		{ ylim=c(44.6,45.9); 	xlim=c(-60.8,-59.6)	}
  if(area=='31A')		{ ylim=c(44.4,45.7); 	xlim=c(-61.8,-60)	}
  if(area=='31B')		{ ylim=c(44.1,45.3); 	xlim=c(-62.2,-60.5)	}
  if(area=='32')		{ ylim=c(43.8,45);	 	xlim=c(-63.5,-61.5)	}
  if(area=='33')		{ ylim=c(42.5,44.8); 	xlim=c(-65.8,-62.2)	}
  if(area=='34')		{ ylim=c(42.5,45);	 	xlim=c(-67.8,-65)	}
  if(area=='35')		{ ylim=c(44.5,46);	 	xlim=c(-66,-63.2)	}
  if(area=='36')		{ ylim=c(44.5,45.7); 	xlim=c(-67.2,-65)	}
  if(area=='37')		{ ylim=c(44.5,45.2);	xlim=c(-67.2,-66) }
  if(area=='38')		{ ylim=c(43.8,45);		xlim=c(-67.5,-66.2) }
  if(area=='40')		{ ylim=c(42.25,43);		xlim=c(-66.5,-65.25)}
  if(area=='41')		{ ylim=c(41.1,44); 		xlim=c(-68,-63.5)	}
  if(area=='SWN')		{ ylim=c(42.5,45); 		xlim=c(-67.8,-62.2)	}
  if(area=='BoF')		{ ylim=c(43.75,46); 	xlim=c(-67.8,-63.2)	}
  if(area=='33-35')	{ ylim=c(42.5,46); 		xlim=c(-67.8,-63.2)	}
  if(area=='33-34')	{ ylim=c(42.5,45); 		xlim=c(-67.5,-62.2)	}	
  
  
  library("ggplot2")
  theme_set(theme_bw())
  library("sf")
  
  
    sf_use_s2(FALSE) #needed for cropping
  ns_coast =readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","CoastSF.rds"))
  r<-readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolysSF.rds"))
  rL = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
  
  st_crs(r) <- 4326
  st_crs(rL) <- 4326
  st_crs(ns_coast) <- 4326
  
  ns_coast = suppressWarnings(suppressMessages(st_crop(ns_coast,xmin=xlim[1],ymin=ylim[1],xmax=xlim[2],ymax=ylim[2])))
    r = suppressWarnings(suppressMessages(st_crop(r,xmin=xlim[1],ymin=ylim[1],xmax=xlim[2],ymax=ylim[2])))
  rL = suppressWarnings(suppressMessages(st_crop(rL,xmin=xlim[1],ymin=ylim[1],xmax=xlim[2],ymax=ylim[2])))
  
  b = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","bathy10-300SF.rds"))
  l = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolysSF.rds"))
  st_crs(b) <- 4326
  st_crs(l) <- 4326
  b = suppressWarnings(suppressMessages(st_crop(b,xmin=xlim[1],ymin=ylim[1],xmax=xlim[2],ymax=ylim[2])))
  l = suppressWarnings(suppressMessages(st_crop(l,xmin=xlim[1],ymin=ylim[1],xmax=xlim[2],ymax=ylim[2])))
  cents = readRDS(file.path( project.datadirectory("bio.lobster"), "data","maps","LFALabelsSF.rds"))
  
      p =  ggplot(data=l) + 
          geom_sf(size=1.25) + 
          geom_sf(data=ns_coast,fill='grey') +
          xlab("Longitude") +
          ylab("Latitude")
      
        if(bathy){
            p = p + geom_sf(data=subset(b,Z %in% c(60,100)),colour=alpha("#2C77BF", .3))
        }
      if(addGrids){
          p = p + geom_sf(data=r,fill=NA) + geom_sf(data=ns_coast,fill='grey') 
        }
  
  if(!is.null(attrData)) {
        g = attrData
        if(any(grepl('GRID',toupper(names(g))))) { 
              rL <- r
              r1 = st_as_sf(merge(g,rL,by.x=c('LFA','GRID_NO'),by.y=c('LFA','GRID_NO')))
            } else {
              r1 = st_as_sf(merge(g,rL,by.x=c('LFA'),by.y=c('LFA')))
          }
        st_crs(r1) <- 4326 
  if(!is.null(fw)){
        p =  p + 
          geom_sf(data=r1 , aes(fill=Z)) +
          #scale_fill_viridis_c(trans=scaleTrans,limits=brks,palette='spectral') +
          scale_fill_distiller(trans=scaleTrans,limits=brks,palette='Spectral') +
          labs(fill=legLab) +
          facet_wrap(fw) +
          geom_sf(data=ns_coast,fill='grey')
    } else {
        p =  p + 
          geom_sf(data=r1 , aes(fill=Z)) +
          labs(fill=legLab) +
          #scale_fill_viridis_c(trans=scaleTrans,limits=brks) +
          scale_fill_distiller(trans=scaleTrans,limits=brks,palette='Spectral') +
          geom_sf(data=ns_coast,fill='grey')
            }
  }
      if(addLFALabels){
        p = p + geom_sf_text(data=cents, aes(label=label),family='sans')+coord_sf(xlim=xlim,ylim=ylim)
      }
      
      
      if(return.object) return(p)
  p + theme_bw()+
    theme( panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
          axis.text.x=element_text(angle=90,hjust=1)) +
    scale_x_continuous(breaks=round(seq(xlim[1],xlim[2],length.out = 4),2)) +
    scale_y_continuous(breaks=round(seq(ylim[1],ylim[2],length.out = 4))) 
  
          }
      

  