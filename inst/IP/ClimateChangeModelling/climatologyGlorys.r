
require(bio.lobster)
require(satin)
require(tidyr)
require(PBSmapping)
setwd(file.path('bio.lobster','Temperature Data','GLORYS')) #downloads from CMEMS


y1 = read.cmems('GLORYS1993')
a = y1$bottomT
image(a@lon, a@lat, t(a@data[,,1,]))

fil = dir()
fil = fil[grep('Glo',fil)]

for(i in 1:length(fil)){
		g = bio.lobster::glorysReshape(glorysfile=fil[i])
		saveRDS(g,file = file.path('SummaryFiles',paste(fil[i],'ShelfBoF.rds',sep="_")))
		print(fil[i])
	}


s = file.path('bio.lobster','Temperature Data','GLORYS','SummaryFiles')
k = dir(s,full.names=T)
k = k[grep('ShelfBoF',k)]
k = k[-grep('2022',k)]

for(i in 1:length(k)){
  a=Sys.time()
      h = readRDS(k[i])
      print(k[i])
      if(i==1){
       hdim =dim(h)
        h$Date = as.Date(h$Date)
        h$doy = lubridate::yday(h$Date)
        h1 = aggregate(bottomT~doy+X+Y,data=h,FUN=mean)
        names(h1)[4]=paste("BT",unique(year(h$Date)),sep=".")
        out = h1
        h1 = st_as_sf(h1,coords =c('X',"Y"),crs=4326)
        h1 = h1 %>% st_transform(32620)
          rm(h)
      } else {
        y = paste("BT",unique(year(h$Date)),sep=".")
        h$Date = as.Date(h$Date)
        h$doy = lubridate::yday(h$Date)
        h2 = aggregate(bottomT~doy+X+Y,data=h,FUN=mean)
        names(h2)[4]= y
     
        o = list()
        for(j in 1:365){
        h4 = subset(h2,doy==j)
        h5 = subset(h1,doy==j)
        h3 = st_as_sf(h4,coords =c('X',"Y"),crs=4326)
        h3 = h3 %>% st_transform(32620)
        ou = st_nearest_feature(h5,h3)
        o[[j]] = h4[ou,]
        } 
        h2 = as.data.frame(do.call(rbind,o))
        if((dim(out)[1] != dim(h2)[1])) browser()
        out = merge(out,h2,all.x=T)
        rm(h3)
       }
       print(Sys.time()-a)
}

g = grep('BT',names(out))

  combineClims = function(x,cols,ind){
      x1 = do.call(cbind,x[,cols])
      apply(x1[,seq(ind,ncol(x1),by=7)],1,mean)
    }

#current Climatology
gcur = c(grep('200',names(out)),grep('201',names(out)))
out$Clim0.01  = combineClims(out,g,1)
out$Clim0.025 = combineClims(out,g,2)
out$Clim0.25  = combineClims(out,g,3)
out$Clim0.5   = combineClims(out,g,4)
out$Clim0.75  = combineClims(out,g,5)
out$Clim0.975 = combineClims(out,g,6)
out$Clim0.99  = combineClims(out,g,7)


out1 = out %>% st_as_sf(coords=c('X','Y'),crs=4326) %>% st_transform(32620)
      
