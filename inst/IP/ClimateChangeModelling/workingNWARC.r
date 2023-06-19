-#nwarc decadal climatologies
#1995-2004

require(bio.lobster)
require(bio.utilities)
require(sf)
require(ggplot2)


fd = file.path(project.datadirectory('bio.lobster'),'Temperature Data','NWARC')
fi = dir(fd,full.names=T)
fi = fi[grep('an10',fi)]
out = list()
for(i in 1:length(fi)){
		x = read.csv(fi[i],skip=1)
		names(x)[1:3]=c('Y','X','X0')
		x$Decade=NA
		if(grepl('95A4',fi[i])) x$Decade = '95_04'
		if(grepl('A5B7',fi[i])) x$Decade = '05_17'
		f = function(x,j=1,nm=T) {
			if(!nm)return( x[tail(which(!is.na(x)),j)][1])
		   	if(nm)return( names(x[tail(which(!is.na(x)),j)][1]))
		   }
		x$BT = apply(x,1,f,2,nm=F)
		x$z =  apply(x,1,f,3,nm=T)
		x$z = as.numeric(substr(x$z,2,5))
		x$mon = sub(".*(..)an.*","\\1",fi[i])
		out[[i]] = subset(x,select=c(X,Y,Decade,mon,BT,z))

		}

ol = dplyr::bind_rows(out)

ol = st_as_sf(ol,coords=c('X','Y'),crs=4326)
sf_use_s2(FALSE)

 ol = st_crop(ol,
          c(xmin = -68, ymin = 41, xmax = -56.5, ymax = 47.5))

ol = st_transform(ol,32620) 
st_geometry(ol) <- st_geometry(st_as_sf(ol$geometry/1000)) 
st_crs(ol) <- 32620
ol$BT = as.numeric(ol$BT	)

ggplot(subset(ol,Decade=='95_04')) +
  geom_sf(aes(fill=BT,color=BT)) + 
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap(~mon) +
 # geom_sf(data=rL,size=1,colour='black',fill=NA)+
  theme( axis.ticks.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
  ) +
  coord_sf()

  saveRDS(ol,file=file.path(fd,'CompiledClimatologies.rds'))
