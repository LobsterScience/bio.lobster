#comparing nwarc to glorys and observations
require(bio.lobster)
require(bio.utilities)
require(sf)
require(ggplot2)

setwd(file.path(bio.datadirectory,'bio.lobster','analysis','ClimateModelling'))

 fd = file.path(project.datadirectory('bio.lobster'),'Temperature Data','NWARC')
  ol = readRDS(file=file.path(fd,'CompiledClimatologies.rds'))

  Glsur = readRDS('GlorysPredictSurfaceMonth.rds')


ol$mn = as.numeric(ol$mon)

interpolate_gstat = function(model,x,...){
	v = st_as_sf(x,coords=c())
}
#observed temps
	g = lobster.db('temperature.data')
	g$yr = year(g$T_DATE)
	g$mon = month(g$T_DATE)
	g = aggregate(TEMP~LON_DD+LAT_DD+mon+yr,data=g,FUN=mean)
	g = g %>% st_as_sf(coords=c('LON_DD','LAT_DD'),crs=4326) %>% st_transform(32620)
	st_geometry(g) = st_geometry(g)/1000
	st_crs(g) = 32620

	gs = subset(g, yr %in% 1995:2017)
	gl = subset(Glsur, yr %in% 1995:2017 )

	y = 1995:2017
	m = 1:12
	junk = list()
	v=0
	for(i in y){
		for(n in m){
			
			x  = subset(gs,yr==i & m==n)
			if(dim(x)[1]<1) next
			v=v+1
			xg = subset(gl,yr==i & m==n)
			if(i %in% 1995:2004) xn = subset(ol,Decade=='95_04' & mn==n)
			if(i %in% 2005:2017) xn = subset(ol,Decade=='05_17'& mn==n)
	  	 #using idw
       	 x=st_as_sf(x)
		
		gg = gstat(id='bottomT',formula = bottomT~1,data=xg,nmax=5,set=list(idp=.5))
			ggp = predict(gg,newdata=x)[,1]
			st_geometry(ggp) <- NULL
			x$gloInt=ggp[,1]
       	 
       	 	gg = gstat(id='BT',formula = BT~1,data=xn,nmax=5,set=list(idp=.5))
			ggp = predict(gg,newdata=x)
			st_geometry(ggp) <- NULL
			x$nwarcInt=ggp[,1]
     
		#just using near points	
		 ou = st_nearest_feature(x,xg)
       	 ds = st_distance(x,xg[ou,],by_element=T)
       	 st_geometry(xg) = NULL
       	 x$glo = xg$bottomT[ou]
       	 x$gloz = xg$z[ou]
       	 x$glod = as.numeric(ds)
       	 
       	 ou = st_nearest_feature(x,xn)
       	 ds = st_distance(x,xn[ou,],by_element=T)
       	 st_geometry(xn) = NULL
       	 x$nwarc = xn$BT[ou]
       	 x$nwarcz = xn$z[ou]
       	 x$nwarcd = as.numeric(ds)     		  	 
junk[[v]] = x
		}

	}

da = dplyr::bind_rows(junk)

da$gDiff = da$glo-da$TEMP
da$nDiff = da$nwarc-da$TEMP
da$gIDiff = da$gloInt-da$TEMP
da$nIDiff = da$nwarcInt-da$TEMP

aggregate(cbind(gDiff,nDiff,gIDiff,nIDiff)~mon,data=da,FUN=function(x) mean(x))

ggplot(data = da,aes(gIDiff)) + geom_density(aes(y=..density..))+ geom_density(data=aes(y=..density..))

ggplot(da,aes(gIDiff)) + geom_histogram(aes(y=..density..)) + facet_wrap(~mon)	

ggplot(da,aes(nDiff)) + geom_histogram(aes(y=..density..)) + facet_wrap(~mon)	


ggplot(da) +
  geom_sf(aes(fill=nIDiff,color=nIDiff)) + 
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
