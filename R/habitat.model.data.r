#' @export 

habitat.model.data <- function(DS, p) {
	    options(stringsAsFactors=F)

    fn.root =  file.path( project.datadirectory('bio.lobster'), "data") 
    fnProducts = file.path(fn.root,'products')
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnProducts, recursive = TRUE, showWarnings = FALSE )

    if(DS %in% c('logs41.habitat','logs41.habitat.redo')) {
		    if(DS == 'logs41.habitat') {
    				 a = lobster.db('logs41.habitat')
					a$CPUE = a$ADJCATCH / a$NUM_OF_TRAPS
					vars.2.keep = c('plon','plat','timestamp',"CPUE",'z','dZ','ddZ','t','substrate.mean')    				 
					a = a[,vars.2.keep]
					a = rename.df(a,c('CPUE'),c('B'))
    				 return(a)
	   				}
	   			
	   			a = lobster.db('logbook41.habitat.redo')
	   			return(a)
	   				}

	if(DS %in% c('nefsc.surveys', 'nefsc.surveys.redo')) {

			if(DS == 'nefsc.surveys') {

				    load(file=file.path(fnProducts,'nefscHabitatData.rdata'))
				    vars.2.keep = c('plon','plat','timestamp','z','ddZ','t','substrate.mean','TOTWGT')
				    aa = aa[,vars.2.keep]
					aa = rename.df(aa,c('TOTWGT'),c('B'))
    				return(aa)
					}

				      p = bio.lobster::load.environment()
				              require(raster)
				              require(bio.lobster)
				              require(rgdal)
				              loadfunctions('bio.habitat')
				              loadfunctions('bio.utilities')
				              loadfunctions('bio.indicators')
				              loadfunctions('bio.temperature')
				              la()

				      p$reweight.strata = F #this subsets 
				      p$years.to.estimate = c(1969:2015)
				      p$length.based = T
				      p$size.class = c(50,300)
				      p$by.sex = F
				      p$bootstrapped.ci=F
				      p$strata.files.return=F
				      p$strata.efficiencies=F
				      p$clusters = c( rep( "localhost", 7) )
				      p$strata.files.return=T
				      p$season =c('spring')# p$series =c('spring');p$series =c('fall')
				      p$define.by.polygons = F
				      p$lobster.subunits=F
				      p$area = 'all'
				      p = make.list(list(yrs=p$years.to.estimate),Y=p)
				                aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
				      			aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data
				      p$season ='fall'
				                aout= nefsc.analysis(DS='stratified.estimates.redo',p=p,save=F)
				      			bb = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data

				      aa = rbind(aa,bb)

					  aa = lonlat2planar(aa,input_names = c('X','Y'),proj.type = p$internal.projection)
				            aa$plon = grid.internal(aa$plon,p$plons)
				            aa$plat = grid.internal(aa$plat,p$plats)
				            aa$zO = aa$z
				            aa$z = NA
				            aa$depth = NULL
				             
				            aa = completeFun(aa,c('plon','plat'))
				            aa = habitat.lookup(aa,p=p,DS='depth')

				            #clean up some errors
				            print('check to see if depth is avaliable ')

				            hist(aa$z,'fd',xlab='Depth',main="")

				            #time stamping for seasonal temperatures

				            aa$timestamp = as.POSIXct(aa$BEGIN_GMT_TOWDATE,tz='America/Halifax',origin=lubridate::origin)
				            aa$timestamp = with_tz(aa$timestamp,"UTC")
				            aa$dyear = lubridate::decimal_date(aa$timestamp)- lubridate::year(aa$timestamp)
				       
				            aa = habitat.lookup(aa,p=p,DS='temperature.seasonal')
				            aa$t = aa$BOTTEMP
				            aa = habitat.lookup(aa,p=p,DS='substrate')
				            save(aa,file=file.path(fnProducts,'nefscHabitatData.rdata'))
							}
		if(DS %in% c('DFO.summer.habitat','DFO.summer.habitat.redo')) {

				if(DS =='DFO.summer.habitat') {
				            load(file=file.path(fnProducts,'dfosummerHabitatData.rdata'))
				            vars.2.keep = c('plon','plat','timestamp','t','z','dZ','ddZ','substrate.mean','totwgt')
				            aa = aa[,vars.2.keep]
				            aa = rename.df(aa,c('totwgt'),c('B'))
					return()
				}


				      p = bio.lobster::load.environment()
				              require(raster)
				              require(bio.lobster)
				              require(rgdal)
				              loadfunctions('bio.habitat')
				              loadfunctions('bio.utilities')
				              loadfunctions('bio.indicators')
				              loadfunctions('bio.temperature')
				              la()
					  	  p$series =c('summer')# p$series =c('georges');p$series =c('fall')
					      p$define.by.polygons = F
					      p$lobster.subunits=F
					      p$area = 'all'
					      p$years.to.estimate = c(1970:2015)
					      p$length.based = F
					      p$by.sex = F
					      p$bootstrapped.ci=F
					      p$strata.files.return=F
					      p$vessel.correction.fixed=1.2
					      p$strat = NULL
					      p$clusters = c( rep( "localhost", 7) )
					      p$strata.efficiencies = F
					      p$strata.files.return=T
					      p = make.list(list(yrs=p$years.to.estimate),Y=p)

					      

					# DFO survey All stations including adjacent
					      p$define.by.polygons = F
					      p$lobster.subunits=F
					      p$reweight.strata = F #this subsets 
					      
					      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
					      


	aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data
	  aa = lonlat2planar(aa,input_names = c('X','Y'),proj.type = p$internal.projection)
				            aa$plon = grid.internal(aa$plon,p$plons)
				            aa$plat = grid.internal(aa$plat,p$plats)
				            aa$zO = aa$z
				            aa$z = NA
				            aa$depth = NULL
				             
				            aa = completeFun(aa,c('plon','plat'))
				            aa = habitat.lookup(aa,p=p,DS='depth')
				            aa$z = aa$zO #use observed depths
				            #clean up some errors
				            

				            hist(aa$z,'fd',xlab='Depth',main="")

				            #time stamping for seasonal temperatures

				            aa$timestamp = as.POSIXct(aa$sdate,tz='America/Halifax',origin=lubridate::origin)
				            aa$timestamp = with_tz(aa$timestamp,"UTC")
				            aa$dyear = lubridate::decimal_date(aa$timestamp)- lubridate::year(aa$timestamp)
				       
				            aa = habitat.lookup(aa,p=p,DS='temperature.seasonal')
				            aa$t = aa$bottom_temperature
				            aa = habitat.lookup(aa,p=p,DS='substrate')
				            save(aa,file=file.path(fnProducts,'dfosummerHabitatData.rdata'))
			
		}

	if(DS %in% c('DFOGeorgesHabitat','DFOGeorgesHabitat.redo')) {
		
	}

}