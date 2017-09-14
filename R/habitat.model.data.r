#' @export 

habitat.model.data <- function(DS, p) {
	    options(stringsAsFactors=F)

    fn.root =  file.path( project.datadirectory('bio.lobster'), "data") 
    fnProducts = file.path(fn.root,'products')
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnProducts, recursive = TRUE, showWarnings = FALSE )

    if(DS %in% c('logs41','logs41.redo')) {
		    if(DS == 'logs41.habitat') {
    				 a = lobster.db('logs41.habitat')
					a$CPUE = a$ADJCATCH / a$NUM_OF_TRAPS
					vars.2.keep = c('dyear','plon','plat','timestamp',"CPUE",'z','dZ','ddZ','t','substrate.mean')    				 
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
				    vars.2.keep = c('dyear','plon','plat','timestamp','z','dZ','ddZ','t','TOTWGT')
				    ab = ab[,vars.2.keep]
					ab = rename.df(ab,c('TOTWGT'),c('B'))
    				return(ab)
					}
      #  require(raster)
		#		              require(bio.lobster)
		#		              require(rgdal)
		#		              loadfunctions('bio.habitat')
		#		              loadfunctions('bio.utilities')
		#		              loadfunctions('bio.indicators')
		#		              loadfunctions('bio.temperature')
		#		              la()
        # p = bio.lobster::load.environment()
				
				      p$reweight.strata = F #this subsets 
				      p$years.to.estimate = c(1969:2016)
				      p$length.based = T
				      p$size.class = c(50,300)
				      p$by.sex = F
				      p$bootstrapped.ci=F
				      
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
					  aa = lonlat2planar(aa,input_names = c('X','Y'),newnames=c('plon','plat'),proj.type = p$internal.projection)
				            aa$plon = grid.internal(aa$plon,p$plons)
				            aa$plat = grid.internal(aa$plat,p$plats)
				            aa$zO = aa$z
				      
				            aa$z = aa$depth = NULL

				            aa = completeFun(aa,c('plon','plat'))
				            
				            load(file.path(project.datadirectory('bio.bathymetry'),'modelled','bathymetry.baseline.canada.east.rdata'))

				            baseLine = Z[,c('plon','plat')]
	
							locsmap = match( 
        							lbm::array_map( "xy->1", aa[,c("plon","plat")], gridparams=p$gridparams ), 
        							lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams )
        							)
							ab = cbind(aa,Z[locsmap,c('z','dZ','ddZ')])
				            j = which(is.na(ab$zO))
				            ab$zO[j] = ab$z[j]
				            ab$z = ab$zO
				            #time stamping for seasonal temperatures

				            ab$timestamp = as.POSIXct(ab$BEGIN_GMT_TOWDATE,tz='America/Halifax',origin=lubridate::origin)
				            ab$timestamp = with_tz(ab$timestamp,"UTC")
				            ab$dyear = lubridate::decimal_date(ab$timestamp)- lubridate::year(ab$timestamp)
				       		ab$rdy <- round(ab$dyear,1)*10 #temperature column to pull from
				       		ab$ry <- ab$GMT_YEAR -1950 + 1 #index year for temperature
				       	    ff = file.path(project.datadirectory('bio.temperature'),'modelled','t','canada.east','temperature.spatial.annual.seasonal.rdata')
				       		load(ff)
				       		print(paste('loading ',ff))

				            j = which(is.na(ab$BOTTEMP))

				            locsmap = match( 
        							lbm::array_map( "xy->1", ab[j,c("plon","plat")], gridparams=p$gridparams ), 
        							lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams )
        							) #to get the correct locations from the temperature surface
				            
				            k = ab[j,'ry']
				            l = ab[j,'rdy']
				            tp=c()
				            for(i in 1:length(j)){
				            	tp[i] = O[locsmap[i],k[i],l[i]]
				            }

				       		ab$BOTTEMP[j] <- tp
				            ab$t = ab$BOTTEMP

				            save(ab,file=file.path(fnProducts,'nefscHabitatData.rdata'))
				            print('Done Aug 28, 2017')
							}
		if(DS %in% c('dfo.summer','dfo.summer.redo')) {

				if(DS =='dfo.summer') {
				            load(file=file.path(fnProducts,'dfosummerHabitatData.rdata'))
				            vars.2.keep = c('dyear','plon','plat','timestamp','t','z','dZ','ddZ','totwgt')
				            ab = ab[,vars.2.keep]
				            ab = rename.df(ab,c('totwgt'),c('B'))
					return(ab)
				}


					  	  p$series =c('summer')# p$series =c('georges');p$series =c('fall')
					      p$define.by.polygons = F
					      p$lobster.subunits=F
					      p$area = 'all'
					      p$years.to.estimate = c(1970:2016)
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
							
						  load(file.path(project.datadirectory('bio.bathymetry'),'modelled','bathymetry.baseline.canada.east.rdata'))

				            baseLine = Z[,c('plon','plat')]
	
							locsmap = match( 
        							lbm::array_map( "xy->1", aa[,c("plon","plat")], gridparams=p$gridparams ), 
        							lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams )
        							)
							ab = cbind(aa,Z[locsmap,c('z','dZ','ddZ')])
				            j = which(is.na(ab$zO))
				            ab$zO[j] = ab$z[j]
				            ab$z = ab$zO
				            #time stamping for seasonal temperatures
				            ab$timestamp = as.POSIXct(ab$sdate,tz='America/Halifax',origin=lubridate::origin)
				            ab$timestamp = with_tz(ab$timestamp,"UTC")
				            ab$dyear = lubridate::decimal_date(ab$timestamp)- lubridate::year(ab$timestamp)
				       		ab$rdy <- round(ab$dyear,1)*10 #temperature column to pull from
				       		ab$ry <- year(ab$timestamp) -1950 + 1 #index year for temperature
				       	    ff = file.path(project.datadirectory('bio.temperature'),'modelled','t','canada.east','temperature.spatial.annual.seasonal.rdata')
				       		load(ff)
				       		print(paste('loading ',ff))

				            j = which(is.na(ab$bottom_temperature))
				            
				            locsmap = match( 
        							lbm::array_map( "xy->1", ab[j,c("plon","plat")], gridparams=p$gridparams ), 
        							lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams )
        							)
				            k = ab[j,'ry']
				            l = ab[j,'rdy']
				            tp=c()
				            for(i in 1:length(j)){
				            	tp[i] = O[locsmap[i],k[i],l[i]]
				            }

				       		ab$bottom_temperature[j] <- tp
				            ab$t = ab$bottom_temperature

				            save(ab,file=file.path(fnProducts,'dfosummerHabitatData.rdata'))
			
		}

	if(DS %in% c('dfo.georges','dfo.georges.redo')) {
		
		if(DS == 'dfo.georges') {

			load(file=file.path(fnProducts,'dfogeorgesHabitatData.rdata'))
            vars.2.keep = c('dyear','plon','plat','timestamp','t','z','dZ','ddZ','totwgt')
				            ab = ab[,vars.2.keep]
				            ab = rename.df(ab,c('totwgt'),c('B'))
					return(ab)
				
		}
				
     p$series =c('georges')# p$series =c('georges');p$series =c('fall')

      p$define.by.polygons = F
      p$lobster.subunits=F
      p$years.to.estimate = c(1987:2016)
      p$length.based = F
      p$by.sex = F
      p$bootstrapped.ci=T
      p$strata.files.return=F
      p$vessel.correction.fixed=1.2
      p$strat = NULL
      p$clusters = c( rep( "localhost", 7) )
      p$strata.efficiencies = F
      p = make.list(list(yrs=p$years.to.estimate),Y=p)
 
      

# DFO survey All stations including adjacent
      p$define.by.polygons = F
      p$lobster.subunits=F
      p$area = 'all'
      p$reweight.strata = F #this subsets 
      p$strata.files.return=T
      aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)


	aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data
  aa = lonlat2planar(aa,input_names = c('X','Y'),proj.type = p$internal.projection)
				            aa$plon = grid.internal(aa$plon,p$plons)
				            aa$plat = grid.internal(aa$plat,p$plats)
				            #aa$z = aa$z*1.8288
				            aa$zO = aa$z

				            aa$z = NA
				            aa$depth = NULL
				             
				            aa = completeFun(aa,c('plon','plat'))
				            
 load(file.path(project.datadirectory('bio.bathymetry'),'modelled','bathymetry.baseline.canada.east.rdata'))

				            baseLine = Z[,c('plon','plat')]
	
							locsmap = match( 
        							lbm::array_map( "xy->1", aa[,c("plon","plat")], gridparams=p$gridparams ), 
        							lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams )
        							)
							ab = cbind(aa,Z[locsmap,c('z','dZ','ddZ')])
				            j = which(is.na(ab$zO))
				            ab$zO[j] = ab$z[j]
				            ab$z = ab$zO
				            #time stamping for seasonal temperatures
				            ab$timestamp = as.POSIXct(ab$sdate,tz='America/Halifax',origin=lubridate::origin)
				            ab$timestamp = with_tz(ab$timestamp,"UTC")
				            ab$dyear = lubridate::decimal_date(ab$timestamp)- lubridate::year(ab$timestamp)
				       		ab$rdy <- round(ab$dyear,1)*10 #temperature column to pull from
				       		ab$ry <- year(ab$timestamp) -1950 + 1 #index year for temperature
				       	    ff = file.path(project.datadirectory('bio.temperature'),'modelled','t','canada.east','temperature.spatial.annual.seasonal.rdata')
				       		load(ff)
				       		print(paste('loading ',ff))

				            j = which(is.na(ab$bottom_temperature))
				            
				            locsmap = match( 
        							lbm::array_map( "xy->1", ab[j,c("plon","plat")], gridparams=p$gridparams ), 
        							lbm::array_map( "xy->1", baseLine, gridparams=p$gridparams )
        							)
				            k = ab[j,'ry']
				            l = ab[j,'rdy']
				            tp=c()
				            for(i in 1:length(j)){
				            	tp[i] = O[locsmap[i],k[i],l[i]]
				            }

				       		ab$bottom_temperature[j] <- tp
				            ab$t = ab$bottom_temperature


				            save(ab,file=file.path(fnProducts,'dfogeorgesHabitatData.rdata'))

	}

if(DS %in% c('prediction.surface','prediction.surface.redo')) {
	if(DS == 'prediction.surface'){
					load(file=file.path(fnProducts,'CanadaEastPredictionSurface.rdata'))
				    return(H)
			}
						load(file.path(project.datadirectory('bio.bathymetry'),'modelled','bathymetry.baseline.canada.east.rdata'))
						H = Z[,c('z','dZ','ddZ','plon','plat')]
						
						ff = file.path(project.datadirectory('bio.temperature'),'modelled','t','canada.east','temperature.spatial.annual.seasonal.rdata')
				       		load(ff)
				       	
  						for(y in p$yrs) {
    						T = O[,y-1950+1,]
    						if(p$annual.T.means) {
    							T = rowMeans(T,na.rm=TRUE)
    						} else {
    						d= p$dyear * 10
    						T = T[,d]
    					    }
    						H = data.frame(H,T)
    						names(H)[ncol(H)] <- paste('x',y,sep='.')    						 
    						}
    					save(H,file=file.path(fnProducts,'CanadaEastPredictionSurface.rdata'))
				        return(H)
						}
					}