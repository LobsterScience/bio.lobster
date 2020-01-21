#' @export
getGridVar = function(variable="DEPTH",source = c("atSea","FSRS"),grids){

	if(missing(grids)){
		logs = lobster.db("process.logs")
		grids = sort(unique(logs$GRID_NUM))
	}

	if("atSea" %in% source){
		lobster.db('atSea')
		atSea$X = atSea$LONGITUDE
		atSea$Y = atSea$LATITUDE
		x=tapply(atSea[,variable],atSea$GRIDNO,mean,na.rm=T)
		xd = data.frame(GRID=names(x),x)

		xd = subset(xd,GRID%in%grids)
		gridvar = xd
	}
	
	if("FSRS" %in% source){
		lobster.db('fsrs')
		fsrs$X = fsrs$LONG_DD
		fsrs$Y = fsrs$LAT_DD
		y=tapply(fsrs[,variable],fsrs$LFA_GRID,mean,na.rm=T)
		yd = data.frame(GRID=names(y),y)

		yd = subset(yd,GRID%in%grids)
		gridvar = yd
	}

	if("atSea" %in% source && "FSRS" %in% source){
		gridvar = merge(xd,yd,all=T)
		gridvar=data.frame(GRID=gridvar$GRID,rowMeans(gridvar[,2:3],na.rm=T))
	}
	names(gridvar)[2] = variable
	gridvar$GRID = as.numeric(gridvar$GRID)
	gridvar = merge(gridvar,data.frame(GRID=grids),all=T)


	if(any(is.na(gridvar))){

			LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
	    grids.dat<-calcCentroid(LFAgrid)
		  grids.dat = data.frame(GRID=grids.dat$SID,X=grids.dat$X,Y=grids.dat$Y)

	    if(variable=="DEPTH"){

			p = spatial_parameters( type = "canada.east" ) 
			grids.dat = lonlat2planar(grids.dat, input_names=c("X", "Y"),proj.type = p$internal.projection)
			load(file.path(project.datadirectory("bio.lobster"),"bathymetry","bathymetry.complete.canada.east.rdata"))
			Complete = Z
			# identify locations of data relative to baseline for envionmental data
			 locsmap = match( 
			  array_map( "xy->1", grids.dat[,c("plon","plat")], gridparams=p$gridparams ), 
			  array_map( "xy->1", Complete[,c("plon","plat")], gridparams=p$gridparams ) )

		 	grids.dat$DEPTH = Complete$z[locsmap]

		}
		#browser()
		missinggrids = gridvar[is.na(gridvar[,variable]),]$GRID

		gridvar=rbind(gridvar[!is.na(gridvar[,variable]),],subset(grids.dat,GRID%in%missinggrids,c("GRID",variable)))

	}


	return(gridvar)
	

} 