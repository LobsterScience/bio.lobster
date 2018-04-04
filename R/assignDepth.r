assignDepth = function(data, input_names = c("X","Y"), type= "canada.east", DS="complete"){
	
	require(bio.bathymetry)
	require(lbm)

	X = data

	p = spatial_parameters( type = type ) 

	X = lonlat2planar(X, input_names=input_names,proj.type = p$internal.projection)

	Z = bathymetry.db(p=p, DS=DS)
	#X = planar2lonlat(Z, proj.type = p$internal.projection)
 
 	# identify locations of data relative to baseline for envionmental data
 	locsmap = match( 
  		lbm::array_map( "xy->1", X[,c("plon","plat")], gridparams=p$gridparams ), 
  		lbm::array_map( "xy->1", Z[,c("plon","plat")], gridparams=p$gridparams ) )
 

 	data$z = Z$z[locsmap]

 	return(data)

 }

