#' @export
  lonlat2planar = function ( x,  input_names=c("lon", "lat"), newnames = c("plon", "plat") ) {
    #\\ convert lon/lat to a projected surface using proj
    #\\ proj.type can be an internal code such as "utm20" or a proj4 argument
    #\\ output scale is defined in the +units=km (default for bio) or +units=m (default for proj)

    # first try an internal conversion /lookup for CRS  
    proj4.params = try(sp::CRS("+init=epsg:32620"), silent=TRUE )
    
    
    crsX = rgdal::CRSargs( proj4.params)
    if ( ! grepl("units", crsX) ) {
      print (crsX)
      stop( "The proj4 CRS requires an explicit +units=km ")
    }

    y = rgdal::project( as.matrix(x[,input_names]), proj=crsX , inv=F ) 
    colnames(y) = newnames 
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL   
    }
    x = cbind(x,y)
    return (x)
  }


