assignGlorys <- function(
    x=g, x_spatial = c('LONGITUDE', "LATITUDE"), x_crs = 4326,
    temp=file.path(bio.lobster::project.datadirectory('bio.lobster.glorys'),'Glorys2000-2025wBiasCorrColumn_doy_june15.rds'),
    temp_spatial = c('X1000','Y1000'),
    temp_crs = 32620
    
          ){
  
  if(grepl('rds', temp)) {
    temp = readRDS(temp)
  } else {
    stop('needs to be an rds')
  }
  
  if( any(names(temp)=='geometry')) temp$geometry <- NULL  
    x = st_as_sf(x, coords = x_spatial, crs=x_crs)
    s = st_as_sf(temp,coords=temp_spatial,crs=temp_crs)
  if(x_crs != temp_crs) x <- st_transform(x,crs=st_crs(s))
  if(all(any(grepl('1000', temp_spatial)) &  !any(grepl('1000', x_spatial)))){
    st_geometry(x) = st_geometry(x)/1000
    st_crs(x) <- temp_crs
  }
    xc = st_coordinates(x)
    x$LO = xc[,1]
    x$LA = xc[,2]
    
    sc = st_coordinates(s)
    s$LO = sc[,1]
    s$LA = sc[,2]
    
    require(data.table)
    require(FNN)
    
    setDT(x)
    setDT(s)
    s$temperature = s$Glor + s$pred
    
    # Ensure date is Date type
    x[, DATE := as.Date(DATE)]
    s[, DATE := as.Date(Date)]
    
    x[, temperature := {
      
      td <- s[DATE == .BY$DATE]
      
      if (nrow(td) == 0) rep(NA, .N)
      else {
        nn <- get.knnx(
          data = td[, .(LO, LA)],
          query = .SD[, .(LO, LA)],
          k = 1
        )
        td$temperature[nn$nn.index]
      }
      
    }, by = DATE]

    
  
  
}
             