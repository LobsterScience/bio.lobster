assignGlorys <- function(
    x=go, x_spatial = c('LONGITUDE', "LATITUDE"), x_crs = 4326,
    temp=file.path(bio.lobster::project.datadirectory('bio.lobster.glorys'),'Glorys2000-2025wBiasCorrColumn_doy_july29.rds'),
    temp_spatial = c('X','Y'),
  temp_crs = 32620,tmp_dir = 'C:/Users/cooka/Documents/tmp/'
    
          ){
	cat("WARNING: This function is very slow.\n")
	  cat("Make sure you have sufficient time and RAM before running.\n")
	  ans <- readline("Type 'y' to continue or 'n' to stop: ")
    
	    if (tolower(ans) != "y") {
		        stop("Execution stopped by user.", call. = FALSE)
    		}

  if(grepl('rds', temp)) {
    temp = readRDS(temp)
  } else {
    stop('needs to be an rds')
  }
  if( any(names(temp)=='geometry')) temp$geometry <- NULL  
    x = st_as_sf(x, coords = x_spatial, crs=x_crs)
    s = st_as_sf(temp,coords=temp_spatial,crs=temp_crs)
    
  if(x_crs != temp_crs) {
    x <- st_transform(x,crs=st_crs(s))
    st_geometry(x) =  st_geometry(x)/1000
    st_crs(x) = temp_crs
  }
  
s$Temperature = s$Glor+s$pred
s$Date = as.Date(s$Date)
x$Date = as.Date(x$DATE)
dy=unique(x$YEAR)

for(k in seq_along(dy)){
	        v = subset(x,YEAR == dy[k])
	        l = subset(s,yr==dy[k])
	        saveRDS(list(v,l), file=file.path(tmp_dir,paste0('Gl_ob',dy[k],'.rds')))
}

v = dir(tmp_dir,full.names = T)
v = v[grep('Gl_ob',v)]
#file.remove(v)

for(i in 1:length(v)) {
	                b = readRDS(v[i])
	                b1 = b[[1]]
	                b2=b[[2]]
	                ud =unique(b1$Date)
	           for(j in seq_along(ud)){
                        g = subset(b1,Date==ud[j])
			 k = subset(b2,Date==ud[j])
		g$dist = g$Glor = NA	
		   	   js = st_as_sf(g)
				ks = st_as_sf(k)
				  for(l in 1:nrow(g)){
				        b = st_nearest_feature(js[l,],ks)
				        g[l,'dist'] = st_distance(js[l,],ks[b,])
				        g[l,'Glor'] = ks[b,'Temperature']
					}
			saveRDS(g,file=file.path(tmp_dir,paste0('combGL_DA',ud[j],'.rds')))
						}
				rm(b)
				rm(b1)
				rm(b2)
				gc()
			}
file.remove(v)


out = list()
v = dir(tmp_dir,full.names = T)
v = v[grep('combGL_DA',v)]

for(i in 1:length(v)){
	
	out[[i]] = readRDS(v[i])
}
file.remove(v)
oi = do.call(rbind,out)

ois = subset(oi,dist<10)
saveRDS(ois,file=file.path(bio.lobster::project.datadirectory('bio.lobster.glorys'),'lobsterData_withGlorys_sept9.rds'))
    return(ois)
  
  
}
             
