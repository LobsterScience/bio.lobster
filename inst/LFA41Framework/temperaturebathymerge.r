#temperature files
#use the spatial interpolation
#load('/home/bio.data/bio.temperature/interpolated/spatial/canada.east/spatial.interpolation.2014.rdata')
#this is broken into 10 times per year (each column is a segment)

  p = list( project.name = "temperature" )
  p$project.root = project.datadirectory( p$project.name )

  p$libs = RLibrary( c( "lubridate", "gstat", "sp", "rgdal", "parallel", "mgcv", "bigmemory", "fields" ) )
  p$init.files = bioLibrary(  "bio.spacetime", "bio.utilities", "bio.bathymetry", "bio.polygons" , "bio.temperature" )
  load_all('~/git/bio.temperature/')
  p$tyears = c(1950:2015)  # 1945 gets sketchy -- mostly interpolated data ... earlier is even more sparse.
  p$ny = length(p$tyears)
  p$nw = 10 # number of intervals in time within a year
  p$dyears = (c(1:p$nw)-1)  / p$nw # intervals of decimal years... fractional year breaks
  p$gam.optimizer = "nlm" ## other optimizers:: "bam" (slow), "perf"(ok), "nlm" (good), "bfgs" (ok), "newton" (default)
  p$nMin.tbot = p$ny*3 # min number of data points req before attempting to model timeseries in a localized space
  p$dist.km = c( 2.5, 5, 7.5, 10, 12.5, 15 ) # "manhattan" distances to extend search for data
  p$maxdist = 20 # if using gstat  max dist to interpolate in space
  p$tsmethod = "harmonics.1"
  p$spmethod = "kernel.density" ## best
  p$theta = 5 # FFT kernel bandwidth (SD of kernel) for method p$spmethod = "kernel.density"
  p$nsd = 6 # number of SD distances to pad boundaries with 0 for FFT  in method  p$spmethod = "kernel.density

  p$newyear = 2015

  p$subregions = c("canada.east", "SSE", "SSE.mpa", "snowcrab" ) # target domains and resolution
  p$spatial.domain.default = "canada.east"
  p = spatial.parameters( p=p, type=p$spatial.domain.default )  # default grid and resolution
  y = 2011
 
    p$clusters = rep("localhost", detectCores() )
    p = make.list( list( yrs=p$tyears), Y=p )
    P = temperature.db( p=p, DS="spatial.interpolation", yr=y  )
    
    #to get the lats and lons for temperature data
    O = bathymetry.db( p=p, DS="baseline" )
        p$wght = fields::setup.image.smooth(nrow=p$nplons, ncol=p$nplats, dx=p$pres, dy=p$pres, 
          theta=p$theta, xwidth=p$nsd*p$theta, ywidth=p$nsd*p$theta )
        p$O2M = cbind( (O$plon-p$plons[1])/p$pres + 1, (O$plat-p$plats[1])/p$pres + 1) # row, col indices in matrix form
    
        P[ P < -2 ] = -2  
        P[ P > 30 ] = 30 
        ibaddata = which( !is.finite(P) )
        P[ ibaddata ] = mean(P, na.rm=T )
        
    OP = cbind(O,P)
    names(OP) <- c('plon','plat','z','Jan','Feb','March','Apr','May','Jun','July','Aug','Sept','Oct')
levelplot(Jan~plon+plat,data=OP,aspect='iso',colorkey = terrain.colors(100))
