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

levelplot(Jan~plon+plat,data=OP,aspect='iso',col.regions = color.code('seis',150))




#logbook data

require(bio.survey)
require(bio.lobster)
la()
p = bio.lobster::load.environment()
p$libs = NULL

require(bio.utilities)
require(bio.habitat)
require(raster)
loadfunctions('bio.habitat')
loadfunctions('bio.utilities')
loadfunctions('bio.indicators')
loadfunctions('bio.temperature')

lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))

logs41$yr = year(logs41$DATE_FISHED) #2002 to present
ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
ziff41$DDLON = ziff41$DDLON * -1
off41$yr  = year(off41$DATE_FISHED) #1981 to 1994

logs41$OFFAREA = NULL 

#oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

a41 = rbind(off41,ziff41,logs41)
a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)

a41 = lonlat2planar(a41,input_names = c('DDLON','DDLAT'),proj.type = p$internal.projection)
a41$plon = grid.internal(a41$plon,p$plons)
a41$plat = grid.internal(a41$plat,p$plats)
a41$z = NA
a41$depth = NULL
 
a41 = completeFun(a41,c('plon','plat'))
a41 = subset(a41,DDLAT>0)
a41 = subset(a41, MON_DOC_ID!=153219950609)
a41 = habitat.lookup(a41,p=p,DS='depth')

#clean up some errors
a41$z[which(a41$z>450)] <- NA

hist(a41$z,'fd',xlab='Depth',main="")

#time stamping for seasonal temperatures

a41$timestamp = as.POSIXct(a41$DATE_FISHED,tz='America/Halifax',origin=lubridate::origin)
a41$timestamp = with_tz(a41$timestamp,"UTC")
a41$dyear = lubridate::decimal_date(a41$timestamp)- lubridate::year(a41$timestamp)
a41 = subset(a41,fishingYear<2016)

a41 = habitat.lookup(a41,p=p,DS='temperature.seasonal')
a41 = habitat.lookup(a41,p=p,DS='substrate')
