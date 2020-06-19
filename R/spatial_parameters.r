#' @export

spatial_parameters = function( p=NULL, type=NULL ) {

    if (is.null(p)) p=list()

    if ( ! exists("spatial.domain", p) ) p$spatial.domain = type
    if ( ! is.null(type)) p$spatial.domain = type  # type has priority over p$spatial.domain


    if ( p$spatial.domain=="canada.east") {
        p$internal.projection = "lambert.conic.canada.east"
        p$internal.crs = "+proj=lcc +ellps=WGS84  +lon_0=62W +lat_0=45N +lat_1=43N +lat_2=47N +units=km "
        p$dres = 1/60/4  # this is the 15 second grid from CHS  .. ~ 0.25 km
        p$pres = 1  # km
        p$lon0=-72
        p$lon1=-52
        p$lat0=40
        p$lat1=50
        p$psignif = 1
    }

    if ( p$spatial.domain %in% c("canada.east.highres")) {
        p$internal.projection = "lambert.conic.canada.east"
        p$internal.crs = "+proj=lcc +ellps=WGS84  +lon_0=62W +lat_0=45N +lat_1=43N +lat_2=47N +units=km"
        p$dres = 1/60/4  # CHS is 15 arc second ~ 0.25 km
        p$pres = 0.5  # discretize to 0.5 km resolution
        p$lon0=-72
        p$lon1=-52
        p$lat0=40
        p$lat1=50
        p$psignif = 1
    }
    p$nlons = ceiling( diff(range(c(p$lon0,p$lon1)))/p$dres) + 1
    p$nlats = ceiling( diff(range(c(p$lat0,p$lat1)))/p$dres) + 1
    p$corners = data.frame(lon=c(p$lon0,p$lon1), lat=c(p$lat0,p$lat1))
    p$corners = lonlat2planar( p$corners, proj.type=p$internal.projection )
    p$corners$plon = round( p$corners$plon, p$psignif)  # this matches the p$pres value of x km resolution
    p$corners$plat = round( p$corners$plat, p$psignif)  # this matches the p$pres value of x km resolution
    plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    p$nplons = length(plons)
    p$nplats = length(plats)
   
    p$origin = c(min(p$corners$plon), min(p$corners$plat[1]))

    p$gridparams = list( dims=c(p$nplons, p$nplats), origin=p$origin, res=c(p$pres, p$pres) ) # used for fast indexing and merging

  return(p)
}

