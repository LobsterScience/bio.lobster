#' @export

load.environment <- function(){
   p = list()
    p$project.name = "bio.lobster"
    p$libs = RLibrary( c( 'rgdal',"devtools","roxygen2","geosphere", "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" ,'lattice','MASS','doBy','bio.lobster','bio.polygons','bio.utilities','bio.lobster','bio.survey','bio.spacetime','raster','lbm')  )
      p$current.assessment.year=2017

      p$lfas=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")
      p$spatial.domain = "canada.east"  # for temperature/habitat lookups
      p = spatial_parameters(p=p)
	  p$nw=10       #for temperature merge
return(p)
}  

# Global lobster parameters
