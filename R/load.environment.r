#' @export

load.environment <- function(){
   p = list()
    p$project.name = "bio.lobster"
    p$libs = RLibrary( c( 'rgdal',"geosphere", "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" ,'lattice','MASS','doBy','bio.polygons','bio.groundfish','bio.utilities','bio.survey','bio.spacetime','raster','bio.habitat')  )
      
      p$current.assessment.year=2016

      p$lfas=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")
      p$spatial.domain = "canada.east"  # for temperature/habitat lookups
      p = spatial.parameters(p=p)
	  p$nw=10       #for temperature merge
return(p)
}  

# Global lobster parameters
