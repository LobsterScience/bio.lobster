#' @export

load.environment <- function(){
   p = list()
    p$project.name = "bio.lobster"

    p$libs = RLibrary( c( "devtools","roxygen2","geosphere", "chron",
                         'MASS','doBy','bio.lobster','bio.utilities',
                         "lubridate"))

          p$current.assessment.year=year(Sys.time())
      p$lfas=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")
      p$spatial.domain = "canada.east"  # for temperature/habitat lookups
      p = spatial_parameters(p=p)
    
	  p$nw=10       #for temperature merge
return(p)
}  

# Global lobster parameters
