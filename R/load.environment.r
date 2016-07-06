#' @export

load.environment <- function(){
   p = list()
    p$project.name = "bio.lobster"
    p$libs = RLibrary( c( 'rgdal',"geosphere", "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" ,'lattice','MASS','doBy','RODBC','bio.polygons','bio.groundfish','bio.utilities','bio.survey')  )
      
      p$current.assessment.year=2016

      p$lfas=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")
      p$internal.projection='utm20'
      p$nefsc.internal.projection='lambert.conic.us.4x'
return(p)
}  

# Global lobster parameters
