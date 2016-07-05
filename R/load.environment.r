#' @export

load.environment <- function(){
   p = list()
    p$project.name = "bio.lobster"
    p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" ,'lattice','MASS','doBy','RODBC','ggplot2','bio.polygons','bio.groundfish','bio.utilities','bio.survey')  )
      
      p$current.assessment.year=2016

      p$lfas=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")
return(p)
}  

# Global lobster parameters
