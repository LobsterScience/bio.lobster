   

   p = list()
   p$init.files = c(p$init.files,loadfunctions(c("BIOsurvey",'polygons','lobster','groundfish','redfish','utility','parallel') ))
    p$libs = RLibrary( c( "chron", "lubridate", "parallel","sp","PBSmapping","RColorBrewer" ,'lattice','MASS','doBy','RODBC','ggplot2')  )
      
      p$current.assessment.year=2016

      p$lfas=c("27", "28", "29", "30", "31.1", "31.2", "32", "33", "34", "35", "36", "37", "38")
  

# Global lobster parameters
