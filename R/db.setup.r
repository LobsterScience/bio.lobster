#' db.setup
#' 
#' This function allows for automatic selection of ROracle and RODBC packages as the default for Oracle database queries. ROacle is the default. If running 32-bit R or do not have ROracle installed, RODBC will be selected.  
#' @return "con" which defines details of the Oracle connection. Structured differently for RODBC and ROracle.
#' @author Ben Zisserson
#' @export

db.setup=function(RODBC=F){
  if(!.Platform$OS.type == "unix") {
  if(!grepl('64',version$arch) || RODBC) { #in 32-bit R, only RODBC will work
      print("Using RODBC package for Oracle db connections")
      require(RODBC)
      con <<- odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
      connect.command<<-function(...) {
         sqlQuery(...)}
        }
 
  else {
 
    t=try(library(ROracle), silent=T)   
    
    #if ROracle won't load, default to RODBC'
    if ("try-error" %in% class(t)) {
      stop('You must use  R 32bit with RODBC')
      }
  
    #if ROracle package will load
            else {
            print("Using ROracle for Oracle db connections")
            require('ROracle')
            con<<-dbConnect(DBI::dbDriver("Oracle"),username=oracle.username, password=oracle.password, dbname=oracle.server)
            }
         
   connect.command<<-function(...) {
             if ("try-error" %in% class(t)) {
               sqlQuery(...)
             } else {
               dbGetQuery(...)
             }
              }
        }
  }
}  


