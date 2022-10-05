#' @export

snowcrab_sets <- function(){
  db.setup(un = oracle.snowcrab.user, pw = oracle.snowcrab.password)
  sc = connect.command(con, 'select * from sncrabsets')
  sb = connect.command(con, 'select * from sntrawlbycatch')
  
  sb = subset(sb,SPECCD_ID==2550)
  sb$ID = paste(sb$TRIP_ID,sb$SET_NO,sep="_")
  
  sc$ID = paste(sc$TRIP_ID,sc$SET_NO,sep="_")
  sc$START_LONG = sc$START_LONG *-1
  sc$END_LONG = sc$END_LONG *-1
  
  sc$dist0 = geosphere::distGeo( sc[,c("START_LONG", "START_LAT" )], sc[,c( "END_LONG", "END_LAT") ] )
  sc = sc[which(sc$dist0<800 | sc$dist>100),]
  sc = subset(sc,GEARCD_ID==12 )
  
  tots = merge(subset(sc,select=c(TRIP_ID,SET_NO,dist0,ID,BOARD_DATE,START_LONG,START_LAT)),subset(sb,select=c(ID,EST_NUM_CAUGHT,EST_DISCARD_WT)),all.x=T)
  tots$OFFSET = tots$dist0/1000*(10/1000)
  tots$SOURCE = 'Snow crab survey'
  tots$OFFSET_METRIC = 'TowedDist x wing spread km2'
  tots$Lobster = ifelse(!is.na(tots$EST_NUM_CAUGHT),tots$EST_NUM_CAUGHT,0)
  tots$EMPTY = ifelse(tots$Lobster==0,1,0)
  tots$WEIGHT_KG = ifelse(!is.na(tots$EST_DISCARD_WT),tots$EST_DISCARD_WT,0)
  tots$LATITUDE = tots$START_LAT
  tots$LONGITUDE = tots$START_LONG
  tots$DATE = tots$BOARD_DATE
  tots$ID = tots$dist0 = tots$EST_DISCARD_WT = tots$EST_NUM_CAUGHT = tots$START_LAT = tots$START_LONG = tots$BOARD_DATE = NULL
  tots$timestamp = as.POSIXct(tots$DATE,tz='America/Halifax',origin=lubridate::origin)
  tots$timestamp = with_tz(tots$timestamp,"UTC")
  tots$DYEAR = lubridate::decimal_date(tots$timestamp)- lubridate::year(tots$timestamp)
  tots$timestamp = NULL
 return(tots) 
}