#' @export

scallop_sets <- function(){
    x= read.csv(file.path(project.datadirectory('bio.lobster'),'data','ScallopSurvey','berried_lobster_bycatch_Jan17_23.csv'))
    x$LONGITUDE = convert.dd.dddd(apply(x[,c('START_LONG','END_LONG')],1,FUN=mean))
    x$LATITUDE =  convert.dd.dddd(apply(x[,c('START_LAT','END_LAT')],1,FUN=mean))
    x$X1 = convert.dd.dddd(x[,c('START_LONG')])
    x$Y1 = convert.dd.dddd(x[,c('START_LAT')])
    x$X2 = convert.dd.dddd(x[,c('END_LONG')])
    x$Y2 = convert.dd.dddd(x[,c('END_LAT')])
    gear_width= 5.4864 #18ft A Glass
    x$OFFSET = apply(x,1,function(y) geosphere::distm(c(y['X1'],y['Y1']),c(y['X2'],y['Y2'])))/1000 * gear_width/1000
    x$SOURCE = 'Scallop survey'
    x$OFFSET_METRIC = 'dist x spread km2'
    x$DATE = as.Date(x$TOW_DATE)
    x$YEAR = lubridate::year(x$DATE)
    x$DYEAR = lubridate::yday(x$DATE)/365
    x$Lobster = x$TOTAL_LOBSTERS
    x$Berried = x$TOTAL_BERRIED
    x$Legal = x$WEIGHT_KG=NA
    bigO = quantile(x$OFFSET,0.99)
    littleO = quantile(x$OFFSET,0.01)
    x$TEMP = x$BOTTOM_TEMP
    
    x = subset(x,littleO<OFFSET & OFFSET<bigO,select=c(DATE, YEAR, LONGITUDE, LATITUDE,WEIGHT_KG, OFFSET, TEMP,SOURCE, OFFSET_METRIC,DYEAR,Lobster, Berried,Legal))
  return(x)
    }