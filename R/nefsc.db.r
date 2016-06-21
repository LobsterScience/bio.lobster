#' @title nefsc.db
#' @description Pulls the offshore lobster data from NEFSC trawl surveys
#' @param \code{DS} = the selection of data, consists of a full data dump from ODBC account through \code{odbc.dump}. Or individual data tables can be rebuilt (with \code{.redo}) or loaded as \code{uscat},\code{usdet},\code{usinf},\code{usstrata.area}
#' @return saves or loads .rdata objects named \code{usinf}, \code{usdet}, \code{uscat}, \code{usstrat.area}
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' nefsc.db(DS = 'odbc.dump.redo')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

nefsc.db <- function(DS  = 'odbc.dump.redo'){
    fn.root =  file.path( project.datadirectory("lobster"), "data") 
    fnODBC  =  file.path(fn.root, "ODBCDump")

if(grepl('redo',DS)) channel = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

options(stringsAsFactors = FALSE) #necessary?
options(scipen=999)  # this avoids scientific notation
       
  if(DS %in% c('odbc.dump','odbc.dump.redo')) {
                if(DS == 'odbc.dump') {
                      usnefsc.db(DS = 'uscat')        
                      usnefsc.db(DS = 'usinf')        
                      usnefsc.db(DS = 'usdet')        
                      usnefsc.dn(DS = 'usstrata.area')        
                      return('Done')
                       }
                  usnefsc.db(DS = 'uscat.redo')        
                  usnefsc.db(DS = 'usinf.redo')        
                  usnefsc.db(DS = 'usdet.redo')        
                  usnefsc.dn(DS = 'usstrata.area.redo')        
                }


  if(DS %in% c('uscat', 'uscat.redo')) {
      if(DS == 'uscat') {
        
          load(file = file.path(fnODBC, 'usnefsc.catch.rdata'))
          return(uscat)
        } 

             uscat = sqlQuery(channel, "select cruise6 mission,to_number(station) setno, stratum, 1 size_class, sum(expcatchwt) totwgt, 0 sampwgt, sum(expcatchnum) totno, 0 calwt
                                     from  usnefsc.uss_catch 
                                     WHERE to_number(svspp)=301
                                     and stratum like '01%'
                                     group by cruise6, to_number(station),stratum")

             save(uscat, file = file.path(fnODBC, 'usnefsc.catch.rdata'))

        }

  if(DS %in% c('usinf', 'usinf.redo')) {
      if(DS == 'usinf') {

        load(file = file.path(fnODBC, 'usnefsc.inf.rdata'))
        return(usinf)
      } 
                  usinf = sqlQuery(channel,paste("SELECT *
                    FROM USNEFSC.USS_STATION 
                    where to_number(SHG) <= 136
                    and stratum like '01%'"))
                    names(usinf)[1] = 'MISSION'
                    save(usinf, file = file.path(fnODBC, 'usnefsc.inf.rdata'))
  }

  if(DS %in% c('usdet','usdet.redo')) {

            if(DS == 'usdet') {
            
                  load(file = file.path(fnODBC, 'usnefsc.det.rdata'))
                   return(usdet)
            
                }

            raw.gsdet<- sqlQuery(channel, paste(" select cruise6 mission, stratum, to_number(station) setno, length, avg(indwt) fwt
                          from usnefsc.uss_detail
                           where to_number(svspp)=301
                           and stratum like '01%'
                            group by cruise6,station,stratum,length"))
            
          
            raw.lf<- sqlQuery(channel,paste("select cruise6 mission, stratum, catchsex fsex, station setno,length, 
                sum(expnumlen) clen, 1 size_class
                from usnefsc.uss_lengths
                where to_number(svspp)=301
                and catchsex in ('0','1','2','3')
                and stratum like '01%'
                group by cruise6,stratum,station,length, catchsex",sep=""))
            
          raw.gsdet<-merge(raw.gsdet,raw.lf, all.x=T) 
          raw.gsdet$FLEN[is.na(raw.gsdet$FLEN)] <- raw.gsdet$LENGTH[is.na(raw.gsdet$FLEN)]
          usdet = raw.gsdet
          save(usdet, file = file.path(fnODBC, 'usnefsc.det.rdata'))
            }
      

    if(DS %in% c('usstrata.area','usstrata.area.redo')) {
        if(DS == 'usstrata.area') {
          
          load(file = file.path(fnODBC, 'usnefsc.strata.area.rdata'))
          return(strata.area)
        }

        strata.area = sqlQuery(channel,paste("select * from groundfish.gsstratum where strat like '01%' ;"))
        save(strata.area, file = file.path(fnODBC, 'usnefsc.strata.area.rdata'))
        }
odbcCloseAll()
}




