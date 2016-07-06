#' @title nefsc.db
#' @description Pulls the offshore lobster data from NEFSC trawl surveys
#' @param \code{DS} :the selection of data, consists of a full data dump from ODBC account through \code{odbc.dump}. Or individual data tables can be rebuilt (with \code{.redo}) or loaded as \code{uscat},\code{usdet},\code{usinf},\code{usstrata.area}
#' @param \code{fn.root} : specify the location of data saves, default is null and uses the project.datadirectory function as default
#' @return saves or loads .rdata objects named \code{usinf}, \code{usdet}, \code{uscat}, \code{usstrat.area}
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' nefsc.db(DS = 'odbc.dump.redo')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

nefsc.db <- function(DS  = 'odbc.dump.redo', fn.root=NULL,p=p){

    if(is.null(fn.root)) {fn.root =  file.path( project.datadirectory("bio.lobster"), "data") }
    fnODBC  =  file.path(fn.root, "ODBCDump")

    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnODBC, recursive = TRUE, showWarnings = FALSE )


if(grepl('redo.odbc',DS)) channel = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

options(stringsAsFactors = FALSE) #necessary?
options(scipen=999)  # this avoids scientific notation
       
  if(DS %in% c('odbc.dump','odbc.dump.redo')) {
                if(DS == 'odbc.dump') {
                      nefsc.db(DS = 'uscat')        
                      nefsc.db(DS = 'usinf')        
                      nefsc.db(DS = 'usdet')        
                      nefsc.db(DS = 'usstrata.area')        
                      return('Done')
                       }
                  nefsc.db(DS = 'uscat.redo.odbc',fn.root)        
                  nefsc.db(DS = 'usinf.redo.odbc',fn.root)        
                  nefsc.db(DS = 'usdet.redo.odbc',fn.root)        
                  nefsc.db(DS = 'usstrata.area.redo.odbc',fn.root)        
                  
                }


  if(DS %in% c('usinf', 'usinf.redo.odbc')) {
      if(DS == 'usinf') {

        load(file = file.path(fnODBC, 'usnefsc.inf.rdata'))
        return(usinf)
      } 
       			#10=  NEST
                #11 = '36 YANKEE TRAWL'
                #41 = Mod. 41 Yankee Trawl (Accepted Code)
                #45 = Mod. 41 Yankee Trawl (Erroneously Coded on Several Cruises)
              
                  usinf = sqlQuery(channel,paste("SELECT *
                    FROM USNEFSC.USS_STATION 
                    where to_number(SHG) <= 136
                    and stratum like '01%' and svgear in (10,11,41,45) and GEARCOND in (1,2,3) and haul in (1,2) and statype = 1"))
                    names(usinf)[1] = 'MISSION'
                    save(usinf, file = file.path(fnODBC, 'usnefsc.inf.rdata'))

  }

 if(DS %in% c('usinf.clean','usinf.clean.redo')) {
  				if(DS == 'usinf.clean') {
					load(file = file.path(fn.root, 'usnefsc.inf.clean.rdata'))
					print('Returning inf')
					return(inf)

  				}

  				inf = nefsc.db(DS = 'usinf')
  				inf$X = (inf$DECDEG_ENDLON + inf$DECDEG_BEGLON)/2
				inf$Y = (inf$DECDEG_ENDLAT + inf$DECDEG_BEGLAT)/2
				vars2keep = c('MISSION','CRUISE','STRATUM','TOW','STATION','STATUS_CODE','ID','AREA','SVVESSEL','CRUNUM','SVGEAR','BEGIN_GMT_TOWDATE','GMT_YEAR','GMT_MONTH','GMT_DAY','TOWDUR','AVGDEPTH','BOTTEMP','BOTSALIN','DOPDISTB')
  				inf = inf[,vars2keep]
  				inf$DIST = inf$DOPDISTB
  				inf$SEASON = recode(inf$GMT_MONTH,"2='Spring';3='Spring';4='Spring';5='Spring';9='Fall';10='Fall';11='Fall';12='Fall'")
				i = which(inf$SEASON %in% c('Spring','Fall'))
  				inf = inf[i,]
  				inf = lonlat2planar(inf,input_names=c('X','Y'),proj.type=p$nefsc.internal.projection)
				inf$ID = paste(inf$MISSION, inf$STATION, sep=".")
			save(inf, file = file.path(fn.root, 'usnefsc.inf.clean.rdata'))
  				}



  if(DS %in% c('uscat', 'uscat.redo.odbc')) {
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

   if(DS %in% c('uscat.clean','usinf.cat.clean.redo')) {
  				if(DS=='uscat.clean') {
					load(file = file.path(fn.root, 'usnefsc.cat.clean.rdata'))
					print('Returning cat')
					return(cat)

  				}

  				ca = nefsc.db(DS='uscat')
  				ca$ID = paste(ca$MISSION, ca$SETNO, sep=".")
  				inf = nefsc.db(DS='usinf.clean')
  				unique()

  			}

  if(DS %in% c('usdet','usdet.redo.odbc')) {

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
      

    if(DS %in% c('usstrata.area','usstrata.area.redo.odbc')) {
        if(DS == 'usstrata.area') {
          
          load(file = file.path(fnODBC, 'usnefsc.strata.area.rdata'))
          return(strata.area)
        }

        strata.area = sqlQuery(channel,paste("select * from groundfish.gsstratum where strat like '01%' ;"))

        strata.area$CanadaOnly = 0
        strata.area$CanadaOnly[which(strata.area$STRAT==1160)] = 0.5211409 #proportion of strata in Canada
        strata.area$CanadaOnly[which(strata.area$STRAT==1170)] = 0.7888889 #proportion of strata in Canada
        strata.area$CanadaOnly[which(strata.area$STRAT==1180)] = 0.7383721 #proportion of strata in Canada
		strata.area$CanadaOnly[which(strata.area$STRAT==1210)] = 0.4952830 #proportion of strata in Canada
		strata.area$CanadaOnly[which(strata.area$STRAT==1220)] = 0.2753304 #proportion of strata in Canada
		strata.area$USOnly = 1 - strata.area$CanadaOnly 
        save(strata.area, file = file.path(fnODBC, 'usnefsc.strata.area.rdata'))
        }
odbcCloseAll()
}




