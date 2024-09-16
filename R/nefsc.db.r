#' @title nefsc.db
#' @description Pulls the offshore lobster data from NEFSC trawl surveys as well as the strata areas. Included in the strata areas are the proportion of the total strata area incorporated in each of the offshore lobster subunits for post stratification.
#' @param \code{DS} :the selection of data, consists of a full data dump from ODBC account through \code{odbc.dump}. Or individual data tables can be rebuilt (with \code{.redo}) or loaded as \code{uscat},\code{usdet},\code{usinf},\code{usstrata.area}. Running any of the previous with the .clean arguement will clean up data, take care of vessel conversions and make it ready for further analysis
#' @param \code{fn.root} : specify the location of data saves, default is null and uses the project.datadirectory function as default
#' @return saves or loads .rdata objects named \code{usinf}, \code{usdet}, \code{uscat}, \code{usstrat.area}
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' nefsc.db(DS = 'odbc.dump.redo')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

nefsc.db <- function(DS  = 'odbc.dump.redo', fn.root=NULL,p=p){

if(grepl('odbc',DS))    db.setup() #Chooses RODBC vs ROracle based on R version and installed packages. db.setup(RODBC=T) will force RODBC
    if(is.null(fn.root)) {fn.root =  file.path( project.datadirectory("bio.lobster"), "data") }
    fnODBC  =  file.path(fn.root, "ODBCDump")

    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnODBC, recursive = TRUE, showWarnings = FALSE )

#if(grepl('redo.odbc',DS)) { #require(RODBC); channel = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F)} # believeNRows=F required for oracle db's

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
        return(usi)
      } 
       		     	#10=  NEST
                #11 = '36 YANKEE TRAWL'
                #41 = Mod. 41 Yankee Trawl (Accepted Code)
                #45 = Mod. 41 Yankee Trawl (Erroneously Coded on Several Cruises)
                ### matches inputs from B.Shank Sept 2016 ###
                ## need the distinct clause in the uss_mstr_cruise as some of the early cruises have multiple entries for a single survey-station combination

                  # usinf = sqlQuery(channel,paste("SELECT *
                  #   FROM  USNEFSC.USS_STATION a, 
                  #         (select distinct cruise6, season, year 
                  #             from usnefsc.uss_mstr_cruise 
                  #             where status_code in (10,15) 
                  #             and purpose_code = 10
                  #             and season in ('SPRING','FALL')
                  #             and year>=1968) b
                  #   where   a.cruise6 = b.cruise6
                  #       and to_number(SHG) <= 136
                  #       and stratum like '01%' 
                  #       ;")) 
                   
                  usinf = connect.command(con,paste("SELECT *
                    FROM  USNEFSC.USS_STATION a, 
                          (select distinct cruise6, season, year 
                              from usnefsc.uss_mstr_cruise 
                              where status_code in (10,15) 
                              and purpose_code = 10
                              and season in ('SPRING','FALL')
                              and year>=1968) b
                    where   a.cruise6 = b.cruise6
                        and to_number(SHG) <= 136
                        and stratum like '01%' 
                        ")) 
                  
                    usinf = rename.df(usinf,c('CRUISE6','STATION'),c('MISSION','SETNO'))
                    usinf$SETNO = as.numeric(usinf$SETNO)
                    usi=usinf
                    save(usi, file = file.path(fnODBC, 'usnefsc.inf.rdata'))
                    #odbcCloseAll()
				}

 if(DS %in% c('usinf.clean','usinf.clean.redo')) {
  				if(DS == 'usinf.clean') {
					load(file = file.path(fn.root, 'usnefsc.inf.clean.rdata'))
					return(inf)

  				}

  				inf = nefsc.db(DS = 'usinf')
  				inf$X = (inf$DECDEG_ENDLON + inf$DECDEG_BEGLON)/2
				inf$Y = (inf$DECDEG_ENDLAT + inf$DECDEG_BEGLAT)/2
				i = which(is.na(inf$X))
				inf$Y[i] = inf$DECDEG_BEGLAT[i]
				inf$X[i] = inf$DECDEG_BEGLON[i]
				vars2keep = c('MISSION','CRUISE','STRATUM','TOW','SETNO','SEASON','STATUS_CODE','ID','AREA','SVVESSEL','CRUNUM','SVGEAR','BEGIN_GMT_TOWDATE','GMT_YEAR','GMT_MONTH','GMT_DAY','TOWDUR','AVGDEPTH','BOTTEMP','BOTSALIN','DOPDISTB','X','Y')
  				inf = inf[,vars2keep]
  				inf$DIST = inf$DOPDISTB

  				sV = unique(inf$SVVESSEL)
  				inf1 = NULL
  				
  				#dealing with crazy tow distances 
  				
  				for(v in sV) {
  						if(v=='AL') {qb = c(0.05, 0.95)}
  						if(v=='DE') {qb = c(0.1, 0.95)}
  						if(v=='HB') {qb = c(0.005, 0.995)}

  						u = subset(inf,SVVESSEL == v)
  						ui = completeFun(u,c('DIST','TOWDUR'))
  						o = lm(DIST~TOWDUR, data=ui)
  						i = which.quantile(o$residuals,qb,inside=F)
  						iid = ui$ID[i]
  						u[which(u$ID %in% iid),'DIST'] <- NA
  						ui$DIST[i] <- NA
  						oi = lm(DIST~TOWDUR, data=ui)
  						uD = data.frame(ID = u$ID, DIST= predict(oi,newdata=data.frame(TOWDUR = u$TOWDUR)))
  						u = fillNaDf2(u, uD, mergeCols='ID',fillCols = 'DIST')
  						b = which(is.na(u$DIST))
  						u$DIST[b] = mean(u$DIST,na.rm=T)
  						u$DISTCORRECTION = u$DIST / mean(u$DIST,na.rm=T)
  						inf1 = rbind(inf1,u)
  				}
  				inf = inf1
  				#	inf$SEASON = recode(inf$GMT_MONTH,"2='Spring';3='Spring';4='Spring';5='Spring';9='Fall';10='Fall';11='Fall';12='Fall'")
  				#i = which(inf$SEASON %in% c('Spring','Fall'))
  				
  				#	inf = inf[i,]
  				#inf = lonlat2planar(inf,input_names=c('X','Y'),proj.type='lambert.conic.canada.east')
  				inf$ID = paste(inf$MISSION, inf$SETNO, sep=".")
  				
  				save(inf, file = file.path(fn.root, 'usnefsc.inf.clean.rdata'))
  				return(inf)
 }



if(DS %in% c('uscat', 'uscat.redo.odbc')) {
  if(DS == 'uscat') {
    
    load(file = file.path(fnODBC, 'usnefsc.catch.rdata'))
    return(uscat)
  } 
  
  # uscat = sqlQuery(channel, "select cruise6 mission,to_number(station) setno, stratum, 1 size_class, sum(expcatchwt) totwgt, 0 sampwgt, sum(expcatchnum) totno, 0 calwt
  #                         from  usnefsc.uss_catch 
  #                         WHERE to_number(svspp)=301
  #                         and stratum like '01%'
  #                         group by cruise6, to_number(station),stratum")
  
             uscat = connect.command(con, "select cruise6 mission,to_number(station) setno, stratum, 1 size_class, sum(expcatchwt) totwgt, 0 sampwgt, sum(expcatchnum) totno, 0 calwt
                                     from  usnefsc.uss_catch 
                                     WHERE to_number(svspp)=301
                                     and stratum like '01%'
                                     group by cruise6, to_number(station),stratum")
             
             save(uscat, file = file.path(fnODBC, 'usnefsc.catch.rdata'))
	         #odbcCloseAll()
        }

   if(DS %in% c('uscat.clean','uscat.clean.redo')) {
  				if(DS=='uscat.clean') {
					load(file = file.path(fn.root, 'usnefsc.cat.clean.rdata'))
					return(cainf)

  				}
  				ca = nefsc.db(DS='uscat')
  				ca$ID = paste(ca$MISSION, ca$SETNO, sep=".")
  				inf   = nefsc.db(DS='usinf.clean')
  				id    = unique(inf$ID)
  				cainf = merge(inf,ca,by=c('ID','MISSION','STRATUM','SETNO'),all.x=T)
  				cainf[,c('TOTWGT','SAMPWGT','TOTNO','CALWT')] <- na.zero(cainf[,c('TOTWGT','SAMPWGT','TOTNO','CALWT')])
  				i = which(is.na(cainf$SIZE_CLASS))
  				cainf$SIZE_CLASS[i] = 1

  				de = nefsc.db('usdet.clean.redo') # to adjust the catch rates for size based catchability
  				dea = aggregate(cbind(CLEN,CLEN*FWT/1000)~ID,data=de,FUN=sum)
  				names(dea)[2:3] = c('SAMPNO','SAMPWGT')
  				cainf$SUBSAMPLE = cainf$SAMPWGT / cainf$TOTWGT
  				cainf[which(!is.finite(cainf$SUBSAMPLE)),'SUBSAMPLE'] <- 1
  				cainf[which(cainf$SUBSAMPLE==0),'SUBSAMPLE'] <- 1
  				cainf = subset(cainf,select = -SAMPWGT)
  				cainf = merge(cainf,dea,by='ID',all.x=T)
  				o = which(is.na(cainf$SAMPNO))
  				cainf$SAMPNO[o] <- 0
  				o = which(is.na(cainf$SAMPWGT))
  				cainf$SAMPWGT[o] <- 0

  				cainf$TOTNO = cainf$SAMPNO / cainf$SUBSAMPLE
  				cainf$TOTWGT = cainf$SAMPWGT / cainf$SUBSAMPLE

  				cainf$TOTNO = cainf$TOTNO / cainf$DISTCORRECTION
				cainf$TOTWGT = cainf$TOTWGT / cainf$DISTCORRECTION  

				o = which(cainf$SVGEAR == 41)
				cainf$TOTNO[o] = cainf$TOTNO[o]* 36/41
				cainf$TOTWGT[o] = cainf$TOTWGT[o]* 36/41
				print('All catches are now in Bigelow Equivalents; implying that 13m wingspread can be used for all swept area calculations')
 
 				vars2keep = c('ID','MISSION','STRATUM','SETNO','CRUISE','BEGIN_GMT_TOWDATE','GMT_YEAR','AVGDEPTH','BOTTEMP','BOTSALIN','X','Y','DIST','DISTCORRECTION','SEASON','TOTWGT','TOTNO','CALWT','SUBSAMPLE')
  				cainf = cainf[,vars2keep]
  				save(cainf,file=file.path(fn.root, 'usnefsc.cat.clean.rdata'))
				return(cainf)
  			}



  if(DS %in% c('usdet','usdet.redo.odbc')) {

            if(DS == 'usdet') {
            
                  load(file = file.path(fnODBC, 'usnefsc.det.rdata'))
                   return(usdet)
            
                }

                #no trust in individual weights therefore not included # B Shank Sept 2016
           # raw.gsdet<- sqlQuery(channel, paste(" select cruise6 mission, stratum, to_number(station) setno, length, avg(indwt) fwt
           #              from usnefsc.uss_detail
           #              where to_number(svspp)=301
           #              and stratum like '01%'
           #              group by cruise6,station,stratum,length"))
            
          
            # usdet<- sqlQuery(channel,paste("select len.cruise6 mission, len.stratum, len.tow, len.station setno, len.svspp, len.catchsex fsex, len.length*10 as flen, len.expnumlen CLEN
            #               from usnefsc.uss_lengths len, 
            #               (select distinct cruise6, purpose_code, status_code, year, season from usnefsc.uss_mstr_cruise
            #                 where purpose_code =10
            #                 and STATUS_CODE in (10,15)
            #                 and YEAR >=1968
            #                 and season in ('SPRING','FALL')) cru  
            #               where len.cruise6 = cru.cruise6  
            #               and STRATUM like '01%'
            #               and len.svspp = 301
            #                ",sep=""))
            
            usdet<- connect.command(con,paste("select len.cruise6 mission, len.stratum, len.tow, len.station setno, len.svspp, len.catchsex fsex, len.length*10 as flen, len.expnumlen CLEN
                          from usnefsc.uss_lengths len, 
                          (select distinct cruise6, purpose_code, status_code, year, season from usnefsc.uss_mstr_cruise
                            where purpose_code =10
                            and STATUS_CODE in (10,15)
                            and YEAR >=1968
                            and season in ('SPRING','FALL')) cru  
                          where len.cruise6 = cru.cruise6  
                          and STRATUM like '01%'
                          and len.svspp = 301
                           ",sep=""))
            
            usdet$SETNO = as.numeric(usdet$SETNO)
            
            save(usdet, file = file.path(fnODBC, 'usnefsc.det.rdata'))
            #odbcCloseAll()
            
  }


if(DS %in% c('usdet.clean','usdet.clean.redo')) {
  
  if(DS == 'usdet.clean') {
    load(file = file.path(fn.root, 'usnefsc.det.clean.rdata'))
    return(de)
    
  }
  de = nefsc.db(DS = 'usdet')
  de$ID = paste(de$MISSION, de$SETNO, sep=".")
  inf = nefsc.db(DS = 'usinf.clean')
  #de$LENGTH = de$LENGTH*10 #to mm
  de$FWT = NA
  de$FSEX = bio.utilities::recode(de$FSEX,"0=0; 1=1; 2=2; 3=3; 4=2; 5=3") # 4 and 5 are for notched
  
  i = which(de$FSEX %in% c(1))
  de$FWT[i] = exp(-14.468) * de$FLEN[i] ^ 3.0781 * 2.204 #lw cov from GB 
  i = which(de$FSEX %in% c(2,3))
  de$FWT[i] = exp(-13.3388) * de$FLEN[i] ^ 2.8455 * 2.204 #lw cov from GB 
  i = which(de$FSEX %in% c(0))
  de$FWT[i] = exp(-13.7012) * de$FLEN[i] ^ 2.9212 * 2.204 #lw cov from GB 
  
  de$FWT    = de$FWT*1000 #to g
  #de = de[which(de$LENGTH<300),]
  de[which(de$FWT>11500),'FWT'] <- NA
  
  de = merge(de, inf[,c('SVVESSEL','SVGEAR','ID')],by = 'ID') #removes some of the sets that do match the filtered sets from inf.clean use CV's were quite high below 50mm and vessel corrections were not great
  
  load(file.path( bio.directory,'bio.lobster', "data", 'AlbatrossBigelowConv.rda')) #part of the bio.lobster Rpackage and is named 'a'
  a$Lm = a$CL * 10
  de$Lm = round(de$FLEN)
  de = merge(de,a,by='Lm',all.x=T) 
  de[which(de$SVVESSEL=='HB' & de$SVGEAR==10),'rho'] <- 1 #no conversion for the bigelow
  de[which(is.na(de$rho)),'rho'] <- 1 #no conversion for the bigelow
  de$CLEN = de$CLEN * de$rho
  save(de,file=file.path(fn.root, 'usnefsc.det.clean.rdata'))
  return(de)
  
}


if(DS %in% c('usstrata.area','usstrata.area.redo')) {
  if(DS == 'usstrata.area') {
    
    load(file = file.path(fnODBC, 'usnefsc.strata.area.rdata'))
    return(strata.area)
  }
  
  #strata.area = sqlQuery(channel,paste("select * from groundfish.gsstratum where strat like '01%' ;"))
  a = sf::read_sf(find.bio.gis('BTS_Strata')) 
  st_crs(a) <- 4326
  a$area = st_area(a)/1000000
  TUNITS = T #converting km to trawlable units
  if(TUNITS) {
    a$area = a$area*0.291553 # to square nm
    a$area = a$area / 0.00701944 # 13m net towed 1nm = 0.00701944*1 
  }
  
  st_geometry(a) <- NULL
  save(a, file = file.path(fnODBC, 'usnefsc.strata.area.rdata'))
      
        }
}

