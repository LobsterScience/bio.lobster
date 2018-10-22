#' lobster.db
#' 
#' This function is the main workhorse to pull data from databases and some initial filtering of data used in lobster stock assessments. Results are saved and can be reloaded using this function.
#' @param DS is the main switch that selects which data source to load or operate. Options for DS include 'complete','annual.landings','logs','logs41','logs41jonah','observer41','atSea','cris','port','vlog','fsrs','scallop','survey','annual.landings'.  Any of these arguements called as listed return the data object. To make the data file from scratch would require a 'XXXX.redo', where XXXX is the option listed above. 
#' @return Data objects that contain the data for use in further analyses.
#' @examples lobster.db('fsrs.redo') # makes the data objects for the FSRS data.
#' lobster.db('fsrs') #loads the object fsrs
#' @export

  lobster.db = function( DS="complete.redo",p=p) {
    options(stringsAsFactors=F)

  require(lubridate)
    fn.root =  file.path( project.datadirectory('bio.lobster'), "data") 
    fnODBC  =  file.path(fn.root, "ODBCDump")
    fnProducts = file.path(fn.root,'products')
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnODBC, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnProducts, recursive = TRUE, showWarnings = FALSE )
    
    if (DS %in% c("complete.redo") ) {

        # ODBC data dump of lobster data
        lobster.db(DS == 'port.redo')
        lobster.db( DS="logs.redo")
        lobster.db( DS="logs41.redo")
        lobster.db( DS="logs41jonah.redo")
        lobster.db( DS="observer41.redo")
        lobster.db( DS="fsrs.redo")
        lobster.db( DS="fsrs.commercial.samples.redo")
        lobster.db( DS="atSea.redo")
        lobster.db( DS="atSea.clean.redo")        
        lobster.db( DS="atSea.logbook.link.redo")
        lobster.db( DS="cris.redo")
        lobster.db( DS="ccir.redo")
        lobster.db( DS="port.samples.redo")
        lobster.db( DS="vlog.redo")
        lobster.db( DS="scallop.redo")
        lobster.db( DS="survey.redo")
        lobster.db( DS="annual.landings.redo")
        lobster.db( DS="seasonal.landings.redo")
        lobster.db( DS="historical.landings.redo")
        lobster.db( DS="season.dates.redo")
        lobster.db(DS = "lfa41.vms.redo")
        lobster.db(DS= "logs41.habitat.redo")
        lobster.db(DS = 'landings.by.port.redo')
        }
      
  if(DS %in% c('port','port.redo')){
        if(DS == 'port') {
                  load(file=file.path(fnODBC,'ports.rdata'))
                return(ports)
          }
                  con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  ports = sqlQuery(con,'select * from LOBSTER.port')
                  save(ports,file=file.path(fnODBC,'ports.rdata'))          
        }

  if(DS %in% c('atSea.logbook.link','atSea.logbook.link.redo')){
        if(DS == 'atSea.logbook.link') {
                  load(file=file.path(fnODBC,'atSea.logbook.link.rdata'))
                return(links)
          }
                  con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  links = sqlQuery(con,'select * from LOBSTER.ATSEA_LOG_LINK;')
                  save(links,file=file.path(fnODBC,'atSea.logbook.link.rdata'))          
        }

if(DS %in% c('landings.by.port.redo','landings.by.port')) {

  if(DS == 'landings.by.port.redo') {

     con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                          #1975 - 1996
                          oldd = sqlQuery(con,paste("SELECT date_landed da, prov_code||district||port_code port, cfv boatvesid, wt_lbs, lfa FROM
                                  (SELECT date_landed,
                                prov_code,    CASE      WHEN district = ' 1'      THEN '01'      WHEN district = ' 2'      THEN '02'      WHEN district = ' 3'      THEN '03'      WHEN district = ' 4'      THEN '04'      WHEN district = ' 5'      THEN '05'
                                  WHEN district = ' 6'      THEN '06'      WHEN district = ' 7'      THEN '07'      WHEN district = ' 8'      THEN '08'      WHEN district = ' 9'      THEN '09'      ELSE district    END district,
                                CASE      WHEN port_code = ' 1'      THEN '01'      WHEN port_code = ' 2'      THEN '02'      WHEN port_code = ' 3'      THEN '03'      WHEN port_code = ' 4'      THEN '04'      WHEN port_code = ' 5'      THEN '05'
                                  WHEN port_code = ' 6'      THEN '06'      WHEN port_code = ' 7'      THEN '07'      WHEN port_code = ' 8'      THEN '08'      WHEN port_code = ' 9'      THEN '09'      ELSE port_code    END port_code,    match_cfv CFV,
                                landed_qty_lbs_n WT_LBS,    CASE      WHEN lobster_district = '1'      THEN '36'      WHEN lobster_district = '2'      THEN '38'      WHEN lobster_district = '3'      THEN '35'      WHEN lobster_district = '5'
                                  THEN '31_32'      WHEN lobster_district = '5A'      THEN '32'      WHEN lobster_district = '5B'      THEN '31A'      WHEN lobster_district = '5B1'      THEN '31B'      WHEN lobster_district = '6A'      THEN '30'
                                  WHEN lobster_district = '6B'      THEN '27'      WHEN lobster_district = '7A'      THEN '29'      WHEN lobster_district = '7A1'      THEN '28'      WHEN lobster_district       = 'A'      AND SUBSTR(date_landed,1,4) > 1976
                                  THEN '41'      WHEN match_cfv             IN ('000530','000790','001614','005366','005461','005569','012136','012148')     AND SUBSTR(date_landed,1,4) = 1975      THEN '41'  WHEN match_cfv             IN ('000530','001614','005366','005461','005569','012148')
                                  AND SUBSTR(date_landed,1,4) = 1976     THEN '41'   WHEN lobster_district = '4B'  AND match_cfv NOT    IN ('000115','000530','000790','001530','001532','001540','001550','001614','004005','004034','004056', '005366','005461','005569','005611','005690','012136','012148','100989','101315','100990')
                                  THEN '33'     WHEN lobster_district = '4A'  AND match_cfv NOT    IN ('000115','000530','000790','001530','001532','001540','001550','001614','004005','004034','004056', '005366','005461','005569','005611','005690','012136','012148','100989','101315','100990')
                                  THEN '34'  WHEN lobster_district = '7B'   THEN '7B'   ELSE 'NA'   END LFA  FROM lobster.lob_log_est_1975_1996  WHERE species_code          = 700  AND SUBSTR(date_landed,1,4) <1997 ) "))
                           oldd <- transform(oldd, DA = as.Date(as.character(DA), "%Y%m%d"))
                           oldd = subset(oldd,year(oldd$DA)>1979)
                        #1997 - 2001
                          midd = sqlQuery(con,paste("SELECT date_fished da,  port_landed port,  licence_id boatvesid,  weight_lbs wt_lbs,  lfa FROM lobster.LOB_LOG_EST_1997_2001"))

                        #2002 - current
                            newd = sqlQuery(con,paste("SELECT date_fished da,  community_code port ,  licence_id boatvesid,  NVL(weight_lbs,0)+NVL(weight_lbs_b,0)+NVL(weight_lbs_c,0) wt_lbs,  lfa FROM marfissci.lobster_sd_log"))
                         
                          dats = rbind(oldd,midd,newd)
                          dats = subset(dats,LFA<41)
                          dats = addSYEAR(dats,'DA')
                          season.dates = lobster.db('season.dates')

                        dats$WOS = NA
                        lfa = unique(dats$LFA) 
                            for(i in 1:length(lfa)) {
                                  h  = season.dates[season.dates$LFA==lfa[i],]  
                               for(j in unique(dats$SYEAR[dats$LFA==lfa[i]])){
                                   dats$WOS[dats$LFA==lfa[i] & dats$SYEAR==j] = floor(as.numeric(dats$SDATE[dats$LFA==lfa[i] & dats$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                          }
                          
                        dats$WEIGHT_KG = dats$WT_LBS*0.453592
                      if(any(!is.finite(dats$WOS))) {kl = which(!is.finite(dats$WOS)); dats$WOS[kl] = NA}
                      dats = aggregate(WEIGHT_KG~PORT+SDATE+WOS+SYEAR+LFA,data=dats,FUN=sum)
                      dats = subset(dats,WOS>0)
                       save(dats,file=file.path(fnProducts,'landings.by.port.rdata'))
                  }
               
               load(file=file.path(fnProducts,'landings.by.port.rdata'))
               return(dats)
      
          }

if(DS %in% c('community.to.grid.historic.redo','community.to.grid.historic')){

  if(grepl('redo',DS)) {
      #proportion of old by grid using proportions of landings by WOS and Community into grids using logs from 2002-2009
                a = lobster.db('process.logs')
                b = aggregate(WEIGHT_KG~LFA+GRID_NUM+COMMUNITY_CODE+WOS,data=subset(a,SYEAR<2009),FUN=sum)
                bb = aggregate(WEIGHT_KG~LFA,data=subset(a,SYEAR<2009),FUN=sum)
                    require(bio.utilities)
                    bb = rename.df(bb,'WEIGHT_KG','TOTWGT')
                    bbb = merge(b,bb,all.x=T)
                    bbb$p = bbb$WEIGHT_KG / bbb$TOTWGT
                    bbb = bbb[,c('LFA','COMMUNITY_CODE','WOS','GRID_NUM','p')]
                    names(bbb) =c('LFA','PORT','WOS','GRID_NUM','PropLand')
                    bbb$SD = substr(bbb$PORT,2,3)
                    com2grid = bbb
                    save(com2grid,file=file.path(fnODBC,'community.to.grid.historic.rdata'))
                 }
          load(file.path(fnODBC,'community.to.grid.historic.rdata'))
          return(com2grid)
}


if(DS %in% c('community.to.grid.contemporary.redo','community.to.grid.contemporary')){

  if(grepl('redo',DS)) {
      #proportion of old by grid using proportions of landings by WOS and Community into grids using logs from 2002-2009
                a = lobster.db('process.logs')
                b = aggregate(WEIGHT_KG~LFA+GRID_NUM+COMMUNITY_CODE+WOS+SYEAR,data=a,FUN=sum)
                bb = aggregate(WEIGHT_KG~LFA+SYEAR,data=a,FUN=sum)
                    require(bio.utilities)
                    bb = rename.df(bb,'WEIGHT_KG','TOTWGT')
                    bbb = merge(b,bb,all.x=T)
                    bbb$p = bbb$WEIGHT_KG / bbb$TOTWGT
                    bbb = bbb[,c('LFA','SYEAR','COMMUNITY_CODE','WOS','GRID_NUM','p')]
                    names(bbb) =c('LFA','SYEAR','PORT','WOS','GRID_NUM','PropLand')
                    bbb$SD = substr(bbb$PORT,2,3)
                    com2grid = bbb
                    save(com2grid,file=file.path(fnODBC,'community.to.grid.contemporary.rdata'))
                 }
          load(file.path(fnODBC,'community.to.grid.contemporary.rdata'))
          return(com2grid)
}


 if(DS %in% c('annual.landings','annual.landings.redo')) {
          if(DS == 'annual.landings') {
                load(file=file.path(fnODBC,'annual.landings.rdata'))
                return(annual.landings)
                  }
        
                  con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  annual.landings = sqlQuery(con,'select * from LOBSTER.SLIP_LAND_ANNUAL')
                  print('Last two years of landings data may be incomplete, make sure to check with Cheryl.Denton@dfo-mpo.gc.ca on last update')
                  print('LFA27 for >2015 does not have Gulf landings yet.....')
                  save(annual.landings,file=file.path(fnODBC,'annual.landings.rdata'))
            }

if(DS %in% c('seasonal.landings','seasonal.landings.redo')) {
          if(DS == 'seasonal.landings') {
                load(file=file.path(fnODBC,'seasonal.landings.rdata'))
                return(seasonal.landings)
                  }
        
                  con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  seasonal.landings = sqlQuery(con,'select * from LOBSTER.SLIP_LAND_SEASONAL')
                  print('Last two years of landings data may be incomplete, make sure to check with Cheryl.Denton@dfo-mpo.gc.ca on last update')
                  print('LFA27 for >2015 does not have Gulf landings yet.....')
                  save(seasonal.landings,file=file.path(fnODBC,'seasonal.landings.rdata'))
            }


if(DS %in% c('historical.landings','historical.landings.redo')) {
          if(DS == 'historical.landings') {
                load(file=file.path(fnODBC,'historical.landings.rdata'))
                return(historical.landings)
                  }
        
                historical.landings = read.delim(file.path(project.datadirectory('bio.lobster'),"data","inputs","LFA34_Landings_1892-2004.txt"))

                  #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  #historical.landings = sqlQuery(con,'select * from LOBSTER.SLIP_LAND_HISTORICAL')
                  save(historical.landings,file=file.path(fnODBC,'historical.landings.rdata'))
            }


if(DS %in% c('season.dates','season.dates.redo')) {
          if(DS == 'season.dates') {
                load(file=file.path(fnODBC,'season.dates.rdata'))
                return(season.dates)
                  }
                  con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  print('This is not updated--Check with AMC')
                
              #using dats from landings by port redo AMC Dec 1 2017
              #  a = aggregate(mns~SYEAR+LFA+SDATE,data=dats,FUN=length)
              #  dd = as.data.frame(unique(cbind(a$LFA,a$SYEAR)))
              #  names(dd) = c('LFA','SYEAR')
              #  outs=list()
              #          for(j  in 1:nrow(dd)){
              #                      d2 = subset(a,LFA == dd[j,'LFA'] & SYEAR==dd[j,'SYEAR'])
              #                      x = d2$mns
              #  i1=15
              #      if(dd[j,'LFA'] %in% c(28:30)) i1 = 3

              #                      i = ave(x, FUN = function(x) cumsum(x >= i1 & with(rle(x >= i1), rep(lengths, lengths)) >= 3)) 
              #                      ii = c(which(i>0)[1],which.max(i))
              #                      outs[[j]] = cbind(d2[ii[1],],d2[ii[2],'SDATE'])
              #                      }
              #    at = as.data.frame(do.call(rbind,outs))
              #    names(at) = c('SYEAR','LFA','START_DATE','nn','END_DATE')
              #    at$nn = NULL
              #    season.dates=at
              #         save(season.dates,file=file.path(fnODBC,'season.dates.rdata'))

                    Fish.Date = lobster.db('season.dates')
                    season.dates = backFillSeasonDates(Fish.Date,eyr=year(Sys.time())-1)
              



                  #season.dates = sqlQuery(con,'select * from LOBSTER.FISHING_SEASONS')
                  save(season.dates,file=file.path(fnODBC,'season.dates.rdata'))
            }



### Inshore Commercial Logs and slips
if (DS %in% c("logs.redo", "logs") ) {

           if (DS=="logs.redo") {
              require(RODBC)
             con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
              
              # logs
               logs = sqlQuery(con, "select * from marfissci.lobster_sd_log")
              save( logs, file=file.path( fnODBC, "logs.rdata"), compress=T)
             
              # slips
              slips = sqlQuery(con, "select * from marfissci.lobster_sd_slip")
              save( slips, file=file.path( fnODBC, "slip.rdata"), compress=T)
              gc()  # garbage collection
              odbcClose(con)
            }
            load (file.path( fnODBC, "slip.rdata"), .GlobalEnv)
            load (file.path( fnODBC, "logs.rdata"), .GlobalEnv)
            print("Two files loaded called 'slips' and 'logs'" )
            
          }

if(DS %in% c('process.logs','process.logs.unfiltered', 'process.logs.redo')) {
                            if(DS == 'process.logs') {
                                  load(file=file.path(fnProducts,'logsInSeason.rdata'))
                                  return(logsInSeason)
                                }
                            if(DS == 'process.logs.unfiltered') {
                                  load(file=file.path(fnProducts,'logsInSeasonUnfiltered.rdata'))
                                  return(logsInSeason)
                                }


                    #Filtering by   
                    #Fish.Date = read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FishingSeasonDates.csv"))
                    Fish.Date = lobster.db('season.dates')
                    Fish.Date = backFillSeasonDates(Fish.Date,eyr=year(Sys.time()))
                    lfa  =  sort(unique(Fish.Date$LFA))
                    
                
                          print('Note the ODBC season Dates Need to be Updated AMC jan2017')
                          
                          #lfa "27"  "28"  "29"  "30"  "31A" "31B" "32"  "33"  "34"  "35"  "36"  "38" 

                          max_trap = c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
                          #max_lbs = c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
                          Fish.Date$START_DATE = as.Date(Fish.Date$START_DATE)#,"%d/%m/%Y")
                          Fish.Date$END_DATE = as.Date(Fish.Date$END_DATE)#,"%d/%m/%Y")


                    # imported logs from marfis
                          lobster.db('logs')
                          logs$TOTAL_NUM_TRAPS = rowSums(logs[c('NUM_OF_TRAPS','NUM_OF_TRAPS_B','NUM_OF_TRAPS_C')],na.rm=T)
                          logs$TOTAL_WEIGHT_LBS = rowSums(logs[c('WEIGHT_LBS','WEIGHT_LBS_B','WEIGHT_LBS_C')],na.rm=T)
                          logs$TOTAL_WEIGHT_KG = logs$TOTAL_WEIGHT_LBS*0.4536

                    # select for records within season
                          logs$DATE_FISHED = as.Date(logs$DATE_FISHED,"%Y-%m-%d")
                          #logs$SYEAR = year(logs$DATE_FISHED)
           
                        for(i in 1:length(lfa)) {
                                h  =  Fish.Date[Fish.Date$LFA==lfa[i],]  
                            for(j in 1:nrow(h)) {
                                logs$SYEAR[logs$LFA==lfa[i]&logs$DATE_FISHED>=h[j,'START_DATE']&logs$DATE_FISHED<=h[j,'END_DATE']] = h[j,'SYEAR']
                                }
                              }
                        
                        logs = subset(logs,!is.na(SYEAR))
                   
                    # add week of season (WOS) variable
                        logs$WOS = NA
                          
                            for(i in 1:length(lfa)) {
                                  h  =  Fish.Date[Fish.Date$LFA==lfa[i],]  
                               for(j in unique(logs$SYEAR[logs$LFA==lfa[i]])){
                                   logs$WOS[logs$LFA==lfa[i]&logs$SYEAR==j] = floor(as.numeric(logs$DATE_FISHED[logs$LFA==lfa[i]&logs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                              }

                    # add quarter
                      logs$quarter = NA
                      logs$quarter[month(logs$DATE_FISHED)%in%1:3] = 1
                      logs$quarter[month(logs$DATE_FISHED)%in%4:6] = 2
                      logs$quarter[month(logs$DATE_FISHED)%in%7:9] = 3
                      logs$quarter[month(logs$DATE_FISHED)%in%10:12] = 4


                    commonCols = c("SUM_DOC_ID", "VR_NUMBER", "VESSEL_NAME", "SUBMITTER_NAME", "LICENCE_ID", "LFA", "COMMUNITY_CODE","SD_LOG_ID", "DATE_FISHED","SYEAR","WOS",'quarter',"TOTAL_NUM_TRAPS","TOTAL_WEIGHT_KG")

                    logsInSeasonA = subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS),c(commonCols,"GRID_NUM", "WEIGHT_LBS", "NUM_OF_TRAPS"))
                    logsInSeasonB = subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_B)&!is.na(NUM_OF_TRAPS_B),c(commonCols,"GRID_NUM_B", "WEIGHT_LBS_B", "NUM_OF_TRAPS_B"))
                    logsInSeasonC = subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_C)&!is.na(NUM_OF_TRAPS_C),c(commonCols,"GRID_NUM_C", "WEIGHT_LBS_C", "NUM_OF_TRAPS_C"))

                    names(logsInSeasonB) = names(logsInSeasonA)
                    names(logsInSeasonC) = names(logsInSeasonA)

                    logsInSeason = rbind(logsInSeasonA,logsInSeasonB,logsInSeasonC)
                    logsInSeason$WEIGHT_KG = logsInSeason$WEIGHT_LBS*0.4536

                     
                    logsInSeason$CPUE = logsInSeason$WEIGHT_KG/logsInSeason$NUM_OF_TRAPS
        
                    

                    # add BUMPUP column: total landings/sum of logs for each year  & LFA
                    bumpup=T
                    if(bumpup){
                      seasonLandings = lobster.db('seasonal.landings')
                      annualLandings = lobster.db('annual.landings')
                      sl=reshape(seasonLandings,idvar="SYEAR",times=substr(names(seasonLandings)[-1],4,6),timevar="LFA",varying=list(names(seasonLandings)[-1]),direction='long')
                      sl$SYEAR=substr(sl$SYEAR,6,9)
                      names(sl)=c("SYEAR","LFA","C")
                      al=reshape(annualLandings,idvar="YR",times=substr(names(annualLandings)[-1],4,6),timevar="LFA",varying=list(names(annualLandings)[-1]),direction='long')
                      names(al)=c("SYEAR","LFA","C")
                      TotalLandings=rbind(subset(al,SYEAR>2000&!LFA%in%unique(sl$LFA)),subset(sl,SYEAR>2000))
                      logsInSeason$BUMPUP = NA
                      for(i in 1:length(lfa)){
                        tmplogs = subset(logsInSeason,LFA==lfa[i])
                        yrs = sort(unique(tmplogs$SYEAR))
                        for(y in 1:length(yrs)){
                          logsInSeason$BUMPUP[logsInSeason$SYEAR==yrs[y]&logsInSeason$LFA==lfa[i]] = TotalLandings$C[TotalLandings$SYEAR==yrs[y]&TotalLandings$LFA==lfa[i]]*1000/sum(tmplogs$WEIGHT_KG[tmplogs$SYEAR==yrs[y]],na.rm=T)
                        }
                      }
                    }
                     save(logsInSeason,file=file.path( fnProducts,"logsInSeasonUnfiltered.rdata"),row.names=F)

                    

                    # filter by max trap
                    if(length(max_trap)==length(lfa)){   #these do not match Jan 31, 2018 this code was added but not checked LFA 38 gets dropped
                    logsInSeason.lst = list()
                    for(i in 1:length(lfa)){
                      logsInSeason.lst[[i]] = subset(logsInSeason,LFA==lfa[i]&TOTAL_NUM_TRAPS<max_trap[i])
                    }
                    logsInSeason = do.call("rbind",logsInSeason.lst)
                    }
                    # filter by cpue
                    logsInSeason = subset(logsInSeason,CPUE<20 & !is.na(CPUE))
                    

                    # filter by grid
                    centgrid = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","lfa27_38_centgrid.csv"))
                    grid.key = with(centgrid,paste(LFA,GRID_NUM,sep='.'))
                    logsInSeason = subset(logsInSeason,!is.na(GRID_NUM)&paste(LFA,GRID_NUM,sep='.')%in%grid.key)

                    logsInSeason = assignSubArea2733(logsInSeason)
                    
  
          # Save logsInSeason as working data
              save(logsInSeason,file=file.path( fnProducts,"logsInSeason.rdata"),row.names=F)
   }

### Offshore Commercial Logs
    if (DS %in% c("logs41.redo", "logs41") ) {

             if (DS=="logs41.redo") {
                require(RODBC)
                con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                
                # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
                query41 = "select * from lobster.logs41"
                slipquery41 = "select  * from lobster.slips41"
                ziffquery41  =  "select * from lobster.ziff41"
                offquery41  =  "select * from lobster.crislog41;" # table not view

                slip41 = sqlQuery(con, slipquery41)
                logs41 = sqlQuery(con, query41)
                ziff41 = sqlQuery(con, ziffquery41)
                off41 = sqlQuery(con, offquery41)
            
                off41 = subset(off41,DATE_FISHED < '1995-01-01')

                logs41$DDLON = logs41$DDLON*-1
                save( logs41, file=file.path( fnODBC, "logs41.rdata"), compress=T)
                save( slip41, file=file.path( fnODBC, "slip41.rdata"), compress=T)
                save( ziff41, file=file.path( fnODBC, "ziff41.rdata"), compress=T)
                save( off41, file=file.path( fnODBC, "off41.rdata"), compress=T)
                gc()  # garbage collection
                odbcClose(con)
              }
              load (file.path( fnODBC, "logs41.rdata"), .GlobalEnv)
              load (file.path( fnODBC, "slip41.rdata"), .GlobalEnv)
              load (file.path( fnODBC, "ziff41.rdata"), .GlobalEnv)
              load (file.path( fnODBC, "off41.rdata"), .GlobalEnv)
              print("Objects are called 'logs41', 'slip41', 'ziff41', 'off41'")
              
      
    }
    if(DS %in% c('logs41.habitat','logs41.habitat.redo')) {
        
        if(DS == 'logs41.habitat' ) {
              load(file=file.path(fnProducts,'lfa41LogsHabitatData.rdata'))
              return(a41)
            }
              p = bio.lobster::load.environment()
              require(bio.utilities)
              require(bio.habitat)
              require(raster)
              loadfunctions('bio.habitat')
              loadfunctions('bio.utilities')
              loadfunctions('bio.indicators')
            loadfunctions('bio.temperature')

            lobster.db('logs41') #make sure to do a database recapture through logs41.redo before moving on

            logs41 = rename.df(logs41,c('FV_FISHED_DATETIME'),c('DATE_FISHED'))

            logs41$yr = year(logs41$DATE_FISHED) #2002 to present
            ziff41$yr = year(ziff41$DATE_FISHED) #1995 to 2001
            ziff41$DDLON = ziff41$DDLON * -1
            off41$yr  = year(off41$DATE_FISHED) #1981 to 1994

            logs41$OFFAREA = NULL 

            #oct16-oct15 fishing year until 2005 switch to Jan 1 to Dec 31

            a41 = rbind(off41,ziff41,logs41)
            a41$fishingYear = sapply(a41$DATE_FISHED,offFishingYear)

            a41 = lonlat2planar(a41,input_names = c('DDLON','DDLAT'),proj.type = p$internal.projection)
            a41$plon = grid.internal(a41$plon,p$plons)
            a41$plat = grid.internal(a41$plat,p$plats)
            a41$z = NA
            a41$depth = NULL
             
            a41 = completeFun(a41,c('plon','plat'))
            a41 = subset(a41,DDLAT>0)
            a41 = subset(a41, MON_DOC_ID!=153219950609)
            a41 = habitat.lookup(a41,p=p,DS='depth')

            #clean up some errors
            a41$z[which(a41$z>450)]  =  NA

            hist(a41$z,'fd',xlab='Depth',main="")

            #time stamping for seasonal temperatures

            a41$timestamp = as.POSIXct(a41$DATE_FISHED,tz='America/Halifax',origin=lubridate::origin)
            a41$timestamp = with_tz(a41$timestamp,"UTC")
            a41$dyear = lubridate::decimal_date(a41$timestamp)- lubridate::year(a41$timestamp)
            a41 = subset(a41,fishingYear<2016)

            a41 = habitat.lookup(a41,p=p,DS='temperature.seasonal')
            a41 = habitat.lookup(a41,p=p,DS='substrate')
          save(a41,file=file.path(fnProducts,'lfa41LogsHabitatData.rdata'))
          return(a41)
    }

### Offshore Commercial Logs for Jonah crab
   if (DS %in% c("logs41jonah.redo", "logs41jonah") ) {

           if (DS=="logs41jonah.redo") {
              require(RODBC)
              con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
              
              # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
              query41 = 'NEED TO IDENITFY'
               
              logs41jonah = sqlQuery(con, query41)
              logs41jonah$DDLON = logs41jonah$DDLON*-1
              save( logs41jonah, file=file.path( fnODBC, "logs41jonah.rdata"), compress=T)
              gc()  # garbage collection
              odbcClose(con)
            }
            load (file.path( fnODBC, "logs41jonah.rdata"), .GlobalEnv)
            
    }

### Offshore Observer
    if (DS %in% c("observer41.redo", "observer41") ) {

        if (DS=="observer41.redo") {
                require(RODBC)
                con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                
                # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
                observer41 = sqlQuery(con, 'select * from lobster.observer41') #pulling from a materialized view
                save( observer41, file=file.path( fnODBC, "observer41.rdata"), compress=T)
                gc()  # garbage collection
                odbcClose(con)
              }
              load (file.path( fnODBC, "observer41.rdata"), .GlobalEnv)
      
    }


#vms data
if(DS %in% c('lfa41.vms', 'lfa41.vms.redo')) {
      if(DS == 'lfa41.vms.redo') {
           require(RODBC)
           con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
  #Define a list of VRNs from offshore lobster vrns


      vms.q  =  paste("SELECT rownum vesid,
                  p.longitude lon, p.latitude lat, 
                 NVL(v.vessel_name,p.vr_number) vessel_name, 
                 p.vr_number vrn,
                 to_char(p.POSITION_UTC_DATE, 'YYYY/MM/DD HH24:MI:SS') vmsdate,
                 p.speed_knots 
                 FROM mfd_obfmi.vms_all p, mfd_obfmi.marfis_vessels_syn v
                 WHERE p.VR_NUMBER = v.vr_number(+)  
                 AND p.vr_number IN ('",vrn.vector,"')",
                  sep="" )

      vms.data  =  sqlQuery(con, vms.q, believeNRows=FALSE)  
      odbcClose(con)
        vms.data$VMSDATE  =  as.POSIXct(vms.data$VMSDATE,tz="GMT")  # VMS data is in UTC, assign timezone
  
  # Create date and time variables in local time
      vms.data$DATE  =  format(strftime(vms.data$VMSDATE,format="%Y-%m-%d"), tz="America/Halifax",usetz=TRUE)
      vms.data$TIME  =  format(strftime(vms.data$VMSDATE,format="%H:%M:%S"), tz="America/Halifax",usetz=TRUE)
      vms.data$YEAR  =  format(strftime(vms.data$VMSDATE,format="%Y"), tz="America/Halifax",usetz=TRUE)
      vms.data$VMSDATElocal  =  as.POSIXct(paste(vms.data$DATE, vms.data$TIME), format="%Y-%m-%d %H:%M:%S",tz="America/Halifax")

      save(vms.data,file=file.path( fnODBC,"vms.data.rdata"))
      return(paste('File is saved as', file.path( fnODBC,"vms.data.rdata"),sep=" "))
           }
    
      load(file.path( fnODBC, "vms.data.rdata" ))
      return(vms.data)
    }


### At Sea sampling from Cheryl's view


    if (DS %in% c("atSea.redo", "atSea") ) {

         if (DS=="atSea.redo") {
           require(RODBC)
           con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
            
            # atSea
            atSea = sqlQuery(con, "select * from lobster.LOBSTER_ATSEA_VW")
            save( atSea, file=file.path( fnODBC, "atSea.rdata"), compress=T)
            gc()  # garbage collection
            odbcClose(con)
          }
          load(file.path( fnODBC, "atSea.rdata"), .GlobalEnv)
     }

     if(DS %in% c('atSea.CatchLevel.redo','atSea.CatchLevel')){
           require(RODBC)
           con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        if(DS == 'atSea.CatchLevel.redo') {    
            # atSea
            atSeaCatchLevel = sqlQuery(con, "select * from lobster.atseacatchlevel;")
            save( atSeaCatchLevel, file=file.path( fnODBC, "atSeaCatchLevel.rdata"), compress=T)
            gc()  # garbage collection
            odbcClose(con)
          }
          load(file.path( fnODBC, "atSeaCatchLevel.rdata"), .GlobalEnv)
     }

    if (DS %in% c("atSea.clean.redo", "atSea.clean") ) {

          fname = 'atSea.clean.rdata'
        if(DS == 'atSea.clean') {
                load(file.path( fnODBC, fname))
                return(atSea.clean)
        }
        
         if (DS=="atSea.clean.redo") {
             lobster.db('atSea')
             aS = atSea
             aS = addSYEAR(aS)
             ih = which(is.na(aS$SYEAR))
             aS$SDATE = aS$STARTDATE

             aS$GRIDNO[which(aS$GRIDNO== -99)] <- NA
            LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
            aS = makePBS(aS,polygon=F)
            a = which(is.na(aS$Y) | is.na(aS$X))
            if(length(a)<dim(aS)[1]){
            if(length(a)>0) {
              a1 = findPolys(aS[-a,],LFAgrid,maxRows = 3e6,includeBdry=1)
              }else{
                a1 = findPolys(aS,LFAgrid,maxRows = 3e6,includeBdry=1)
              }
            }
            aS = merge(aS,a1,by='EID',all.x=T)
            i = which(is.na(aS$GRIDNO) & !is.na(aS$PID))
            aS$GRIDNO[i] <- aS$SID[i]
              i = which(is.na(aS$GRIDNO))
              aS$GRIDNO[i] <- 0
              i = which(is.na(aS$VNOTCH) & aS$SPECIESCODE==2550)
              aS$VNOTCH[i] <- 0
              i = which(is.na(aS$CULL) & aS$SPECIESCODE==2550)
              aS$CULL[i] <- 0
              i = which(aS$CARLENGTH>280 & aS$SPECIESCODE==2550)
              aS$CARLENGTH[i] <- NA
              aS$PID = aS$SID = aS$Bdry = NULL
         

                      season.dates = lobster.db('season.dates')
                       aS$WOS = NA
                        lfa = unique(aS$LFA) 
                            for(i in 1:length(lfa)) {
                                  h  = season.dates[season.dates$LFA==lfa[i],]  
                                  k = na.omit(unique(aS$SYEAR[aS$LFA==lfa[i]]))
                                  h = na.omit(h)
                                  k = intersect(k,h$SYEAR)
                               for(j in k){
                                   aS$WOS[aS$LFA==lfa[i] & aS$SYEAR==j] = floor(as.numeric(aS$SDATE[aS$LFA==lfa[i] & aS$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                          }
                          if(any(!is.finite(aS$WOS))) {kl = which(!is.finite(aS$WOS)); aS$WOS[kl] = NA}
                          aS = subset(aS,WOS>0)

           atSea.clean = aS

          save( atSea.clean, file=file.path( fnODBC, fname), compress=T)
          
          }
     }



### port sampling 
    if (DS %in% c("port.sampling.redo", "port.sampling") ) {

     if (DS=="port.sampling.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # port
        port = sqlQuery(con, "select a.SAMPLE_SEQ,a.SAMPLE_NO,a.SDATE,a.SEASON,a.NTRAPS,a.LATITUDE,a.LONGITUDE,a.GRADE, b.L_SIZE,b.N_MALES,b.N_FEM,b.NBF, d.LFA,c.PORT,c.COUNTY,c.STAT,c.PORT_CODE,c.LATITUDE port_lat,c.LONGITUDE port_lon from lobster.CRLENGCODE a, lobster.CRLENGFREQ b, lobster.CRLOCATIONS c, frailc.lfa_port d where a.sample_seq = b.sample_seq and a.port = c.port and c.PORT_CODE = d.port(+) and a.type = 'P' ")
        port = addSYEAR(port)
        season.dates = lobster.db('season.dates')
         m=0
         port = subset(port,LFA %in% c('27','28','29','30','31A','31B','32','33'))
         lfa = as.character(na.omit(unique(port$LFA) ))
        port$WOS = NA
                            for(i in 1:length(lfa)) {
                                  h  = season.dates[season.dates$LFA==lfa[i],]  
                                  if(lfa[i] == '31A') h  = as.data.frame(rbind(season.dates[season.dates$LFA=='31_32',],season.dates[season.dates$LFA=='31A',] )) 
                                  if(lfa[i] == '31B') h  = as.data.frame(rbind(season.dates[season.dates$LFA=='31_32',],season.dates[season.dates$LFA=='31B',] )) 
                                  rr = 1980:2016
    
                                  if(length(rr) != nrow(h)){
                                      rr=data.frame(SYEAR=rr)
                                      h = merge(rr,h,all.x=T)
                                      tr = which(is.na(h$START_DATE))
                                      if(length(tr)>0){
                                      if(any(tr==1)) {
                                                   trr = min(which(!is.na(h$START_DATE)))
                                            for(up in trr:1){
                                                    h[(up-1),c('START_DATE','END_DATE')] <- h[up,c('START_DATE','END_DATE')] - 365                                                  
                                            }
                                      }
                                    }
                                   tr = which(is.na(h$START_DATE))
                                     if(length(tr)>0){
                                   for(gg in 1:length(tr)){
                                          h[tr[gg],c('START_DATE','END_DATE')] <- h[(tr[gg]-1),c('START_DATE','END_DATE')] + 365  
                                          }
                                        }
                                      }
                                  k = as.numeric(na.omit(unique(port$SYEAR[port$LFA==lfa[i]])))
                                  if(any(k<1980)) k = subset(k,k>=1980)
                               for(j in k){
                                m=m+1
                                print(m)
                                   port[which(port$LFA==lfa[i] & port$SYEAR==j),'WOS'] = floor(as.numeric(port[which(port$LFA==lfa[i] & port$SYEAR==j),'SDATE']-h$START_DATE[h$SYEAR==j])/7)+1
                                }
                          }
                          if(any(!is.finite(port$WOS))) {kl = which(!is.finite(port$WOS)); port$WOS[kl] = NA}
        save( port, file=file.path( fnODBC, "port.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fnODBC, "port.rdata"), .GlobalEnv)
     }
  
### voluntary logs 
    if (DS %in% c("vlog.redo", "vlog") ) {

     if (DS=="vlog.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # vlog
        vlog = sqlQuery(con, "select a.FDATE,a.N_TRP,a.W_TOT,a.FCODE,a.N_L,a.W_AVG,a.PORT,a.CPTH,a.NBF,a.SEASON,a.W_C,a.CPTH_C, b.LFA,b.COUNTY,b.STAT,b.PORT_CODE,b.LATITUDE,b.LONGITUDE,b.COMMENTS from lobster.CRLOGDATA a, lobster.CRLOCATIONS b where a.port = b.port")
        save( vlog, file=file.path( fnODBC, "vlog.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fnODBC, "vlog.rdata"), .GlobalEnv)
     }


    if (DS %in% c("process.vlog.redo", "process.vlog") ) {

     if (DS=="process.vlog.redo") {
          load(file.path( fnODBC, "vlog.rdata"), .GlobalEnv)  
          vlog = addSYEAR(vlog,date.field="FDATE")
          vlog$SYEAR = as.numeric(substr(vlog$SEASON,6,9))
          vlog$W_KG = vlog$W_TOT*0.4536
          vlog$CPUE = vlog$W_KG/vlog$N_TRP

          vlog$X = convert.dd.dddd(vlog$LONGITUDE)*-1
          vlog$Y = convert.dd.dddd(vlog$LATITUDE)

          Ports = read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","Ports.csv"))
          ports31A = subset(Ports,LFA=='31A')$Port_Code
          ports31B = c(subset(Ports,LFA=='31B')$Port_Code,11799)
          stat33E = c(18,22,23,25,26)
          stat33W = c(27,28,30,31)
          stat27N = c(1,4)
          stat27S = c(6,7)
          vlog$LFA[vlog$STAT%in%stat27N] = "27N"
          vlog$LFA[vlog$STAT%in%stat27S] = "27S"
          vlog$LFA[vlog$STAT%in%stat33E] = "33E"
          vlog$LFA[vlog$STAT%in%stat33W] = "33W"
          vlog$LFA[vlog$PORT_CODE%in%ports31A] = "31A"
          vlog$LFA[vlog$PORT_CODE%in%ports31B] = "31B"
          save( vlog, file=file.path( fnODBC, "processed.vlog.rdata"), compress=T)
          return(vlog)
        }
        load(file.path( fnODBC, "processed.vlog.rdata"),.GlobalEnv)      
    }





### CRIS database
    if (DS %in% c("cris.redo", "cris") ) {

     if (DS=="cris.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # cris
        cris.trips = sqlQuery(con, "select * from cris.crtrips")
        save( cris.trips, file=file.path( fnODBC, "crisTrips.rdata"), compress=T)
        cris.traps = sqlQuery(con, "select * from cris.crtraps")
        save( cris.traps, file=file.path( fnODBC, "crisTraps.rdata"), compress=T)
        cris.samples = sqlQuery(con, "select * from cris.crsamples")
        save( cris.samples, file=file.path( fnODBC, "crisSamples.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fnODBC, "crisTrips.rdata"), .GlobalEnv)       
      load(file.path( fnODBC, "crisTraps.rdata"), .GlobalEnv)       
      load(file.path( fnODBC, "crisSamples.rdata"), .GlobalEnv)

      fdd = file.path(project.datadirectory('bio.lobster'),'data','CRIScodetables')
      h = dir(fdd)
      code.tables = list()
        for(i in h){
          code.tables[[i]]  =  read.csv(file.path(fdd,i))
        }
      return(code.tables)       
     }

###Observer Length Frequencies

if(DS %in% c('lfa41.observer.samples.redo','lfa41.observer.samples')) {
   if (DS=="lfa41.observer.samples.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # Denton Script Sept 28 2016
        obs.samp  =  sqlQuery(con, paste("
SELECT trip.trip_id,late, lone, sexcd_id,fish_length,st.nafarea_id,board_date, st.fishset_id
                                        FROM isdb.istrips trip, isdb.isfishsets st,   isdb.iscatches ca, isdb.isfish fish,
                                                                          (SELECT 
                                            fishset_id, 
                                        (CASE 
                                            WHEN pntcd_lat_3 is not null and pntcd_lon_3 is not null
                                            THEN pntcd_lat_3
                                            ELSE 
                                              (CASE 
                                               WHEN pntcd_lat_4 is not null and pntcd_lon_4 is not null
                                               THEN pntcd_lat_4
                                               ELSE 
                                                  (CASE 
                                                   WHEN pntcd_lat_1 is not null and pntcd_lon_1 is not null
                                                   THEN pntcd_lat_1
                                                   ELSE 
                                                      (CASE 
                                                       WHEN pntcd_lat_2 is not null and pntcd_lon_2 is not null
                                                       THEN pntcd_lat_2
                                                       ELSE 
                                                        NULL
                                                       END)
                                                   END)
                                               END)
                                            END) late,
                                        (CASE 
                                            WHEN pntcd_lat_3 is not null and pntcd_lon_3 is not null
                                            THEN pntcd_lon_3
                                            ELSE 
                                              (CASE 
                                               WHEN pntcd_lat_4 is not null and pntcd_lon_4 is not null
                                               THEN pntcd_lon_4
                                               ELSE 
                                                  (CASE 
                                                   WHEN pntcd_lat_1 is not null and pntcd_lon_1 is not null
                                                   THEN pntcd_lon_1
                                                   ELSE 
                                                      (CASE 
                                                       WHEN pntcd_lat_2 is not null and pntcd_lon_2 is not null
                                                       THEN pntcd_lon_2
                                                       ELSE 
                                                        NULL
                                                       END)
                                                   END)
                                               END)
                                            END) lone
                                        FROM (
                                        SELECT
                                          a.fishset_id, 
                                         sum(case b.pntcd_id when 1 then latitude else null end ) pntcd_lat_1,
                                         sum(case b.pntcd_id when 1 then longitude else null end ) pntcd_lon_1,
                                         sum(case b.pntcd_id when 2 then latitude else null end ) pntcd_lat_2,
                                         sum(case b.pntcd_id when 2 then longitude else null end ) pntcd_lon_2,
                                         sum(case b.pntcd_id when 3 then latitude else null end ) pntcd_lat_3,
                                         sum(case b.pntcd_id when 3 then longitude else null end ) pntcd_lon_3,
                                         sum(case b.pntcd_id when 4 then latitude else null end ) pntcd_lat_4,
                                         sum(case b.pntcd_id when 4 then longitude else null end ) pntcd_lon_4
                                        FROM observer.isfishsets a, observer.issetprofile b
                                        where a.fishset_id = b.fishset_id(+)
                                        group by a.fishset_id
                                        order by a.fishset_id
                                        ) 
                                        ) ep
                                                                        WHERE trip.tripcd_id = 2550
                                                                        AND comarea_id       ='L41'
                                                                        AND (trip.trip_id    = st.trip_Id)
                                                                        AND st.fishset_id    = ca.fishset_id(+)
                                                                        AND st.fishset_id    = ep.fishset_id(+)
                                                                        AND ca.speccd_id(+)  = 2550
                                                                        AND ca.catch_id      = fish.catch_id(+)
                                                                        AND fish_length     IS NOT NULL

;"))

        save( obs.samp, file=file.path( fnODBC, "lfa41.observer.samples.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }

      load(file=file.path( fnODBC, "lfa41.observer.samples.rdata"),.GlobalEnv)

    }

### FSRS traps 
    if (DS %in% c("fsrs.redo", "fsrs") ) {

     if (DS=="fsrs.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # fsrs
        fsrs = sqlQuery(con, "select * from fsrs_lobster.FSRS_LOBSTER_VW") #the sizes are all recoded to be continuous --- the old guage is now reflected in the new numbering AMC
        save( fsrs, file=file.path( fnODBC, "fsrs.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fnODBC, "fsrs.rdata"), .GlobalEnv)
     }


    if (DS %in% c("fsrs.commercial.samples.redo", "fsrs.commercial.samples") ) {
                fname = 'fsrs.commercial.rdata'
               
               if (DS=="fsrs.commercial.samples.redo") {
                            print('Get updated csv files from FSRS last Update Nov 2017 AMC')
                            tr = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','fsrs.commercial.samples','CTS_Position_17.csv'),header=T)
                            vc = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','fsrs.commercial.samples','Vessel_Code.csv'),header=T)
                            ti = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inputs','fsrs.commercial.samples','Trapss.csv'),header=T)
                            
                            tr$Y = tr$Trap.1.Latitude
                            tr$X = tr$Trap.1.Longitude

                            tr = tr[,-c(grep('Lati',names(tr)))]
                            tr = tr[,-c(grep('Longi',names(tr)))]
                            tr$X = convert.dd.dddd(tr$X)*-1
                            tr$Y = convert.dd.dddd(tr$Y)
                            tr$Comments <- NULL
                            LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
                            
                            tr = makePBS(tr,polygon=F)
                            a = which(is.na(tr$Y) | is.na(tr$X))
                             if(length(a)<dim(tr)[1]){
                                if(length(a)>0) {
                                       a1 = findPolys(tr[-a,],LFAgrid,maxRows = 3e6,includeBdry=1)
                                    }else{
                                       a1 = findPolys(tr,LFAgrid,maxRows = 3e6,includeBdry=1)
                                }
                              }  
                            tr = merge(tr,a1,by='EID',all.x=T)
                            tr$PID = tr$Bdry <- NULL
                            tr = rename.df(tr,c('ID','SID'),c('TR.ID','GRID_NUM'))
                            tr$Temp = ifelse(tr$Temp==0,NA,tr$Temp)
                            tr$Temp = ifelse(tr$Temp==-99,NA,tr$Temp)
                            
                            tt = merge(ti,tr,by=c('Record.Number'))
                            tt = toNums(tt,c('Short','Berried','V.Notched','Recaptured'))
                            tt$Date = as.Date(tt$Date,format = '%d-%b-%y')
                            tt = subset(tt,LFA==33)
                            tt = addSYEAR(tt,'Date')
                            tt = subset(tt,!is.na(Date))
                            tt$WOS = NA
                            Fish.Date = lobster.db('season.dates')
                               h  =  Fish.Date[Fish.Date$LFA==33,]  
                               for(j in unique(tt$SYEAR)){
                                   tt$WOS[tt$SYEAR==j] = floor(as.numeric(tt$Date[tt$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                            tt = subset(tt,WOS>0)
                            fsrs.comm = tt
                            
                            save( fsrs.comm, file=file.path( fnODBC, fname), compress=T)
                            gc()  # garbage collection
                          }
                load(file.path( fnODBC, fname), .GlobalEnv)
      
    }



###CCIR data frames
  if(DS %in% c('ccir','ccir.redo')){

      if(DS=='ccir.redo'){
          lobster.db('fsrs')
          vars = c('RECORD_NUMBER','TRAP_NO','LOBSTER_NO','SEX','SIZE_CD','SHORT','VESSEL_CD','SOAK_DAYS','DEPTH','LFA','LATITUDE','LONGITUDE','TEMP','WIND_DIRECTION','WIND_SPEED','HAUL_DATE','HAUL_YEAR','LFA_GRID')
          fsrs = fsrs[,vars]
          fsrs = rename.df(fsrs,vars, c("Record.Number", "Trap.Number", "Lobster.Number", "Sex", "Size", "Short" , "Vessel.Code" ,"Soak.Days", "Depth", "LFA", "Latitude", "Longitude", 'Temperature',"Wind.Direction", "Wind.Speed" ,"DATE" , "YEAR",'Grid'))
          fsrs$indY = ifelse(month(fsrs$DATE)>8,1,0)
          fsrs$YEAR =  fsrs$YEAR + fsrs$indY #ending year required by CCIR program
          fsrs$indY = NULL
          fsrs$Berried = ifelse(fsrs$Sex == 3, 1, 0)
          fsrs$Sex = ifelse(fsrs$Sex == 3, 2, fsrs$Sex)
          fsrs$julian = round(as.numeric(julian(fsrs$DATE)))

          mls = read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","MinLegalSize.csv"))
          lfa = rep(unlist(lapply(strsplit(names(mls)[2:ncol(mls)],"LFA"),'[[',2)),each=nrow(mls))
          mls = reshape(mls,idvar='Year',varying=list(2:14),v.names=c('MLS'),direction='long')
          mls$lfa = lfa
          i = which(mls$lfa=='31a')
          mls$lfa[i] = '31.1'
          i = which(mls$lfa=='31b')
          mls$lfa[i] = '31.2'
          mls$lfa = as.numeric(mls$lfa)
          names(mls) = c('YEAR','ID','MLS','LFA')
          mls$MLS_FSRS  =  NA
          scd = read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FSRS_SIZE_CODES.csv"))
          for(i in 1:nrow(mls)) {
              a = mls[i,'MLS']
               mls$MLS_FSRS[i]= scd$SIZE_CD[intersect(which(scd$MIN_S<=a),which(scd$MAX_S>=a))]
            }

            fsrs = merge(fsrs,mls,by=c('LFA','YEAR'),all.x=T)

          # remove berried
          fsrs = fsrs[order(fsrs$YEAR),]
          fsrs = subset(fsrs,Berried==0)
          fsrs$IsLegal  =  1-fsrs$Short
          ccir_data = fsrs
          ccir_data$Y = convert.dd.dddd(c(ccir_data$Latitude))
          ccir_data$X = convert.dd.dddd(c(ccir_data$Longitude))
          ccir_data$DATE = format(ccir_data$DATE,'%Y-%m-%d')
          ccir_data$LFA = ifelse(ccir_data$LFA==31.1,'31a',ccir_data$LFA)
          ccir_data$LFA = ifelse(ccir_data$LFA==31.2,'31b',ccir_data$LFA)
         save( ccir_data, file=file.path( fnODBC, "ccir_data.rdata"), compress=T)
        gc()  # garbage collection
      }
      load(file.path( fnODBC, "ccir_data.rdata"), .GlobalEnv)
     }
        
       

### lobster catch from scallop survey  
    if (DS %in% c("scallop.redo", "scallop") ) {

     if (DS=="scallop.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # scallop
        scallop.catch = sqlQuery(con, "select * from SCALLSUR.SCBYCATCHES")
        save( scallop.catch, file=file.path( fnODBC, "scallopCatch.rdata"), compress=T)
        scallop.tows = sqlQuery(con, "select * from SCALLSUR.SCTOWS")
        save( scallop.tows, file=file.path( fnODBC, "scallopTows.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fnODBC, "scallopCatch.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "scallopTows.rdata"), .GlobalEnv)
    }

### lobster survey  
    if (DS %in% c("survey.redo", "survey") ) {

      if (DS=="survey.redo") {
        # survey
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        ILTS2016TowDepth = sqlQuery(con, "select * from FRAILC.MARPORT_DEPTH")
        ILTS2016TowSpread = sqlQuery(con, "select * from FRAILC.MARPORT_SPREAD")
        ILTS2016Tracks = sqlQuery(con, "select * from FRAILC.MARPORT_TRACKS")
        ILTSTemp = sqlQuery(con, "select * from FRAILC.MINILOG_TEMP")
        ILTS2016Tracks = ILTS2016Tracks[order(ILTS2016Tracks$TTIME),]
        #NM1 = merge(ILTSTowDepth,ILTSTowSpread) #merge net mensuration into one file
        #netMensuration = merge( NM1,ILTS2016Tracks)#merge net mensuration into one file
        #netMensuration$TTIME = NULL #remove load date from merged file
        surveyCatch = sqlQuery(con, "select * from lobster.ILTSSETS_MV")
        surveyMeasurements = sqlQuery(con, "select * from lobster.ILTSDETAILS_MV")
        fishMeasurements = sqlQuery(con, "select * from lobster.ILTSFISHLENGTHS_MV")
       
        
        with(surveyMeasurements,paste(TRIP_ID,SET_NO,sep=''))->surveyMeasurements$SET_ID
        with(surveyCatch,paste(TRIP_ID,SET_NO,sep=''))->surveyCatch$SET_ID
        surveyCatch$SET_LONG = surveyCatch$SET_LONG*-1
        surveyCatch$HAUL_LONG = surveyCatch$HAUL_LONG*-1
        surveyCatch$YEAR = year(surveyCatch$BOARD_DATE)
        surveyMeasurements$SET_LON = surveyMeasurements$SET_LON*-1
        surveyMeasurements$HAUL_LON = surveyMeasurements$HAUL_LON*-1
        
        if(unique(subset(surveyCatch,YEAR==2017,select=GEAR))[,1]=='280 BALLOON') {
        	j = which(surveyCatch$YEAR==2017)
        	surveyCatch$GEAR[j] = 'NEST'
        	j = which(surveyMeasurements$YEAR==2017)
        	surveyMeasurements$GEAR[j] = 'NEST'
        }

        surveyStationID = sqlQuery(con, "select * from LOBSTER.ILTS_SURVEY_STATION")
        save(list=c("ILTS2016TowDepth","ILTS2016TowSpread","ILTS2016Tracks") , file=file.path( fnODBC, "MarPort2016.rdata"), compress=T)
        save(surveyCatch, file=file.path( fnODBC, "surveyCatch.rdata"), compress=T)
        save(surveyMeasurements, file=file.path(fnODBC, "surveyMeasurements.rdata"), compress=T)
        save(fishMeasurements, file=file.path(fnODBC, "fishMeasurements.rdata"), compress=T)
        save(ILTSTemp, file=file.path(fnODBC, "ILTSTemp.rdata"), compress=T)
        save(surveyStationID, file=file.path(fnODBC, "surveyStationID.rdata"), compress=T)
        
        gc()  # garbage collection
      }
      load(file.path( fnODBC, "MarPort2016.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyCatch.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyMeasurements.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "fishMeasurements.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSTemp.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyStationID.rdata"), .GlobalEnv)
      
    }
### lobster sampling from rv survey  

if(DS %in% c('rv.survey.samples.redo','rv.survey.samples.samples')) {
   if (DS=="rv.survey.samples.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # Denton Script May 2017

        sql = "SELECT d.mission,
                d.setno,
                d.spec,
                d.size_class,
                d.specimen_id,
                d.flen,
                d.fwt,
                d.clen,
                MIN(DECODE(o.LV1_OBSERVATION,'Abdominal Width', o.data_value)) ab_width,
                MIN(DECODE(o.LV1_OBSERVATION,'Clutch Fullness Rate', o.data_value)) Clutch_full,
                MIN(DECODE(o.LV1_OBSERVATION,'Egg Stage', o.data_value)) Egg_st,
                MIN(DECODE(o.LV1_OBSERVATION,'Molt Stage', o.data_value)) Molt_stage,
                MIN(DECODE(o.LV1_OBSERVATION,'Spermatophore Presence', o.data_value)) sperm_plug,
                MIN(DECODE(o.LV1_OBSERVATION,'Shell Disease Index', o.data_value)) disease
              FROM GROUNDFISH.GSDET d,
                GROUNDFISH.GS_LV1_OBSERVATIONS o
              WHERE d.mission           = o.mission(+)
              AND d.setno               = o.setno(+)
              AND d.specimen_id         = o.SPECIMEN_ID(+)
              AND d.spec                = 2550
              AND SUBSTR(d.mission,4,4) = 2016
              GROUP BY d.mission,
                d.setno,
                d.spec,
                d.size_class,
                d.specimen_id,
                d.flen,
                d.fwt,
                d.clen"

        rv.samp  =  sqlQuery(con, sql)


        save( rv.samp, file=file.path( fnODBC, "rv.survey.samples.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }

      load(file=file.path( fnODBC, "rv.survey.samples.rdata"),.GlobalEnv)

    }


  }

