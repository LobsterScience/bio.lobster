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
        lobster.db( DS="logs.redo")
        lobster.db( DS="logs41.redo")
        lobster.db( DS="logs41jonah.redo")
        lobster.db( DS="observer41.redo")
        lobster.db( DS="atSea.redo")
        lobster.db( DS="cris.redo")
        lobster.db( DS="port.redo")
        lobster.db( DS="vlog.redo")
        lobster.db( DS="fsrs.redo")
        lobster.db( DS="scallop.redo")
        lobster.db( DS="survey.redo")
        lobster.db( DS="annual.landings.redo")
        lobster.db( DS="seasonal.landings.redo")
        lobster.db( DS="historical.landings.redo")
        lobster.db( DS="season.dates.redo")
        lobster.db(DS = "lfa41.vms.redo")
        lobster.db(DS= "logs41.habitat.redo")
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
                  season.dates = sqlQuery(con,'select * from LOBSTER.FISHING_SEASONS')
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

if(DS %in% c('process.logs', 'process.logs.redo')) {
                            if(DS == 'process.logs') {
                                  load(file=file.path(fnProducts,'logsInSeason.rdata'))
                                  print("Object is 'logsInseason'")
                                  return(logsInSeason)
                                }

                    #Filtering by   
                    Fish.Date<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","FishingSeasonDates.csv"))
                     
                
                          print('Not the ODBC season Dates Need to Have Updated AMC jan2017')
                          lfa <- unique(Fish.Date$LFA)
                          
                          max_trap<-c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
                          max_lbs<-c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
                          Fish.Date$START_DATE<-as.Date(Fish.Date$START_DATE,"%d/%m/%Y")
                          Fish.Date$END_DATE<-as.Date(Fish.Date$END_DATE,"%d/%m/%Y")


                    # imported logs from marfis
                          lobster.db('logs')
                          logs$TOTAL_NUM_TRAPS<-rowSums(logs[c('NUM_OF_TRAPS','NUM_OF_TRAPS_B','NUM_OF_TRAPS_C')],na.rm=T)
                          logs$TOTAL_WEIGHT_LBS<-rowSums(logs[c('WEIGHT_LBS','WEIGHT_LBS_B','WEIGHT_LBS_C')],na.rm=T)
                          logs$TOTAL_WEIGHT_KG<-logs$TOTAL_WEIGHT_LBS*0.4536

                    # select for records within season
                          logs$SYEAR<-NA
                          logs$DATE_FISHED<-as.Date(logs$DATE_FISHED,"%Y-%m-%d")
            
                        for(i in 1:length(lfa)) {
                                h <- Fish.Date[Fish.Date$LFA==lfa[i],]  
                            for(j in 1:nrow(h)) {
                                logs$SYEAR[logs$LFA==lfa[i]&logs$DATE_FISHED>=h[j,'START_DATE']&logs$DATE_FISHED<=h[j,'END_DATE']]<-h[j,'SYEAR']
                                }
                              }
                        
                        logs<-subset(logs,!is.na(SYEAR))
                    
                    # add week of season (WOS) variable
                      logs$WOS<-NA
                        
                          for(i in 1:length(lfa)) {
                                h <- Fish.Date[Fish.Date$LFA==lfa[i],]  
                             for(j in unique(logs$SYEAR[logs$LFA==lfa[i]])){
                                 logs$WOS[logs$LFA==lfa[i]&logs$SYEAR==j]<-floor(as.numeric(logs$DATE_FISHED[logs$LFA==lfa[i]&logs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                              }
                            }

                    # add quarter
                      logs$quarter<-NA
                      logs$quarter[month(logs$DATE_FISHED)%in%1:3]<-1
                      logs$quarter[month(logs$DATE_FISHED)%in%4:6]<-2
                      logs$quarter[month(logs$DATE_FISHED)%in%7:9]<-3
                      logs$quarter[month(logs$DATE_FISHED)%in%10:12]<-4


                    commonCols<-c("SUM_DOC_ID", "VR_NUMBER", "VESSEL_NAME", "SUBMITTER_NAME", "LICENCE_ID", "LFA", "COMMUNITY_CODE","SD_LOG_ID", "DATE_FISHED","SYEAR","WOS",'quarter',"TOTAL_NUM_TRAPS","TOTAL_WEIGHT_KG")

                    logsInSeasonA<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS)&!is.na(NUM_OF_TRAPS),c(commonCols,"GRID_NUM", "WEIGHT_LBS", "NUM_OF_TRAPS"))
                    logsInSeasonB<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_B)&!is.na(NUM_OF_TRAPS_B),c(commonCols,"GRID_NUM_B", "WEIGHT_LBS_B", "NUM_OF_TRAPS_B"))
                    logsInSeasonC<-subset(logs,!is.na(SYEAR)&!is.na(WEIGHT_LBS_C)&!is.na(NUM_OF_TRAPS_C),c(commonCols,"GRID_NUM_C", "WEIGHT_LBS_C", "NUM_OF_TRAPS_C"))

                    names(logsInSeasonB)<-names(logsInSeasonA)
                    names(logsInSeasonC)<-names(logsInSeasonA)

                    logsInSeason<-rbind(logsInSeasonA,logsInSeasonB,logsInSeasonC)
                    logsInSeason$WEIGHT_KG<-logsInSeason$WEIGHT_LBS*0.4536

                    centgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","lfa27_38_centgrid.csv"))
                    grid.key<-with(centgrid,paste(LFA,GRID_NUM,sep='.'))
                    
                    logsInSeason<-subset(logsInSeason,!is.na(GRID_NUM)&paste(LFA,GRID_NUM,sep='.')%in%grid.key)
                    logsInSeason$CPUE<-logsInSeason$WEIGHT_KG/logsInSeason$NUM_OF_TRAPS
                    logsInSeason<-subset(logsInSeason,CPUE<20)

                    subareas<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","LFA2733subarea.csv"))
                    names(subareas)[2]<-"GRID_NUM"
                    logsInSeason<-merge(logsInSeason,subareas,all.x=T)

                      
                    # add BUMPUP column: total landings/sum of logs for each year  & LFA
                    bumpup=T
                    if(bumpup){
                      seasonLandings<-lobster.db('seasonal.landings')
                      annualLandings<-lobster.db('annual.landings')
                      sl=reshape(seasonLandings,idvar="SYEAR",times=substr(names(seasonLandings)[-1],4,6),timevar="LFA",varying=list(names(seasonLandings)[-1]),direction='long')
                      sl$SYEAR=substr(sl$SYEAR,6,9)
                      names(sl)=c("SYEAR","LFA","C")
                      al=reshape(annualLandings,idvar="YR",times=substr(names(annualLandings)[-1],4,6),timevar="LFA",varying=list(names(annualLandings)[-1]),direction='long')
                      names(al)=c("SYEAR","LFA","C")
                      TotalLandings=rbind(subset(al,SYEAR>2000&!LFA%in%unique(sl$LFA)),subset(sl,SYEAR>2000))

                      logsInSeason$BUMPUP<-NA
                      lfa<-unique(TotalLandings$LFA)  
                      for(i in 1:length(lfa)){
                        tmplogs<-subset(logsInSeason,LFA==lfa[i])
                        yrs<-sort(unique(tmplogs$SYEAR))
                        for(y in 1:length(yrs)){
                          logsInSeason$BUMPUP[logsInSeason$SYEAR==yrs[y]&logsInSeason$LFA==lfa[i]]<-TotalLandings$C[TotalLandings$SYEAR==yrs[y]&TotalLandings$LFA==lfa[i]]*1000/sum(tmplogs$WEIGHT_KG[tmplogs$SYEAR==yrs[y]],na.rm=T)
                        }
                      }
                    }
          # Save logsInSeason as working data
              save(logsInSeason,file=file.path( fnProducts,"logsInSeason.rdata"),row.names=F)
   }

### Offshore Commercial Logs
    if (DS %in% c("logs41.redo", "logs41") ) {

             if (DS=="logs41.redo") {
                require(RODBC)
                con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                
                # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
                query41<-"select * from lobster.logs41"
                slipquery41<-"select  * from lobster.slips41"
                ziffquery41 <- "select * from lobster.ziff41"
                offquery41 <- "select * from lobster.crisofflogs41 "

                slip41 = sqlQuery(con, slipquery41)
                logs41 = sqlQuery(con, query41)
                ziff41 = sqlQuery(con, ziffquery41)
                off41 = sqlQuery(con, offquery41)

                off41 = subset(off41,DATE_FISHED < '1995-01-01')

                logs41$DDLON<-logs41$DDLON*-1
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
            a41$z[which(a41$z>450)] <- NA

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
              query41<-'NEED TO IDENITFY'
               
              logs41jonah = sqlQuery(con, query41)
              logs41jonah$DDLON<-logs41jonah$DDLON*-1
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


      vms.q <- paste("SELECT rownum vesid,
                  p.longitude lon, p.latitude lat, 
                 NVL(v.vessel_name,p.vr_number) vessel_name, 
                 p.vr_number vrn,
                 to_char(p.POSITION_UTC_DATE, 'YYYY/MM/DD HH24:MI:SS') vmsdate,
                 p.speed_knots 
                 FROM mfd_obfmi.vms_all p, mfd_obfmi.marfis_vessels_syn v
                 WHERE p.VR_NUMBER = v.vr_number(+)  
                 AND p.vr_number IN ('",vrn.vector,"')",
                  sep="" )

      vms.data <- sqlQuery(con, vms.q, believeNRows=FALSE)  
      odbcClose(con)
        vms.data$VMSDATE <- as.POSIXct(vms.data$VMSDATE,tz="GMT")  # VMS data is in UTC, assign timezone
  
  # Create date and time variables in local time
      vms.data$DATE <- format(strftime(vms.data$VMSDATE,format="%Y-%m-%d"), tz="America/Halifax",usetz=TRUE)
      vms.data$TIME <- format(strftime(vms.data$VMSDATE,format="%H:%M:%S"), tz="America/Halifax",usetz=TRUE)
      vms.data$YEAR <- format(strftime(vms.data$VMSDATE,format="%Y"), tz="America/Halifax",usetz=TRUE)
      vms.data$VMSDATElocal <- as.POSIXct(paste(vms.data$DATE, vms.data$TIME), format="%Y-%m-%d %H:%M:%S",tz="America/Halifax")

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
            atSea = sqlQuery(con, "select * from FRAILC.LOBSTER_ATSEA_VW")
            save( atSea, file=file.path( fnODBC, "atSea.rdata"), compress=T)
            gc()  # garbage collection
            odbcClose(con)
          }
          load(file.path( fnODBC, "atSea.rdata"), .GlobalEnv)
     }

### port sampling 
    if (DS %in% c("port.redo", "port") ) {

     if (DS=="port.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # port
        port = sqlQuery(con, "select a.SAMPLE_SEQ,a.SAMPLE_NO,a.SDATE,a.SEASON,a.NTRAPS,a.LATITUDE,a.LONGITUDE,a.GRADE, b.L_SIZE,b.N_MALES,b.N_FEM,b.NBF, c.LFA,c.PORT,c.COUNTY,c.STAT,c.PORT_CODE,c.LATITUDE port_lat,c.LONGITUDE port_lon from lobster.CRLENGCODE a, lobster.CRLENGFREQ b, lobster.CRLOCATIONS c where a.sample_seq = b.sample_seq and a.port = c.port and a.type = 'P' ")
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
     }

###Observer Length Frequencies

if(DS %in% c('lfa41.observer.samples.redo','lfa41.observer.samples')) {
   if (DS=="lfa41.observer.samples.redo") {
        require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
        
        # Denton Script Sept 28 2016
        obs.samp <- sqlQuery(con, paste("
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
        fsrs = sqlQuery(con, "select * from fsrs_lobster.FSRS_LOBSTER_VW")
        save( fsrs, file=file.path( fnODBC, "fsrs.rdata"), compress=T)
        gc()  # garbage collection
        odbcClose(con)
      }
      load(file.path( fnODBC, "fsrs.rdata"), .GlobalEnv)
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
        ILTSTowDepth<-sqlQuery(con, "select * from FRAILC.MARPORT_DEPTH")
        ILTSTowSpread<-sqlQuery(con, "select * from FRAILC.MARPORT_SPREAD")
        ILTSTowDist<-sqlQuery(con, "select * from FRAILC.MARPORT_TOWDIST")
        ILTSTemp<-sqlQuery(con, "select * from FRAILC.MINILOG_TEMP")
        NM1<-merge(ILTSTowDepth,ILTSTowSpread) #merge net mensuration into one file
        netMensuration<-merge( NM1,ILTSTowDist)#merge net mensuration into one file
        netMensuration$TTIME<-NULL #remove load date from merged file
        surveyCatch<-sqlQuery(con, "select * from lobster.ILTSSETS_MV")
        surveyMeasurements<-sqlQuery(con, "select * from lobster.ILTSDETAILS_MV")
        with(surveyMeasurements,paste(TRIP_ID,SET_NO,sep=''))->surveyMeasurements$SET_ID
        with(surveyCatch,paste(TRIP_ID,SET_NO,sep=''))->surveyCatch$SET_ID
        surveyCatch$SET_LONG<-surveyCatch$SET_LONG*-1
        surveyCatch$HAUL_LONG<-surveyCatch$HAUL_LONG*-1
        surveyCatch$YEAR<-year(surveyCatch$BOARD_DATE)
        surveyMeasurements$SET_LON<-surveyMeasurements$SET_LON*-1
        surveyMeasurements$HAUL_LON<-surveyMeasurements$HAUL_LON*-1
        surveyStationID<-sqlQuery(con, "select * from LOBSTER.ILTS_SURVEY_STATION")
        save(netMensuration, file=file.path( fnODBC, "netMensuration.rdata"), compress=T)
        save(surveyCatch, file=file.path( fnODBC, "surveyCatch.rdata"), compress=T)
        save(surveyMeasurements, file=file.path(fnODBC, "surveyMeasurements.rdata"), compress=T)
        save(ILTSTemp, file=file.path(fnODBC, "ILTSTemp.rdata"), compress=T)
        save(surveyStationID, file=file.path(fnODBC, "surveyStationID.rdata"), compress=T)
        
        gc()  # garbage collection
      }
      load(file.path( fnODBC, "netMensuration.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyCatch.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyMeasurements.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSTemp.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyStationID.rdata"), .GlobalEnv)
      
    }


  }

