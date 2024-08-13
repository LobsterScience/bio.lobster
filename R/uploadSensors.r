#' @export
uploadSensors <- function(datafile,tablenm, appendIt=F,UID='cooka',PWD='thisisntit',year=2024){
        Sys.setenv(TZ = "GMT")
        Sys.setenv(ORA_SDTZ = "GMT")   
   if(any(grepl('.csv',datafile))) datafile = read.csv(datafile,header=T)
      if(dim(datafile)[2]!=19) stop('The number of columns in the datafile do not match the number required (19)') 
        datafile$FLAG=NA
      bio.lobster::db.setup(un=UID,pw=PWD)  
      datafile$GPSDATE = as.Date(datafile$GPSDATE)
      datafile$GPSTIME = sprintf("%06d",datafile$GPSTIME)
      datafile$GPSTIME = format(as.POSIXct(datafile$GPSTIME, format="%H%M%S") - as.difftime(180, units="mins"),"%H:%M:%S")
      se = dbGetQuery(conn=con, statement = paste("select trip_id, set_no, min(starttime) starttime, min(endtime) endtime, min(setdate) setdate
                      from (select a.trip_id, b.set_no,case when c.pntcd_id = 2 then to_char(to_date(settime, 'hh24miss'),'hh24:mi:ss')else null end starttime,case when c.pntcd_id = 3 then to_char(to_date(settime, 'hh24miss'),'hh24:mi:ss')else null end endtime,case when pntcd_id = 2 then setdate
                      else null end setdate
                      from isdb.istrips a, isdb.isfishsets b, isdb.issetprofile c
                      where a.trip_id = b.trip_id
                      and b.fishset_id = c.FISHSET_ID
                      and to_char(a.board_date,'yyyy') = ",year," and a.tripcd_id = 7065and b.haulccd_id = 1)group by trip_id, set_no
                      order by trip_id, setdate, set_no",sep=" ")
      )
      se$STARTDIFF <- format(as.POSIXct(se$STARTTIME, format="%H:%M:%S", tz="UTC") - as.difftime(10, units="mins"),"%H:%M:%S")
      se$ENDDIFF <- format(as.POSIXct(se$ENDTIME, format="%H:%M:%S", tz="UTC") + as.difftime(10, units="mins"),"%H:%M:%S")
      k = unique(se$SETDATE)
      j = as.POSIXct(unique(datafile$GPSDATE))
      if(length(j)==1 & any(j==k)){
        seP = subset(se, SETDATE==j)
        for(i in 1:nrow(seP)){
          sePP = seP[i,]
          ii = which(datafile$GPSTIME>sePP$STARTDIFF &datafile$GPSTIME<sePP$ENDDIFF)
          datafile$SET_NO[ii] = sePP$SET_NO
          datafile$TRIP_ID[ii] = sePP$TRIP_ID
        }
      }
      datafile = subset(datafile, !is.na(SET_NO))

            if(appendIt==F & ROracle::dbExistsTable(con, tablenm)) stop('table already exists in the space. You need to either use a new name or use appendIT=T')
      if(appendIt==F & !ROracle::dbExistsTable(con, tablenm)){    
                        dbSendQuery(conn=con, statement = paste("create table ",tablenm,"(",
                                              "CPUDATEANDTIME DATE,
                                               GPSTIME VARCHAR2(26BYTE),
                                               LATITUDE VARCHAR2(14BYTE),
                                               LONGITUDE VARCHAR2(15BYTE),
                                               SPEED NUMBER(3,1),
                                               HEADING NUMBER(3,0),
                                               VALIDITY VARCHAR2(10BYTE),
                                               TRANSDUCERNAME VARCHAR2(26BYTE),
                                               SENSORNAME VARCHAR2(26BYTE),
                                               SENSORVALUE NUMBER(11,6),
                                               ERRORCODE VARCHAR2(26BYTE),
                                               HYDROPHONE VARCHAR2(26BYTE),
                                               SET_NO NUMBER(3,0),
                                               TRIP_ID NUMBER(10,0),
                                               GPSDATE DATE,
                                               DDLAT NUMBER(10,0),
                                               DDLON NUMBER(10,0),
                                               SOURCE VARCHAR2(20BYTE),
                                               SIGNALSTRENGTH NUMBER,
                                                FLAG NUMBER)",SEP=" ")
                        )
        print(paste('Table ',tablenm,' has been created',sep=""))
        appendIt=T
      }
    dbWriteTable(conn=con,name=tablenm,append=appendIt, value=datafile)
    print('fileUploaded')

}
