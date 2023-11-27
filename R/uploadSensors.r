#' @export
uploadSensors <- function(datafile,tablenm, appendIt=F,UID='cooka',PWD='thisisntit'){
        Sys.setenv(TZ = "GMT")
        Sys.setenv(ORA_SDTZ = "GMT")   
   if(any(grepl('.csv',datafile))) datafile = read.csv(datafile,header=T)
      if(dim(datafile)[2]!=19) stop('The number of columns in the datafile do not match the number required (19)') 
        datafile$FLAG=NA
      bio.lobster::db.setup(un=UID,pw=PWD)  
      datafile$GPSDATE = as.Date(datafile$GPSDATE)
      datafile$GPSTIME = sprintf("%06d",datafile$GPSTIME)
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
