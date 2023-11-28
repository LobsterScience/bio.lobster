#' @export
uploadClickTouch <- function(datafile,tablenm, appendIt=F,UID='cooka',PWD='thisisntit'){
        Sys.setenv(TZ = "GMT")
        Sys.setenv(ORA_SDTZ = "GMT")   
      if(any(grepl('.csv',datafile))) datafile = read.csv(datafile,header=T)
      if(dim(datafile)[2]!=12) stop('The number of columns in the datafile do not match the number required (12)') 
        bio.lobster::db.setup(un=UID,pw=PWD)  
      datafile = subset(datafile, select=c(startdate, starttime, enddate, endtime, depth, year, trip, set.no,quality.wingspread, quality.touchdown, quality.liftoff, explanation))
      datafile$startdate = as.Date(datafile$startdate)
      datafile$enddate = as.Date(datafile$enddate)
      if(appendIt==F & ROracle::dbExistsTable(con, tablenm)) stop('table already exists in the space. You need to either use a new name or use appendIT=T')
      if(appendIt==F & !ROracle::dbExistsTable(con, tablenm)){    
                        dbSendQuery(conn=con, statement = paste("create table ",tablenm," (STARTDATE DATE,
                                              STARTTIME VARCHAR2(8),
                                              ENDDATE DATE,
                                              ENDTIME VARCHAR2(8),
                                              DEPTHM NUMBER(38,8),
                                              YEAR NUMBER(38,0),
                                              TRIP_ID NUMBER(38,0),
                                              SET_NO NUMBER(38,0),
                                              QUALITY_WINGSPREAD NUMBER(38,0),
                                              QUALITY_TOUCHDOWN NUMBER(38,0),
                                              QUALITY_LIFTOFF NUMBER(38,0),
                                              EXPLANATION VARCHAR(128))
                                              ",SEP=" ")
                                    )
        print(paste('Table ',tablenm,' has been created',sep=""))
        appendIt=T
      }
    
      dbWriteTable(conn=con,name=tablenm,append=appendIt, value=datafile)
    print('fileUploaded')

}
