#' @export
uploadTempData <- function(datafile,tablenm, appendIt=F,UID='cooka',PWD='thisisntit'){
        Sys.setenv(TZ = "GMT")
        Sys.setenv(ORA_SDTZ = "GMT")   
   if(any(grepl('.csv',datafile,ignore.case = T))){
     
            datafile = read.table(datafile,sep = ";")
            datafile = datafile[-1,]
            names(datafile)=c('Number','STDDATE','STDTIME','TEMPC','DEPTHM')
            datafile = bio.utilities::toNums(datafile,c(1,4,5))
            datafile$STDDATE = as.Date(datafile$STDDATE,format="%m-%d-%Y")
            datafile$STDTIME = chron::times(datafile$STDTIME)
            
            datafile$da = as.POSIXct(paste(datafile$STDDATE, datafile$STDTIME),format='%Y-%m-%d %H:%M:%S')
            datafile$dau = (datafile$da)+3*60*60
            datafile$UTCDATE = (strptime(datafile$dau,format='%Y-%m-%d'))
            datafile$UTCTIME = as.character(format(datafile$dau,format='%H:%M:%S'))
            datafile$STDTIME = as.character(datafile$STDTIME)
            datafile$TRIP_ID=datafile$SET_NO=0
            datafile$SOURCE = 'STARODDI'
            datafile$UTCDATE = as.Date(datafile$UTCDATE)
             datafile = subset(datafile,select=c(TEMPC, DEPTHM, UTCDATE, UTCTIME,STDDATE, STDTIME, SET_NO,TRIP_ID, SOURCE ))
                 }
      if(dim(datafile)[2]!=9) stop('The number of columns in the datafile do not match the number required (9)')  
      bio.lobster::db.setup(un=UID,pw=PWD)  
      datafile$STDDATE = as.Date(datafile$STDDATE)
      if(appendIt==F & ROracle::dbExistsTable(con, tablenm)) stop('table already exists in the space. You need to either use a new name or use appendIT=T')
      if(appendIt==F & !ROracle::dbExistsTable(con, tablenm)){    
                        dbSendQuery(conn=con, statement = paste("create table ",tablenm,"(",
                                              "  TEMPC NUMBER(8,4),
                                                DEPTHM NUMBER(10,4),
                                                UTCDATE DATE,
                                                UTCTIME VARCHAR2(26 BYTE),
                                                STDDATE DATE,
                                                STDTIME VARCHAR2(26 BYTE),
                                                SET_NO NUMBER(4,0),
                                                TRIP_ID NUMBER(10,0),
                                                SOURCE VARCHAR2(20 BYTE) )",SEP=" ")
                        )
        print(paste('Table ',tablenm,' has been created',sep=""))
        appendIt=T
      }
    dbWriteTable(conn=con,name=tablenm,append=appendIt, value=datafile)
    print('fileUploaded')

}
