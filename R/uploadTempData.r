#' @export
uploadTempData <- function(datafile,tablenm, appendIt=F,UID='cooka',PWD='thisisntit',year=2022, tripid=NULL,source=NULL,idSets=NULL){
        Sys.setenv(TZ = "GMT")
        Sys.setenv(ORA_SDTZ = "GMT")   
        bio.lobster::db.setup(un=UID,pw=PWD)  
      if(any(grepl('.csv',datafile,ignore.case = T))){
     
            datafile = read.table(datafile,sep = ";")
            datafile = datafile[-1,]
            names(datafile)=c('Number','STDDATE','STDTIME','TEMPC','DEPTHM')
            datafile = bio.utilities::toNums(datafile,c(1,4,5))
            datafile$STDDATE = as.Date(datafile$STDDATE,format="%d-%m-%Y")
            datafile$STDTIME = chron::times(datafile$STDTIME)
            
            datafile$da = as.POSIXct(paste(datafile$STDDATE, datafile$STDTIME),format='%Y-%m-%d %H:%M:%S')
            datafile$dau = (datafile$da)+3*60*60
            datafile$UTCDATE = (strptime(datafile$dau,format='%Y-%m-%d'))
            datafile$UTCTIME = as.character(format(datafile$dau,format='%H:%M:%S'))
            datafile$STDTIME = as.character(datafile$STDTIME)
            datafile$TRIP_ID= ifelse(!is.null(tripid),tripid, NA)
            datafile$SOURCE = ifelse(!is.null(source),source, NA)
            datafile$UTCDATE = as.Date(datafile$UTCDATE)
            datafile$SET_NO = NA
        }
      if(!is.null(idSets)){
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
        j = as.POSIXct(unique(datafile$STDDATE))
        if(length(j)==1 & any(j==k)){
              seP = subset(se, SETDATE==j)
                for(i in 1:nrow(seP)){
                    sePP = seP[i,]
                    ii = which(datafile$STDTIME>sePP$STARTDIFF &datafile$STDTIME<sePP$ENDDIFF)
                    datafile$SET_NO[ii] = sePP$SET_NO
                    datafile$TRIP_ID[ii] = sePP$TRIP_ID
                  }
        }
        datafile = subset(datafile, !is.na(SET_NO))
        
      }
        datafile = subset(datafile,select=c(TEMPC, DEPTHM, UTCDATE, UTCTIME,STDDATE, STDTIME, SET_NO,TRIP_ID, SOURCE ))
        if(dim(datafile)[2]!=9) stop('The number of columns in the datafile do not match the number required (9)')  
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
