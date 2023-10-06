#' @export
uploadOlexData <- function(fn='C:/Users/CookA/Downloads/ILTS2023_LEG_1_track.txt',tablenm, appendIt=F,UID='cooka',PWD='thisisntit',year=2022, tripid=NULL,source=NULL,idSets=NULL){
        Sys.setenv(TZ = "GMT")
        Sys.setenv(ORA_SDTZ = "GMT")   
        bio.lobster::db.setup(un=UID,pw=PWD)  
        
        lines <- readLines(fn)
        nheadLines = 18
        lines <- lines[(nheadLines):length(lines)]
        elements <- lapply(strsplit(lines, ","), unlist)
        max_length <- max(sapply(elements, length))
        elements <- lapply(elements, function(x) {
          if (length(x) < max_length) c(x, rep(NA, max_length - length(x)))
          else x
        })
        
        df <- as.data.frame(do.call(rbind, elements))
        df1 = strsplit(as.character(df$V1),split= " ")
        df1 = as.data.frame(do.call(rbind,df1))
        df1 = df1[,c(2,4,5,6)]
        
        df1$V2 = substr(df1$V2,start=2,stop=nchar(df1$V2)-2)
        
        remove_b0 <- function(x) {
          gsub("<b0>", "", x)
        }
        
        # Apply the function to each element in the vector
        modified_vector <- lapply(df1$V2, remove_b0)
        
        # Convert the result back to a vector
        df1$V2 <- unlist(modified_vector)
        
        df1$V4 =substr(df1$V4,start=1,stop=nchar(df1$V4)-1) 
        modified_vector <- lapply(df1$V4, remove_b0)
        
        # Convert the result back to a vector
        df1$V4 <- unlist(modified_vector)
        
        names(df1) = c('Y','X','Date','Time')
        
         
        
            df1$Date = as.Date(df1$Date,format="%d-%m-%Y")
            df1$Time = chron::times(df1$Time)
            
            df1$Y = bio.lobster::convert.dd.dddd(as.numeric(df1$Y))
            df1$X = bio.lobster::convert.dd.dddd(as.numeric(df1$X))
            
            df1$da = as.POSIXct(paste(df1$Date, df1$Time),format='%Y-%m-%d %H:%M:%S')
            df1$dau = (df1$da)-3*60*60
            df1$StdDATE = (strptime(df1$dau,format='%Y-%m-%d'))
            df1$StdTIME = as.character(format(df1$dau,format='%H:%M:%S'))
            
            
            
      if(!is.null(idSets)){
        bio.lobster::db.setup()
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
        df1$Date = as.POSIXct((df1$StdDATE))
        df1$TRIP_ID = df1$SET_NO = NA
        oo = list()
        m=0
        for(i in 1:length(k)){
              seP = subset(se, SETDATE==k[i])
              df1P = subset(df1,Date==k[i])
                if(nrow(df1P)>5){
                for(b in 1:nrow(seP)){
                    sePP = seP[b,]
                    ii = which(df1P$StdTIME>sePP$STARTDIFF & df1P$StdTIME<sePP$ENDDIFF)
                    if(length(ii)>2){
                      m=m+1
                    df1P$SET_NO[ii] = sePP$SET_NO
                    df1P$TRIP_ID[ii] = sePP$TRIP_ID
                    oo[[m]] = df1P 
                    }
                  }
                }
              }
        da = do.call(rbind,oo)
        datafile = subset(da, !is.na(SET_NO),select=c(Y,X,StdDATE,StdTIME,SET_NO,TRIP_ID))
        
      
      if(appendIt==F & ROracle::dbExistsTable(con, tablenm)) stop('table already exists in the space. You need to either use a new name or use appendIT=T')
      if(appendIt==F & !ROracle::dbExistsTable(con, tablenm)){    
                        dbSendQuery(conn=con, statement = paste("create table ",tablenm,"(",
                                              " Y NUMBER(10,0),
                                                X NUMBER(10,0),
                                                STDDATE DATE, 
                                              STDTIME VARCHAR2(26 BYTE),
                                                SET_NO NUMBER(4,0),
                                                TRIP_ID NUMBER(10,0)
                                                )",SEP=" ")
                        )
        print(paste('Table ',tablenm,' has been created',sep=""))
        appendIt=T
      }
    dbWriteTable(conn=con,name=tablenm,append=appendIt, value=datafile)
    print('fileUploaded')

}
