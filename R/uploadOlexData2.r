#' @export

uploadOlexData2 <- function(fn='C:/LOCAL WORKING FOLDER/ILTS/Olex Tracks/ILTS_2022_leg_2_track.txt',months=c(5,6,7), tablenm='ILTS_OLEXTRACKS', appendIt=F,UID='frailc',PWD='notit',year=2022){
  Sys.setenv(TZ = "GMT")
  Sys.setenv(ORA_SDTZ = "GMT")   
  bio.lobster::db.setup(un=UID,pw=PWD)  
  options(digits=10)
  lines <- readLines(fn)
  nheadLines = 18
  lines <- lines[(nheadLines):length(lines)]
    elements <- as.data.frame(stringr::str_split_fixed(lines, "\\s+",6))
    elements$V6 <-elements$V1 <- NULL
    df1 = elements  
  
    convert_coord <- function(coord_str) {
      coord_str = stringi::stri_trans_general(coord_str,id='latin-ascii')
      clean <- gsub("[^0-9.]", " ", coord_str)
      parts <- strsplit(clean, "\\s+")[[1]]
      parts <- parts[parts != ""]
      
      deg <- as.numeric(parts[1])
      min <- as.numeric(parts[2])
      
      return(deg + (min / 60))
    }
    
  require(dplyr)
    
    df1 = df1 %>%
          mutate(V2=sapply(V2,convert_coord),
                 V3 = sapply(V3,convert_coord))
    
    
  names(df1) = c('Y','X','Date','Time')
  
  
  
  df1$Date = as.Date(df1$Date,format="%d/%m/%Y")
  df1$Time = chron::times(df1$Time)
  
  df1$X = df1$X * -1
  
  df1$da = as.POSIXct(paste(df1$Date, df1$Time),format='%Y-%m-%d %H:%M:%S')
  df1$dau = (df1$da)-3*60*60
  df1$StdDATE = (strptime(df1$dau,format='%Y-%m-%d'))
  df1$StdTIME = as.character(format(df1$dau,format='%H:%M:%S'))
  
  
  bio.lobster::db.setup(un=UID,pw = PWD)
  se = dbGetQuery(conn=con, statement = paste("select trip_id, set_no, min(starttime) starttime, min(endtime) endtime, min(setdate) setdate
                      from (select a.trip_id, b.set_no,case when c.pntcd_id = 2 then to_char(to_date(settime, 'hh24miss'),'hh24:mi:ss')else null end starttime,
                      case when c.pntcd_id = 3 then to_char(to_date(settime, 'hh24miss'),'hh24:mi:ss')else null end endtime,case when pntcd_id = 2 then setdate
                      else null end setdate
                      from isdb.istrips a, isdb.isfishsets b, isdb.issetprofile c
                      where a.trip_id = b.trip_id
                      and b.fishset_id = c.FISHSET_ID
                      and to_char(a.board_date,'yyyy') = ",year," and a.tripcd_id = 7065and b.haulccd_id = 1
                      and a.trip_id not in ('100058328', '100058330')
                      )group by trip_id, set_no
                      order by trip_id, setdate, set_no",sep=" ")
  )
  se$STARTDIFF <- format(as.POSIXct(se$STARTTIME, format="%H:%M:%S", tz="UTC") - as.difftime(10, units="mins"),"%H:%M:%S")
  se$ENDDIFF <- format(as.POSIXct(se$ENDTIME, format="%H:%M:%S", tz="UTC") + as.difftime(10, units="mins"),"%H:%M:%S")
  se = subset(se,month(se$SETDATE) %in% months)
  k = unique(se$SETDATE)
  
  df1$Date = as.POSIXct((df1$StdDATE))
  df1$TRIP_ID = df1$SET_NO = NA
  oo = list()
  m=0
  for(i in 1:length(k)){
    seP = subset(se, SETDATE==k[i])
    df1P = subset(df1,Date==k[i])
    m=m+1
    if(nrow(df1P)>5){
      for(b in 1:nrow(seP)){
        sePP = seP[b,]
        ii = which(df1P$StdTIME>sePP$STARTDIFF & df1P$StdTIME<sePP$ENDDIFF)
        if(length(ii)>2){
          df1P$SET_NO[ii] = sePP$SET_NO
          df1P$TRIP_ID[ii] = sePP$TRIP_ID
        }
      }
   }
   oo[[m]] = df1P 
  }
  da = do.call(rbind,oo)
  datafile = subset(da, !is.na(SET_NO),select=c(Y,X,StdDATE,StdTIME,SET_NO,TRIP_ID))
  datafile$X = round(datafile$X,6)
  datafile$Y = round(datafile$Y,6)
datafile$StdDATE = as.Date(strptime(datafile$StdDATE,format='%Y-%m-%d'))
  
  names(datafile) = toupper(names(datafile))
  
  if(appendIt==F & ROracle::dbExistsTable(con, tablenm)) stop('table already exists in the space. You need to either use a new name or use appendIT=T')
  if(appendIt==F & !ROracle::dbExistsTable(con, tablenm)){   
    dbSendQuery(conn=con, statement = paste("create table ",tablenm," (
                  Y         NUMBER(10, 6),
                  X         NUMBER(10, 6),
                  STDDATE   DATE,
                  STDTIME   VARCHAR2 (1000),
                  SET_NO    NUMBER(4, 0),
                  TRIP_ID   NUMBER(10, 0))",SEP=" ")
                  )
    print(paste('Table ',tablenm,' has been created',sep=""))
    appendIt=T
  }
  dbWriteTable(conn=con,name=tablenm,append=appendIt, value=datafile)
  print('fileUploaded')
  return(datafile)  
}

