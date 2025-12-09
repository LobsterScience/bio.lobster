#' @export

ILTS_summary <- function(redo_analysis=F,fname=c('ILTS_SETS_POSTPROCESS'),path = file.path(project.datadirectory('bio.lobster'),'analysis','ILTSSurvey','compiled'),pull=c('local','oracle')){
  
  dir.create(path,showWarnings = F)
   fname1 = 'ILTS_SETS_POSTPROCESS.rds'
  
  if(redo_analysis){
  #Recruits
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F,size=c(70,82),aggregate=T,species=2550,biomass = F,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = round(y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y0 = subset(y1,select=c('TRIP_ID','SET_NO','YEAR','SET_DATE','SET_LONG','VESSEL_NAME','SET_DEPTH','SET_LAT','temp','SET_TIME','sweptArea','GEAR','SA_CORRECTED_PRORATED_N'))
    names(y0)[13] = 'Recruit_N'
  
  #Commercial
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F,size=c(82,220),aggregate=T,species=2550,biomass = F,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = round(y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y2 = subset(y1,select=c('TRIP_ID','SET_NO','SA_CORRECTED_PRORATED_N'))
    names(y2)[3] = 'Commercial_N'
  
  #Commercial B
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F, size=c(82,220),aggregate=T,species=2550,biomass = T,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = (y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y3 = subset(y1,select=c('TRIP_ID','SET_NO','SA_CORRECTED_PRORATED_N'))
    names(y3)[3] = 'Commercial_B'
  
  #Female B>82
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F,sex=c(2,3), size=c(82,220),aggregate=T,species=2550,biomass = T,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = round(y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y4 = subset(y1,select=c('TRIP_ID','SET_NO','SA_CORRECTED_PRORATED_N'))
    names(y4)[3] = 'Female_B'
  
  #Small  
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F,size=c(50,70),aggregate=T,species=2550,biomass = F,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = round(y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y5 = subset(y1,select=c('TRIP_ID','SET_NO','SA_CORRECTED_PRORATED_N'))
    names(y5)[3] = 'Small_N'
    
    require(dplyr)
    require(purrr)
    dfs = list(y0,y2,y3,y4,y5)
    mu <- reduce(dfs, ~ left_join(.x, .y, by = c("TRIP_ID", "SET_NO")))
    
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F,size=NULL,aggregate=T,species=2550,biomass = F,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = round(y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y5 = subset(y1,select=c('TRIP_ID','SET_NO','YEAR','SET_DATE','SET_LONG','VESSEL_NAME','SET_DEPTH','SET_LAT','temp','SET_TIME','sweptArea','GEAR','SA_CORRECTED_PRORATED_N'))
    names(y5)[13] = 'Total_N'
    
    y1 = ILTS_ITQ_All_Data(redo_base_data = T,redo_set_data = F,size=NULL,aggregate=T,species=2550,biomass = T,applyGearConversion = T)
    y1$SA_CORRECTED_PRORATED_N = (y1$SA_CORRECTED_PRORATED_N*y1$sweptArea)
    y6 = subset(y1,select=c('TRIP_ID','SET_NO','YEAR','SET_DATE','SET_LONG','VESSEL_NAME','SET_DEPTH','SET_LAT','temp','SET_TIME','sweptArea','GEAR','SA_CORRECTED_PRORATED_N'))
    names(y6)[13] = 'Total_B'
    
    
    mu1 = merge(y5,mu,all=T)
    mu2 = merge(mu1,y6,all=T)
    
    names(mu2)[9] = 'Temperature'
    
    gu = save_with_metadata(mu2,file = file.path(path,fname1))
    
    db.setup(un=oracle.lobster.user,pw=oracle.lobster.password)
    Sys.setenv(TZ = "America/Curacao")
    Sys.setenv(ORA_SDTZ = "America/Curacao")   
    dbExecute(conn=con, paste("drop table ",fname,SEP=""))
    dbExecute(conn=con, statement = paste("create table ",fname,
                                            "(TRIP_ID NUMBER(38,0),
	                                           SET_NO NUMBER(3,0),
	                                           YEAR NUMBER(4,0),
	                                           SET_DATE DATE,
	                                        SET_LONG NUMBER(38,8),
	                                        VESSEL_NAME VARCHAR2(255 BYTE),
	                                        SET_DEPTH NUMBER(38,2),
	                                        SET_LAT NUMBER(38,8),
	                                        TEMPERATURE NUMBER(38,4),
	                                        SET_TIME VARCHAR(255 BYTE),
	                                        SWEPT_AREA NUMBER(8,8),
	                                        GEAR VARCHAR(255 BYTE),
	                                        TOTAL_N NUMBER(38,0),
	                                        RECRUIT_N NUMBER(38,0),
	                                        COMMERCIAL_N NUMBER(38,0),
	                                        COMMERCIAL_B NUMBER(38,8),
	                                        FEMALE_B NUMBER(38,8),
	                                        SMALL_N NUMBER(38,0),
	                                        TOTAL_B NUMBER(38,8))",SEP=" "))
    
    dbWriteTable(conn=con,name=fname,append=T, value=mu2)
    com = paste("'File created by", gu$metadata$user,'on',gu$metadata$created_at,"'" ,SEP=" ")
    dbSendQuery(conn=con, statement = paste('COMMENT ON TABLE lobster.',fname,' IS ',com,sep=""))
    
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".SWEPT_AREA IS 'area in km2'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".TOTAL_N IS 'total count of lobster in catch,gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".RECRUIT_N IS 'total count of lobster 70-82mm (inclusive) in catch,gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".COMMERCIAL_N IS 'total count of lobster >82.5 in catch,gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".COMMERCIAL_B IS 'total weight of lobster >82.5 in catch,gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".FEMALE_B IS 'total weight of female lobster >82.5 in catch, proxy for ssb,gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".SMALL_N IS 'total count of lobster 50-70mm (inclusive),gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("COMMENT ON COLUMN lobster.",fname,".TOTAL_B IS 'total weight of lobster,gear corrected'",sep=""))
    dbSendQuery(conn=con, statement = paste("GRANT SELECT ON lobster.",fname," to PEDLOBSTERGROUP",sep=""))
    
    }
  
   if(pull=='oracle'){
      db.setup(un=oracle.personal.user, pw = oracle.personal.password)
    gu = connect.command(conn=con, statement = paste('select * from LOBSTER.',fname,sep=""))
    comm=  dbGetQuery(con,"SELECT comments FROM all_tab_comments WHERE owner = 'LOBSTER' AND table_name = 'ILTS_SETS_POSTPROCESS'")
    
    saveRDS(list(gu,comm),file=file.path(path,fname1))
    print(paste('file saved to ',file.path(path,fname1)))
  }
   if(pull=='local'){
     gu = readRDS(file=file.path(path,fname1))
   }
   return(gu)
  }
    