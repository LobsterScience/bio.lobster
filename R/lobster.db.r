#' lobster.db
#'
#' This function is the main workhorse to pull data from databases and some initial filtering of data used in lobster stock assessments. Results are saved and can be reloaded using this function.
#' @param DS is the main switch that selects which data source to load or operate. Options for DS include 'complete','annual.landings','logs','logs41','logs41jonah','observer41','atSea','cris','port','vlog','fsrs','scallop','survey','annual.landings'.  Any of these arguements called as listed return the data object. To make the data file from scratch would require a 'XXXX.redo', where XXXX is the option listed above.
#' @return Data objects that contain the data for use in further analyses.
#' @examples lobster.db('fsrs.redo') # makes the data objects for the FSRS data.
#' lobster.db('fsrs') #loads the object fsrs
#' @export

lobster.db = function( DS="complete.redo",pH=p) {
    options(stringsAsFactors=F)

  require(lubridate)
  #require(RODBC)
  if(grepl('redo', DS)) db.setup(un=oracle.lobster.user,pw=oracle.lobster.password) #Chooses RODBC vs ROracle based on R version and installed packages. db.setup(RODBC=T) will force RODBC
    fn.root =  file.path( project.datadirectory('bio.lobster'), "data")
    fnODBC  =  file.path(fn.root, "ODBCDump")
    fnProducts = file.path(fn.root,'products')
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnODBC, recursive = TRUE, showWarnings = FALSE )
    dir.create( fnProducts, recursive = TRUE, showWarnings = FALSE )

if(DS %in% c('percent_reporting')){
    print('This requires ODBC connection as it is refreshed everytime you run it; please note in oracle this is a materialized view, requiring refresh')  
      db.setup()
      vsP = connect.command(con,"select * from lobster.percent_reporting")
      return(vsP)
    }

    if(DS %in% c('amo','amo.redo')){
      fli =  file.path(project.datadirectory('bio.lobster'),'data','amo.csv')
      if(grepl('redo', DS)){
      wp = read.table('https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.amo.dat',skip=1,header = T)
      write.csv(wp,fli,row.names = F)
      }
      
      return(read.csv(fli))
    }
    
    
if(DS %in% c('licence_categories')){
      cats = readxl::excel_sheets(file.path(project.datadirectory('bio.lobster'),'data','LicenceHolder','Lobster_Licences_1999-2022.xlsx'))
      ou=list()
      for(i in 1:length(cats)){
      ou[[i]] = readxl::read_excel(file.path(project.datadirectory('bio.lobster'),'data','LicenceHolder','Lobster_Licences_1999-2022.xlsx'),sheet=i)
      }
      ca = dplyr::bind_rows(ou)
      ca$LFA1 = substr(ca$LFA,24,100)
      i = grep('GREY',ca$LFA)
      ca$LFA1[i] = '38B'
      ca$LFA = ca$LFA1
      ca$LFA1 = NULL
      return(ca)
    }

if(DS %in% c('licence_characteristics', 'licence_characteristics_redo')){
  fn = file.path(fnODBC,'licence_characteristics.rds')
      if(grepl('redo',DS)){

      vsP = connect.command(con,"SELECT
    l.species_code,
    l.licence_id,
    a.area lfa,
    lp.fin,
    p.surname,
    p.firstname,
    p.birthdate,
    lp.START_DATE,
    lt.desc_eng lic_type,
    lst.desc_eng lic_subtype,
    p.community_code,
    c.community_name,
  --  p.address1,
  --  p.address2,
  --  p.address3,
  --  p.postal_cd,
  --    p.telephone,
  --    p.email_address,
    v.vr_number,
    v.vessel_name
FROM
    marfissci.participants           p,
    marfissci.vessels                v,
    marfissci.licences               l,
    marfissci.licence_participants   lp,
    marfissci.licence_areas          la,
    marfissci.areas                  a,
    marfissci.licence_vessels        lv,
    marfissci.communities            c,
    marfissci.licence_types          lt,
    marfissci.licence_subtypes       lst
WHERE
    l.licence_id = lp.licence_id
    AND l.licence_id = la.licence_id
    AND l.licence_id = lv.licence_id (+)
    AND lv.vr_number = v.vr_number (+)
    AND la.area_id = a.area_id
    AND lp.fin = p.fin
    AND p.community_code = c.community_code
    and lt.licence_type_id = l.licence_type_id
    and lst.licence_subtype_id = l.licence_subtype_id
    AND l.species_code = 700
    AND SYSDATE BETWEEN lp.start_date AND lp.end_date
    AND SYSDATE BETWEEN la.start_date AND la.end_date
    AND SYSDATE BETWEEN lv.start_date (+) AND lv.end_date (+)")
    vsP$YearsHeld = lubridate::year(Sys.Date())- lubridate::year(vsP$START_DATE)
    vsP$Age = lubridate::year(Sys.Date())- lubridate::year(vsP$BIRTHDATE)
      saveRDS(vsP, file=file.path(fnODBC,'licence_characteristics.rds'))
      return(vsP)
      }

      return(readRDS(fn))
    }

if(DS %in% c('licence_ages','licence_ages.redo')){
      if(grepl('redo',DS)) { 
      oo = connect.command(con,"
    SELECT
    l.licence_id,
    a.area lfa,
    lp.fin,
    p.surname,
    p.firstname,
    p.birthdate,
    lp.START_DATE,
    lp.END_DATE,
    lst.desc_eng lic_subtype 
    FROM
    marfissci.participants           p,
    marfissci.licences               l,
    marfissci.licence_participants   lp,
    marfissci.licence_areas          la,
    marfissci.areas                  a,
    marfissci.licence_subtypes       lst
    WHERE
    l.licence_id = lp.licence_id
    AND l.licence_id = la.licence_id
    AND la.area_id = a.area_id
    AND lp.fin = p.fin
    and lst.licence_subtype_id = l.licence_subtype_id
    AND l.species_code = 700
    and a.area in ('27','28','29','30','31A','31B','32','33','34','35','36','37','38')")
      save(oo, file=file.path(fnODBC,'licence_ages.rdata'))
      return(oo)
    }
    
    load(file=file.path(fnODBC,'licence_ages.rdata'))
    return(oo)
    
}


if(DS %in% c('inflation')){

      #from https://www.in2013dollars.com/Canada-inflation
      infl = read.csv(file.path(project.datadirectory('bio.lobster'),'data','inflation_data.csv'))
      return(infl)
    }

if(DS %in% c('old_slips','old_slips.redo')){
      if(grepl('redo',DS)) {
        #1975-1996
        ##only needs to be done once so turning off for the redo
        vsO = connect.command(con,"SELECT
         date_landed  da,    prov_code    || district    || port_code port,    cfv          boatvesid,    wt_lbs,    lfa,value
         FROM
         (
           SELECT            date_landed,            prov_code,            CASE                WHEN
           district = ' 1' THEN                    '01'
           WHEN district = ' 2' THEN
           '02'
           WHEN district = ' 3' THEN
           '03'
           WHEN district = ' 4' THEN
           '04'
           WHEN district = ' 5' THEN
           '05'
           WHEN district = ' 6' THEN
           '06'
           WHEN district = ' 7' THEN
           '07'
           WHEN district = ' 8' THEN
           '08'
           WHEN district = ' 9' THEN
           '09'
           ELSE
           district
           END              district,
           CASE
           WHEN port_code = ' 1' THEN
           '01'
           WHEN port_code = ' 2' THEN
           '02'
           WHEN port_code = ' 3' THEN
           '03'
           WHEN port_code = ' 4' THEN
           '04'
           WHEN port_code = ' 5' THEN
           '05'
           WHEN port_code = ' 6' THEN
           '06'
           WHEN port_code = ' 7' THEN
           '07'
           WHEN port_code = ' 8' THEN
           '08'
           WHEN port_code = ' 9' THEN
           '09'
           ELSE
           port_code
           END              port_code,
           match_cfv        cfv,
           landed_qty_lbs_n wt_lbs, value,
           CASE
           WHEN lobster_district = '1'   THEN
           '36'
           WHEN lobster_district = '2'   THEN
           '38'
           WHEN lobster_district = '3'   THEN
           '35'
           WHEN lobster_district = '5'   THEN
           '31_32'
           WHEN lobster_district = '5A'  THEN
           '32'
           WHEN lobster_district = '5B'  THEN
           '31A'
           WHEN lobster_district = '5B1' THEN
           '31B'
           WHEN lobster_district = '6A'  THEN
           '30'
           WHEN lobster_district = '6B'  THEN
           '27'
           WHEN lobster_district = '7A'  THEN
           '29'
           WHEN lobster_district = '7A1' THEN
           '28'
           WHEN lobster_district = 'A'
           AND substr(date_landed, 1, 4) > 1976 THEN
           '41'
           WHEN match_cfv IN ( '000530', '000790', '001614', '005366', '005461',
                               '005569', '012136', '012148' )
           AND substr(date_landed, 1, 4) = 1975 THEN
           '41'
           WHEN match_cfv IN ( '000530', '001614', '005366', '005461', '005569',
                               '012148' )
           AND substr(date_landed, 1, 4) = 1976 THEN
           '41'
           WHEN lobster_district = '4B'
           AND match_cfv NOT IN ( '000115', '000530', '000790', '001530', '001532',
                                  '001540', '001550', '001614', '004005', '004034',
                                  '004056', '005366', '005461', '005569', '005611',
                                  '005690', '012136', '012148', '100989', '101315',
                                  '100990' ) THEN
           '33'
           WHEN lobster_district = '4A'
           AND match_cfv NOT IN ( '000115', '000530', '000790', '001530', '001532',
                                  '001540', '001550', '001614', '004005', '004034',
                                  '004056', '005366', '005461', '005569', '005611',
                                  '005690', '012136', '012148', '100989', '101315',
                                  '100990' ) THEN
           '34'
           WHEN lobster_district = '7B'  THEN
           '7B'
           ELSE
           'NA'
           END              lfa
           FROM
           lobster.lob_log_est_1975_1996
           WHERE
           species_code = 700
           AND substr(date_landed, 1, 4) < 1997)"
         )
         vsO <- transform(vsO, DA = as.Date(as.character(DA), "%Y%m%d"))
         vsO$v = gsub(",","",vsO$VALUE)
         vsO$VALUE = as.numeric(vsO$v)
         vsO$WT_LBS = as.numeric(vsO$WT_LBS)
         vsO$PRICE = NA
         vsO$PRICE = ifelse(vsO$WT_LBS>0,vsO$VALUE/vsO$WT_LBS, vsO$PRICE)
        save(vsO , file=file.path(fnODBC,'slips7596.rdata'))

        ##1997 change in method of reporting lobster from buyers sales slips to monthly reporting documents 
        ##sent in by fishers, effective Nov. 95
        ##1997 landings taken from Table 19 due to inaccuracy in ZIFF
        vsM = connect.command(con, "select year, slip_code,trip_code,T_PRICE_PER_UNIT PRICE,T_LANDED_QUANTITY WT_LBS, PORT_LANDED PORT,CFV_NO,  a.LICENCE_NO,LICENCE_ID,
        CASE
        WHEN lobster_district IN ('1', '1   ')   THEN
        '36'
        WHEN lobster_district IN ('2','2   ')   THEN
         '38'
         WHEN lobster_district IN ('3' ,'3   ')  THEN
         '35'
         WHEN lobster_district IN ('5A','5A  ')  THEN
         '32'
         WHEN lobster_district IN ('5B','5B  ')  THEN
         '31A'
         WHEN lobster_district IN ('5B1','5B1 ') THEN
         '31B'
         WHEN lobster_district IN ('6A','6A  ')  THEN
         '30'
         WHEN lobster_district IN ('6B','6B  ')  THEN
         '27'
         WHEN lobster_district IN ('4A','4A  ')  THEN
         '34'
         WHEN lobster_district IN ('4B','4B  ')  THEN
         '33'
         WHEN lobster_district IN ('7A','7A  ')  THEN
         '29'
         WHEN lobster_district IN ('7A1','7A1 ') THEN
         '28'
         WHEN lobster_district IN ('A','A   ') THEN
         '41' else 'NA' end LFA
FROM
    (
        SELECT concat(19,substr(trip_code,7,2)) year,
           a.slip_code, trip_code,
            t_price_per_unit,
            t_landed_quantity,
            port_landed,
            cfv_no,
            lobster_district, LICENCE_NO
        FROM
            cl.slip_detail_1997 a,
            cl.slip_header_1997 b
        WHERE
                a.slip_code = b.slip_code
            AND a.species_id LIKE '700%'
        UNION ALL
     
        SELECT
        substr(trip_code,7,4) year,
           a.slip_code, trip_code,
            t_price_per_unit,
            t_landed_quantity,
            port_landed,
            cfv_no,
            lobster_district, LICENCE_NO
        FROM
            cl.slip_detail_1998 a,
            cl.slip_header_1998 b
        WHERE
                a.slip_code = b.slip_code
            AND a.species_id LIKE '700%'
        UNION ALL
        SELECT
        substr(trip_code,7,4) year,
           a.slip_code, trip_code,
            t_price_per_unit,
            t_landed_quantity,
            port_landed,
            cfv_no,
            lobster_district, LICENCE_NO
        FROM
            cl.slip_detail_1999 a,
            cl.slip_header_1999 b
        WHERE
                a.slip_code = b.slip_code
            AND a.species_id LIKE '700%'
        UNION ALL
        SELECT
        substr(trip_code,7,4) year,
            a.slip_code,trip_code,
            t_price_per_unit,
            t_landed_quantity,
            port_landed,
            cfv_no,
            lobster_district, LICENCE_NO
        FROM
            cl.slip_detail_2000 a,
            cl.slip_header_2000 b
        WHERE
                a.slip_code = b.slip_code
            AND a.species_id LIKE '700%'
        UNION ALL
        SELECT
        substr(trip_code,7,4) year,
            a.slip_code,trip_code,
            t_price_per_unit,
            t_landed_quantity,
            port_landed,
            cfv_no,
            lobster_district, LICENCE_NO
        FROM
            cl.slip_detail_2001 a,
            cl.slip_header_2001 b
        WHERE
                a.slip_code = b.slip_code
            AND a.species_id LIKE '700%'
    ) a 
    left join frailc.lob_id b
    on a.LICENCE_NO=b.LICENCE_NO
")
         vsM$Date = as.Date(substr(vsM$TRIP_CODE,7,14),"%Y%m%d")
         i=which(vsM$YEAR=='1997')
        vsM$Date[i] =as.Date(paste('19',substr(vsM$TRIP_CODE[i],7,13),sep=""),"%Y%m%d")
        save(vsM, file=file.path(fnODBC,'slips9801.rdata'))
        return(list(vsO,vsM))
    }
  
  load( file=file.path(fnODBC,'slips7596.rdata'))
  
  load(file=file.path(fnODBC,'slips9801.rdata'))
  return(list(vsO,vsM))
  
}
        
if(DS %in% c('landings_by_vessel','landings_by_vessel.redo')){
  if(grepl('redo',DS)){
    bb = connect.command(con,"
        select season, lfa, to_char(vr_number) vr_number, sum(mt) mt, 'MF' source from (
select case

when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2002-10-01' and '2003-09-30' then 2003
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2003-10-01' and '2004-09-30' then 2004
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2004-10-01' and '2005-09-30' then 2005
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2005-10-01' and '2006-09-30' then 2006
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2006-10-01' and '2007-09-30' then 2007
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2007-10-01' and '2008-09-30' then 2008
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2008-10-01' and '2009-09-30' then 2009
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2009-10-01' and '2010-09-30' then 2010
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2010-10-01' and '2011-09-30' then 2011
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2011-10-01' and '2012-09-30' then 2012
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2012-10-01' and '2013-09-30' then 2013
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2013-10-01' and '2014-09-30' then 2014
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2014-10-01' and '2015-09-30' then 2015
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2015-10-01' and '2016-09-30' then 2016
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2016-10-01' and '2017-09-30' then 2017
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2017-10-01' and '2018-09-30' then 2018
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2018-10-01' and '2019-09-30' then 2019
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2019-10-01' and '2020-09-30' then 2020
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2020-10-01' and '2021-09-30' then 2021
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2021-10-01' and '2022-09-30' then 2022
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2022-10-01' and '2023-09-30' then 2023
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2023-10-01' and '2024-09-30' then 2024
when LFA IN ('35','36','38') AND to_char(date_landed,'YYYY-MM-DD') between '2024-10-01' and '2025-09-30' then 2025
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2002-11-01' and '2003-10-31' then 2003
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2003-11-01' and '2004-10-31' then 2004
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2004-11-01' and '2005-10-31' then 2005
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2005-11-01' and '2006-10-31' then 2006
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2006-11-01' and '2007-10-31' then 2007
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2007-11-01' and '2008-10-31' then 2008
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2008-11-01' and '2009-10-31' then 2009
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2009-11-01' and '2010-10-31' then 2010
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2010-11-01' and '2011-10-31' then 2011
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2011-11-01' and '2012-10-31' then 2012
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2012-11-01' and '2013-10-31' then 2013
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2013-11-01' and '2014-10-31' then 2014
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2014-11-01' and '2015-10-31' then 2015
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2015-11-01' and '2016-10-31' then 2016
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2016-11-01' and '2017-10-31' then 2017
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2017-11-01' and '2018-10-31' then 2018
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2018-11-01' and '2019-10-31' then 2019
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2019-11-01' and '2020-10-31' then 2020
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2020-11-01' and '2021-10-31' then 2021
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2021-11-01' and '2022-10-31' then 2022
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2022-11-01' and '2023-10-31' then 2023
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2023-11-01' and '2024-10-31' then 2024
when LFA IN ('33','34') AND to_char(date_landed,'YYYY-MM-DD') between '2024-11-01' and '2025-10-31' then 2025
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2002-04-01' and '2002-12-31' then 2002
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2003-01-01' and '2003-12-31' then 2003
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2004-01-01' and '2004-12-31' then 2004
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2005-01-01' and '2005-12-31' then 2005
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2006-01-01' and '2006-12-31' then 2006
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2007-01-01' and '2007-12-31' then 2007
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2008-01-01' and '2008-12-31' then 2008
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2009-01-01' and '2009-12-31' then 2009
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2010-01-01' and '2010-12-31' then 2010
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2011-01-01' and '2011-12-31' then 2011
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2012-01-01' and '2012-12-31' then 2012
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2013-01-01' and '2013-12-31' then 2013
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2014-01-01' and '2014-12-31' then 2014
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2015-01-01' and '2015-12-31' then 2015
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2016-01-01' and '2016-12-31' then 2016
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2017-01-01' and '2017-12-31' then 2017
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2018-01-01' and '2018-12-31' then 2018
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2019-01-01' and '2019-12-31' then 2019
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2020-01-01' and '2020-12-31' then 2020
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2021-01-01' and '2021-12-31' then 2021
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2022-01-01' and '2022-12-31' then 2022
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2023-01-01' and '2023-12-31' then 2023
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2024-01-01' and '2024-12-31' then 2024
when LFA IN ('27','28','29','30','31A','31B','32') AND to_char(date_landed,'YYYY-MM-DD') between '2025-01-01' and '2025-12-31' then 2025
else null
end season,
lfa, vr_number, slip_weight_lbs/2.2046/1000 mt
from marfissci.lobster_sd_slip
where to_char(date_landed,'YYYY-MM-DD') between '2002-04-01' and '2025-12-31'
and species_code = 700
) where season is not null
group by season, lfa, to_char(vr_number)")
aa = connect.command(con, paste("
select VR_NUMBER, SEASON,sum(MT) MT, '38B' LFA, 'GreySlip' source 
from (
select VR_NUMBER, slip_weight_lbs/2.2046/1000 MT, landing_date_time, CASE
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2001-10-01' and '2002-09-30' then 2002
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2002-10-01' and '2003-09-30' then 2003
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2003-10-01' and '2004-09-30' then 2004
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2004-10-01' and '2005-09-30' then 2005
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2005-10-01' and '2006-09-30' then 2006
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2006-10-01' and '2007-09-30' then 2007
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2007-10-01' and '2008-09-30' then 2008
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2008-10-01' and '2009-09-30' then 2009
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2009-10-01' and '2010-09-30' then 2010
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2010-10-01' and '2011-09-30' then 2011
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2011-10-01' and '2012-09-30' then 2012
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2012-10-01' and '2013-09-30' then 2013
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2013-10-01' and '2014-09-30' then 2014
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2014-10-01' and '2015-09-30' then 2015
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2015-10-01' and '2016-09-30' then 2016
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2016-10-01' and '2017-09-30' then 2017
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2017-10-01' and '2018-09-30' then 2018
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2018-10-01' and '2019-09-30' then 2019
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2019-10-01' and '2020-09-30' then 2020
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2020-10-01' and '2021-09-30' then 2021
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2021-10-01' and '2022-09-30' then 2022
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2022-10-01' and '2023-09-30' then 2023
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2023-10-01' and '2024-09-30' then 2024
when to_char(LANDING_DATE_TIME,'YYYY-MM-DD') between '2024-10-01' and '2025-09-30' then 2025
else null
end season

from marfissci.lobster_md_slip a where mon_doc_defn_id in (46,49)

)
group by VR_NUMBER, season"))
  
  
cc = connect.command(con, paste("	
  select season, cfv_number vr_number, sum(live_wt) MT, lfa, 'IC' source from (	
    SELECT	
    case	
when b.lfa in ('35','36','38') and a.land_date between to_date('1989-10-01','YYYY-MM-DD') and to_date('1990-09-30','YYYY-MM-DD')    then 1990
when b.lfa in ('35','36','38') and a.land_date between to_date('1990-10-01','YYYY-MM-DD') and to_date('1991-09-30','YYYY-MM-DD')    then 1991
when b.lfa in ('35','36','38') and a.land_date between to_date('1991-10-01','YYYY-MM-DD') and to_date('1992-09-30','YYYY-MM-DD')    then 1992
when b.lfa in ('35','36','38') and a.land_date between to_date('1992-10-01','YYYY-MM-DD') and to_date('1993-09-30','YYYY-MM-DD')    then 1993
when b.lfa in ('35','36','38') and a.land_date between to_date('1993-10-01','YYYY-MM-DD') and to_date('1994-09-30','YYYY-MM-DD')    then 1994
when b.lfa in ('35','36','38') and a.land_date between to_date('1994-10-01','YYYY-MM-DD') and to_date('1995-09-30','YYYY-MM-DD')    then 1995
when b.lfa in ('35','36','38') and a.land_date between to_date('1995-10-01','YYYY-MM-DD') and to_date('1996-09-30','YYYY-MM-DD')    then 1996
when b.lfa in ('33','34') and a.land_date between to_date('1989-11-01','YYYY-MM-DD') and to_date('1990-10-31','YYYY-MM-DD') then 1990
when b.lfa in ('33','34') and a.land_date between to_date('1990-11-01','YYYY-MM-DD') and to_date('1991-10-31','YYYY-MM-DD') then 1991
when b.lfa in ('33','34') and a.land_date between to_date('1991-11-01','YYYY-MM-DD') and to_date('1992-10-31','YYYY-MM-DD') then 1992
when b.lfa in ('33','34') and a.land_date between to_date('1992-11-01','YYYY-MM-DD') and to_date('1993-10-31','YYYY-MM-DD') then 1993
when b.lfa in ('33','34') and a.land_date between to_date('1993-11-01','YYYY-MM-DD') and to_date('1994-10-31','YYYY-MM-DD') then 1994
when b.lfa in ('33','34') and a.land_date between to_date('1994-11-01','YYYY-MM-DD') and to_date('1995-10-31','YYYY-MM-DD') then 1995
when b.lfa in ('33','34') and a.land_date between to_date('1995-11-01','YYYY-MM-DD') and to_date('1996-10-31','YYYY-MM-DD') then 1996
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1990-01-01','YYYY-MM-DD') and to_date('1990-12-31','YYYY-MM-DD')  then 1990
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1991-01-01','YYYY-MM-DD') and to_date('1991-12-31','YYYY-MM-DD')  then 1991
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1992-01-01','YYYY-MM-DD') and to_date('1992-12-31','YYYY-MM-DD')  then 1992
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1993-01-01','YYYY-MM-DD') and to_date('1993-12-31','YYYY-MM-DD')  then 1993
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1994-01-01','YYYY-MM-DD') and to_date('1994-12-31','YYYY-MM-DD')  then 1994
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1995-01-01','YYYY-MM-DD') and to_date('1995-12-31','YYYY-MM-DD')  then 1995
when b.lfa in ('27','28','29','30','31A','31B','32') and a.land_date between to_date('1996-01-01','YYYY-MM-DD') and to_date('1996-12-31','YYYY-MM-DD')  then 1996
     else null	
    end season,	
    a.cfv_number,	
    a.live_wt,	
    a.land_prov_code	
    || a.land_stat_dist_code	
    || a.land_community_code,	
    b.lfa	
    FROM	
    cl.all_identified_catches_view   a,	
    frailc.marfis_ports              b	
    WHERE	
    a.land_prov_code	
    || a.land_stat_dist_code	
    || a.land_community_code = b.community_code (+)	
    AND a.species_code = 700	
    AND a.land_prov_code IN (1,2)	
    and a.land_date between to_date('1989-10-01','YYYY-MM-DD') and to_date('1996-08-31','YYYY-MM-DD')	
     AND B.LFA IN ('27','28','29','30','31A','31B','32','33','34','35','36','38')
  )	
  where season is not null
  and  cfv_number !='000000' 	
  group by season, cfv_number, lfa	
  "))
   

dd = connect.command(con,paste("
select * from lobster.landings_per_vessel_1997_2002
	"))

ee = do.call(rbind,list(aa,bb,cc,dd))

names(ee) = c('VR_NUMBER','SYEAR', 'MT','LFA','DATASOURCE')
saveRDS(ee,file=file.path(fnODBC,'land_by_vess.rdata'))
   }
ee = readRDS(file=file.path(fnODBC,'land_by_vess.rdata'))
  return(ee)
}
if(DS %in% c('slips','slips.redo')){
    if(grepl('redo',DS)) {
      #current
      vsP = connect.command(con,"select * from MARFISSCI.LOBSTER_SD_SLIP")
      save(vsP, file=file.path(fnODBC,'slips.rdata'))
      return(vsP)
    }
    load(file=file.path(fnODBC,'slips.rdata'))
    return(vsP)
}
    
if(DS %in% c('process_slips', 'process_slips.redo')){
  if(grepl('redo',DS)) {
    sl = lobster.db('slips')
    sl = subset(sl, SPECIES_CODE==700 & NIL_REPORT_FLAG=='N') 
    sl$LFA = ifelse(sl$LICENCE_ID=='027050',27,sl$LFA)
    sl$DYR = lubridate::decimal_date(as.Date(sl$DATE_LANDED)) - lubridate::year(as.Date(sl$DATE_LANDED))
    sl$WYR = ceiling(sl$DYR*52)
    sl$DWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + sl$WYR/52
    sl$MWYR = lubridate::year(as.Date(sl$DATE_LANDED)) + ceiling(sl$DYR*12)/12
    sl$YR = lubridate::year(as.Date(sl$DATE_LANDED))
    ql = quantile(sl$PRICE,c(.04,0.9999),na.rm=T)
    sl$PRICE = ifelse(sl$PRICE>=ql[1] & sl$PRICE<=ql[2],sl$PRICE,NA)
    price.data = aggregate(PRICE~DWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T))) #price per week of year 
    price.data2 = aggregate(PRICE~MWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
    price.data3 = aggregate(PRICE~YR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
    
    sll = bio.utilities::fillNaDf2(sl, price.data, mergeCols=c('DWYR','LFA'),fillCols=c('PRICE'))  #reduces to .03%missing
    slll = bio.utilities::fillNaDf2(sll, price.data2, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))#reduces to .01%missing
    sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))#reduces to .005%missing
    vsP = sllll
    
    
    vsC = lobster.db('old_slips')
    sl = vsC[[1]]
    sl$DYR = lubridate::decimal_date(as.Date(sl$DA)) - lubridate::year(as.Date(sl$DA))
    sl$WYR = ceiling(sl$DYR*52)
    sl$DWYR = lubridate::year(as.Date(sl$DA)) + sl$WYR/52
    sl$MWYR = lubridate::year(as.Date(sl$DA)) + ceiling(sl$DYR*12)/12
    sl$YR = lubridate::year(as.Date(sl$DA))
    ql = quantile(sl$PRICE,c(.04,0.9999),na.rm=T)
    sl$PRICE = ifelse(sl$PRICE>=ql[1] & sl$PRICE<=ql[2],sl$PRICE,NA)
    price.data = aggregate(PRICE~DWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T))) #price per week of year 
    price.data2 = aggregate(PRICE~MWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
    price.data3 = aggregate(PRICE~YR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
     #only 4% missing at the start
    sll = bio.utilities::fillNaDf2(sl, price.data, mergeCols=c('DWYR','LFA'),fillCols=c('PRICE'))  #reduces to .008%missing
    slll = bio.utilities::fillNaDf2(sll, price.data2, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))#reduces to .007%missing
    sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))#reduces to .005%missing
    #lfa41 in 1975 is the only one missing I am filling in from LFA 33 for that year
    price.data4 = subset(price.data2,LFA==33 & floor(MWYR)==1975)
    price.data4$LFA=41
    price.data5 = subset(price.data3,LFA==33 & YR==1975 )
    price.data5$LFA=41
    slllll = bio.utilities::fillNaDf2(sllll, price.data4, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))#reduces to .007%missing
    sllllll = bio.utilities::fillNaDf2(slllll, price.data5, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))#reduces to .007%missing
    vsO = subset(sllllll,!is.na(PRICE))
    
    sl = vsC[[2]] #this is rolled up to monthly slips so is the best we can do
    sl$DYR = lubridate::decimal_date(as.Date(sl$Date)) - lubridate::year(as.Date(sl$Date))
    sl$WYR = ceiling(sl$DYR*52)
    sl$DWYR = lubridate::year(as.Date(sl$Date)) + sl$WYR/52
    sl$MWYR = lubridate::year(as.Date(sl$Date)) + ceiling(sl$DYR*12)/12
    sl$YR = lubridate::year(as.Date(sl$Date))
    ql = quantile(sl$PRICE,c(.025,0.9999),na.rm=T)
    sl$PRICE = ifelse(sl$PRICE>=ql[1] & sl$PRICE<=ql[2],sl$PRICE,NA)
    price.data2 = aggregate(PRICE~MWYR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
    price.data3 = aggregate(PRICE~YR+LFA, data=sl, FUN=function(x) c(mean(x,na.rm=T)))
    price.data4 = subset(price.data2,LFA==33 & floor(MWYR)==2000)
    price.data4$LFA=41
    price.data5 = subset(price.data3,LFA==33 & YR==2000 )
    price.data5$LFA=41
    
    #only 2% missing at the start
    slll = bio.utilities::fillNaDf2(sl, price.data2, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))#reduces to 1 record missing
    sllll = bio.utilities::fillNaDf2(slll, price.data3, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))#reduces to .005%missing
    slllll = bio.utilities::fillNaDf2(sllll, price.data4, mergeCols=c('MWYR','LFA'),fillCols=c('PRICE'))#reduces to .007%missing
    sllllll = bio.utilities::fillNaDf2(slllll, price.data5, mergeCols=c('YR','LFA'),fillCols=c('PRICE'))#reduces to .007%missing
    
    vsM = sllllll
    
    vsP = subset(vsP,select=c(LFA,DATE_LANDED,LICENCE_ID,SLIP_WEIGHT_LBS,PRICE,VR_NUMBER)) # no port so removed from others
    vsM = subset(vsM,select=c(LFA,Date,LICENCE_NO,WT_LBS,PRICE,CFV_NO))
    vsO = subset(vsO,select=c(LFA,DA,BOATVESID,WT_LBS,PRICE,BOATVESID))
    
    names(vsP) = names(vsO) = names(vsM)
    vsP$ID = 'marfis'
    vsM$ID = 'ziff'
    vsO$ID = 'old'
    tt = do.call(rbind,list(vsP,vsO,vsM))
    
    tt$YR = lubridate::year(tt$Date)
    tt$MN = lubridate::month(tt$Date)
    tt$SYEAR = tt$YR
    tt$SYEAR = ifelse(tt$LFA %in% c(33,34,36,38 ) & tt$MN %in% c(11,12),tt$SYEAR+1,tt$SYEAR)
    tt$SYEAR = ifelse(tt$LFA %in% c(35 ) & tt$MN %in% c(10,11,12),tt$SYEAR+1,tt$SYEAR)
    tt = subset(tt,LFA %ni% c('24','7B','NA'))
    tt = subset(tt, !is.na(LFA))
    i = which(tt$LFA %in% c('31A','31B') & tt$SYEAR<1984)
    tt = tt[-i,]
    i = which(tt$LFA %in% c('31A') & tt$SYEAR<1993)
    tt = tt[-i,]
    i = which(tt$LFA %in% c('36' , '38',32) & tt$SYEAR<1979)
    tt = tt[-i,]
    tt = subset(tt,SYEAR %ni% c(1974,1975))
    
    tt = subset(tt,LFA %ni% '31_32')
    tt = subset(tt,SYEAR %ni% c(1997,1974,1975))
    
    ta = aggregate(WT_LBS/2.2046/1000~LFA+SYEAR,data=tt,FUN=sum)
    names(ta)[3]='T'
    db.setup(un=oracle.lobster.user,pw=oracle.lobster.password)
    gu = connect.command(con,'select * from frailc.gulf_land')
    tag = merge(ta,subset(gu,YR>2001),by.x=c('SYEAR','LFA'),by.y=c('YR','LFA'),all=T)
    tag$T = apply(tag[,c('T','MT')],1,FUN=sum,na.rm=T)
    tag$MT = NULL
    
    #this section is to get the ratios of landings from these slips to cheryls correct
    b = lobster.db('seasonal.landings')
    b$LFA38B <- NULL
    b$SYEAR = as.numeric(substr(b$SYEAR,6,9))
    nb = nrow(b)
    b = reshape(b,idvar='SYEAR', varying=list(2:6),direction='long')
    b$LFA=rep(c(33,34,35,36,38),each=nb)
    b$time <- NULL
    names(b)[1:2]=c('SYEAR','SlipLand')
    
    
    d = lobster.db('annual.landings')
    d = subset(d,select=c(YR,LFA27,LFA28,LFA29,LFA30,LFA31A,LFA31B,LFA32))
    nd = nrow(d)
    d = reshape(d,idvar='YR', varying=list(2:8),direction='long')
    d$LFA=rep(c(27,28,29,30,'31A','31B',32),each=nd)
    d$time <- NULL
    names(d)[1:2]=c('SYEAR','SlipLand')
    bd = rbind(d,b)
    tad = merge(tag,bd,all.x=T)
    
    tad$SlipLand = ifelse(is.na(tad$SlipLand),tad$T,tad$SlipLand)
    tad$SlipLand = ifelse(tad$SlipLand==0,tad$T,tad$SlipLand)
    tad$ratio = tad$T/tad$SlipLand
    
    tt = merge(tt,tad[,c('SYEAR','LFA','ratio')],all.x=T)
    
    tt$adj_wt_kg = tt$WT_LBS/2.2046*tt$ratio
    tt$value = tt$WT_LBS*tt$ratio*tt$PRICE
    
   # 
  #  aggs = aggregate(cbind(value,adj_wt_kg)~SYEAR+LFA,data=tt,FUN=sum)
  #  aggs$millions = aggs$value/1e6
  #  aggs$t = aggs$adj_wt_kg/1e3
  #  ggplot(subset(aggs,LFA==34),aes(x=SYEAR,y=millions))+geom_bar(stat='identity')+facet_wrap(~LFA,scales='free_y')
  
   # d = subset(aggs,LFA==34 & SYEAR<2024)
    #co = (mean(d$t)/mean(d$millions))*.75
     # ggplot(d,aes(x=SYEAR))+
      #geom_bar(aes(y=t),stat='identity')+
      #geom_line(aes(y=millions*co),color='red',linewidth=2 )+
      #  scale_y_continuous(
      #name='Landings (t)',
      #sec.axis = sec_axis(trans=~./co,name='Value (Millions)')
  
saveRDS(tt,file=file.path(fnODBC,'slips_processed.rds'))
return(tt)
  } 
      
  tt = readRDS(file.path(fnODBC,'slips_processed.rds'))
return(tt)  
  }
  
if(DS %in% c('vessels.by.port','vessels.by.port.redo')){
  print('Lobster Vessels by LFA, Port and Year--NOTE there are duplicates with multiple ports per VRN per year')
  if(grepl('redo',DS)) {

    vsP = connect.command(con,"SELECT DISTINCT
                a.vr_number,
                a.lfa,
                (a.community_code) port,
                TO_CHAR(date_fished, 'yyyy') yr_fished,
                b.year_built,
                b.gross_tonnage,
                b.bhp,
                b.loa,
                b.breadth,
                b.depth
            FROM
                marfissci.lobster_sd_log   a,
                marfissci.vessels          b
            WHERE
                a.vr_number = b.vr_number
            ")
  save(vsP, file=file.path(fnODBC,'vessels_port.rdata'))
  return(vsP)
  }
  load(file=file.path(fnODBC,'vessels_port.rdata'))
  return(vsP)

}


if(DS %in% 'civi'){

  load(file.path(fn.root,'CIVI','CIVI.rdata'))
  return(civi)

}



if(DS %in% c('port_location','port_location.redo')){
      if(DS == 'port_location') {
        load(file=file.path(fnODBC,'port_locs.rdata'))
        return(port_locs)
      }
      #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
      port_locs = connect.command(con,'select * from frailc.port_locs')
      save(port_locs,file=file.path(fnODBC,'port_locs.rdata'))
    }

if(DS %in% c('port','port.redo')){
        if(DS == 'port') {
                  load(file=file.path(fnODBC,'ports.rdata'))
                return(ports)
          }
                  #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  ports = connect.command(con,'select * from LOBSTER.port')
                  save(ports,file=file.path(fnODBC,'ports.rdata'))
        }

if(DS %in% c('community_code','community_code.redo')){
      if(DS == 'community_code') {
        load(file=file.path(fnODBC,'community_code.rdata'))
        return(ports)
      }
      #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
      ports = connect.command(con,'select * from MARFISSCI.COMMUNITIES')
      save(ports,file=file.path(fnODBC,'community_code.rdata'))
    }


if(DS %in% c('atSea.logbook.link','atSea.logbook.link.redo')){
        if(DS == 'atSea.logbook.link') {
                  load(file=file.path(fnODBC,'atSea.logbook.link.rdata'))
                return(links)
          }
                  #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  links = connect.command(con,'select * from LOBSTER.ATSEA_LOG_LINK')
                  save(links,file=file.path(fnODBC,'atSea.logbook.link.rdata'))
        }

    if(DS %in% c('temperature.data.redo', 'temperature.data')){
      if(DS == 'temperature.data') {
        load(file=file.path(fnODBC,'temperature.rdata'))
        return(TempData)
      }
      #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
      TempData = connect.command(con,"select * from SNOWCRAB.SC_TEMP_MERGE")
      save(TempData,file=file.path(fnODBC,'temperature.rdata'))
    }

    if(DS %in% c('bathymetry')){
        load(file=file.path(project.datadirectory('bio.lobster'),'bathymetry','bathymetry.complete.canada.east.rdata'))
        return(Z)
      }

if(DS %in% c('historic.cpue.redo', 'historic.cpue')){
      if(DS == 'historic.cpue') {
                  load(file=file.path(fnODBC,'historic.cpue.rdata'))
                return(hcpue)
          }
                   #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  hcpue = connect.command(con,"select a.port, sdate, to_char(sdate,'yyyy') year, lfa, portname, lbsptrap from lobster.histcatch a, lobster.port b where
a.port = b.port")
                  hcpue$SYEAR = year(hcpue$SDATE)
                  hcpue$MONTH = month(hcpue$SDATE)
                  ii = which(hcpue$MONTH>8)
                  hcpue$SYEAR[ii] = hcpue$SYEAR[ii]+1
                  dos1 = aggregate(SDATE~SYEAR+LFA,data=hcpue,FUN=min)
                  names(dos1)[3] = 'D1'
                  hcpue = merge(hcpue,dos1,all.x=T)
                  hcpue$DOS = as.numeric((hcpue$SDATE-hcpue$D1)/(60*60*24))
                  save(hcpue,file=file.path(fnODBC,'historic.cpue.rdata'))
          }


if(DS %in% c('historic.landings.redo', 'historic.landings')){
      if(DS == 'historic.landings') {
                  load(file=file.path(fnODBC,'historic.landings.rdata'))
                return(hland)
          }
                   #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  hland = connect.command(con,"select * from lobster.historical_lobdist_land")
                  save(hland,file=file.path(fnODBC,'historic.landings.rdata'))
          }



if(DS %in% c('landings.by.port.redo','landings.by.port')) {

  if(DS == 'landings.by.port.redo') {

     #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                          #1975 - 1996 sales slips
                          oldd = connect.command(con,paste("SELECT date_landed da, prov_code||district||port_code port, cfv boatvesid, wt_lbs, lfa FROM
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
                          midd = connect.command(con,paste("SELECT date_fished da,  port_landed port,  licence_id boatvesid,  weight_lbs wt_lbs,  lfa FROM lobster.LOB_LOG_EST_1997_2001"))

                        #2002 - current
                            newd = connect.command(con,paste("SELECT date_fished da,  community_code port ,  licence_id boatvesid,  NVL(weight_lbs,0)+NVL(weight_lbs_b,0)+NVL(weight_lbs_c,0) wt_lbs,  lfa FROM marfissci.lobster_sd_log"))

                          dats = rbind(oldd,midd,newd)
                          dats = subset(dats,LFA<41)
                          dats = addSYEAR(dats,'DA')
                          season.dates = lobster.db('season.dates')

                    #    dats$WOS = NA
                    #    lfa = unique(dats$LFA)
                    #        for(i in 1:length(lfa)) {
                    #              h  = season.dates[season.dates$LFA==lfa[i],]
                    #           for(j in unique(dats$SYEAR[dats$LFA==lfa[i]])){
                    #               dats$WOS[dats$LFA==lfa[i] & dats$SYEAR==j] = floor(as.numeric(dats$SDATE[dats$LFA==lfa[i] & dats$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                    #            }
                    #      }

                        dats$WEIGHT_KG = dats$WT_LBS*0.453592
                    #  if(any(!is.finite(dats$WOS))) {kl = which(!is.finite(dats$WOS)); dats$WOS[kl] = NA}
                    #  dats = aggregate(WEIGHT_KG~PORT+SDATE+WOS+SYEAR+LFA,data=dats,FUN=sum)
                    #  dats = subset(dats,WOS>0)
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
      #proportion of old by grid using proportions of landings by WOS and Community into grids using logs
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

                  #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  annual.landings = connect.command(con,'select * from LOBSTER.SLIP_LAND_ANNUAL')
                  print('make sure to check with Cheryl.Denton@dfo-mpo.gc.ca on last update')

                  save(annual.landings,file=file.path(fnODBC,'annual.landings.rdata'))
            }

if(DS %in% c('seasonal.landings','seasonal.landings.redo')) {
          if(DS == 'seasonal.landings') {
                load(file=file.path(fnODBC,'seasonal.landings.rdata'))
                return(seasonal.landings)
                  }

                  #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  seasonal.landings = connect.command(con,'select * from LOBSTER.SLIP_LAND_SEASONAL')
                  seasonal.landings = seasonal.landings[order(seasonal.landings$SYEAR),]
                   print('Last two years of landings data may be incomplete, make sure to check with Cheryl.Denton@dfo-mpo.gc.ca on last update')
                  print('LFA27 for needs to have gulf landings added manually each year from GFIS.....')
                  save(seasonal.landings,file=file.path(fnODBC,'seasonal.landings.rdata'))
            }


if(DS %in% c('historical.landings','historical.landings.redo')) {
          if(DS == 'historical.landings') {
                load(file=file.path(fnODBC,'historical.landings.rdata'))
                return(historical.landings)
                  }

                historical.landings = read.delim(file.path(project.datadirectory('bio.lobster'),"data","inputs","LFA34_Landings_1892-2004.txt"))

                  ##con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
                  #historical.landings = connect.command(con,'select * from LOBSTER.SLIP_LAND_HISTORICAL')
                  save(historical.landings,file=file.path(fnODBC,'historical.landings.rdata'))
            }

if(DS %in% c('season.dates','season.dates.redo')) {
          if(DS == 'season.dates') {
                load(file=file.path(fnODBC,'season.dates.rdata'))
                return(season.dates)
                  }
                  #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

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

                    #Fish.Date = lobster.db('season.dates')
                    Fish.Date = season.dates = connect.command(con,'select * from LOBSTER.FISHING_SEASONS')
                    print('Lobster.Fishing_Seasons needs to be updated in SQL if you want to use this season dates script--these dates come from FAM.')
                    season.dates = backFillSeasonDates(Fish.Date,eyr=year(Sys.time())-1)
                    print(paste0("Maximum Season SYEAR in season.dates is ", max(season.dates$SYEAR)))
                  save(season.dates,file=file.path(fnODBC,'season.dates.rdata'))
            }




### Inshore Commercial Logs and slips
if (DS %in% c("logs.redo", "logs") ) {

           if (DS=="logs.redo") {
              require(RODBC)
             #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
             if (is.null(pH$yr)){
              # logs
               logs = connect.command(con, "select * from marfissci.lobster_sd_log")
              save( logs, file=file.path( fnODBC, "logs.rdata"), compress=T)

              # slips
              slips = connect.command(con, "select * from marfissci.lobster_sd_slip")
              save( slips, file=file.path( fnODBC, "slip.rdata"), compress=T)
              gc()  # garbage collection

              # old logs LFA 34
              dd = dir(fnODBC)
             if(!any("oldlogs34.rdata" %in% dd)){
             oldlogs34 = connect.command(con, "select * from lobster.lobster_log_data")
             save( oldlogs34, file=file.path( fnODBC, "oldlogs34.rdata"), compress=T)
              gc()  # garbage collection
              #odbcClose(con)
              }
              }
             if(!is.null(pH$yr)){
               
               load (file.path( fnODBC, "slip.rdata"), .GlobalEnv)
               load (file.path( fnODBC, "logs.rdata"), .GlobalEnv)
                yrs = c(pH$yr-1,pH$yr)
              print(paste('this is just updating ',paste(yrs,collapse=',')))
              logs = subset(logs,lubridate::year(DATE_FISHED) %ni% yrs )
              slips = subset(slips,lubridate::year(DATE_LANDED) %ni% yrs )
           
              logss = connect.command(con, paste("select * from marfissci.lobster_sd_log where to_char(date_fished,'yyyy') IN (",paste(yrs,collapse=','),")",sep=""))
              logs = as.data.frame(rbind(logs,logss))
              save( logs, file=file.path( fnODBC, "logs.rdata"), compress=T)
             
              slipss = connect.command(con, paste("select * from marfissci.lobster_sd_slip where to_char(date_landed,'yyyy') IN (",paste(yrs,collapse=','),")",sep=""))
              slips = as.data.frame(rbind(slips,slipss))
              save( slips, file=file.path( fnODBC, "slip.rdata"), compress=T)
              gc()  # garbage collection
              }
           }
    
            load (file.path( fnODBC, "slip.rdata"), .GlobalEnv)
            load (file.path( fnODBC, "logs.rdata"), .GlobalEnv)
            load (file.path( fnODBC, "oldlogs34.rdata"), .GlobalEnv)
            print("Three files loaded called 'slips', 'logs' and 'oldlogs34" )

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
                

                          #lfa "27"  "28"  "29"  "30"  "31A" "31B" "32"  "33"  "34"  "35"  "36"  "38"

                          max_trap = c(825,750,750,750,750,750,750,750,1126,1126,1126,1226)
                          #max_lbs = c(2750,2750,2750,2750,2750,2750,2750,10000,30000,30000,30000,30000)
                          Fish.Date$START_DATE = as.Date(Fish.Date$START_DATE)#,"%d/%m/%Y")
                          Fish.Date$END_DATE = as.Date(Fish.Date$END_DATE)#,"%d/%m/%Y")


                    # imported logs from marfis
                          lobster.db('logs')

                         oldlogs34$LFA34_WEIGHT1_KGS=oldlogs34$LFA34_WEIGHT1_KGS/0.4536
                         oldlogs34$LFA34_WEIGHT2_KGS=oldlogs34$LFA34_WEIGHT2_KGS/0.4536
                          oldlogs34=subset(oldlogs34,select=c("VR_NUMBER","LICENCE_NO","LOBSTER_AREA","TRIP_ID","DATE_FISHED","GRID_NUMBER_A","LFA34_WEIGHT1_KGS","TRAP_HAULS_GRID_A","GRID_NUMBER_B","LFA34_WEIGHT2_KGS","TRAP_HAULS_GRID_B","V_NOTCHED","PORT_LANDED"))
                          names(oldlogs34)=c("VR_NUMBER","LICENCE_ID","LFA","SD_LOG_ID","DATE_FISHED","GRID_NUM","WEIGHT_LBS","NUM_OF_TRAPS","GRID_NUM_B","WEIGHT_LBS_B","NUM_OF_TRAPS_B","V_NOTCHED","PORT_LANDED")
                          logs=merge(logs,oldlogs34,all=T)


                          logs$TOTAL_NUM_TRAPS = rowSums(logs[c('NUM_OF_TRAPS','NUM_OF_TRAPS_B','NUM_OF_TRAPS_C')],na.rm=T)
                          logs$TOTAL_WEIGHT_LBS = rowSums(logs[c('WEIGHT_LBS','WEIGHT_LBS_B','WEIGHT_LBS_C')],na.rm=T)
                          logs$TOTAL_WEIGHT_KG = logs$TOTAL_WEIGHT_LBS*0.4536

                    # select for records within season
                          logs$DATE_FISHED = as.Date(logs$DATE_FISHED,"%Y-%m-%d", tz="UTC" )
                          #logs$SYEAR = year(logs$DATE_FISHED)

                        for(i in 1:length(lfa)) {
                                h  =  Fish.Date[Fish.Date$LFA==lfa[i],]
                            for(j in 1:nrow(h)) {
                                logs$SYEAR[logs$LFA==lfa[i]&logs$DATE_FISHED>=h[j,'START_DATE']&logs$DATE_FISHED<=h[j,'END_DATE']] = h[j,'SYEAR']
                                }
                              }
                print('Logs Outside of Season Start and End Dates are Discarded')
                        logs = subset(logs,!is.na(SYEAR))

                    # add week of season (WOS) variable
                        logs$DOS = logs$WOS = NA
                            for(i in 1:length(lfa)) {
                                  h  =  Fish.Date[Fish.Date$LFA==lfa[i],]
                               for(j in unique(logs$SYEAR[logs$LFA==lfa[i]])){
                                   print(c(lfa[i],j))
                                  logs$DOS[logs$SYEAR==j&logs$LFA==lfa[i]]<-logs$DATE_FISHED[logs$SYEAR==j&logs$LFA==lfa[i]]-min(logs$DATE_FISHED[logs$SYEAR==j&logs$LFA==lfa[i]])+1
                                  logs$WOS[logs$LFA==lfa[i]&logs$SYEAR==j] = floor(as.numeric(logs$DATE_FISHED[logs$LFA==lfa[i]&logs$SYEAR==j]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                              }

                    # add quarter
                      logs$quarter = NA
                      logs$quarter[month(logs$DATE_FISHED)%in%1:3] = 1
                      logs$quarter[month(logs$DATE_FISHED)%in%4:6] = 2
                      logs$quarter[month(logs$DATE_FISHED)%in%7:9] = 3
                      logs$quarter[month(logs$DATE_FISHED)%in%10:12] = 4


                    commonCols = c("SUM_DOC_ID", "VR_NUMBER", "VESSEL_NAME", "SUBMITTER_NAME", "LICENCE_ID", "LFA", "COMMUNITY_CODE","SD_LOG_ID", "DATE_FISHED","SYEAR","WOS",'quarter',"TOTAL_NUM_TRAPS","TOTAL_WEIGHT_KG","DOS")

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
                       # yrs = 2002:2018

                        tmpland = subset(TotalLandings,LFA==lfa[i])
                        yrs1 = sort(unique(tmplogs$SYEAR))
                        yrs2 = sort(unique(tmpland$SYEAR))
                        yrs = yrs1[yrs1%in%yrs2]


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

### voluntary logs
    if (DS %in% c("vlog.redo", "vlog") ) {

     if (DS=="vlog.redo") {
        require(RODBC)
        #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

        # vlog
        vlog = connect.command(con, "select a.FDATE,a.N_TRP,a.W_TOT,a.FCODE,a.N_L,a.W_AVG,a.PORT,a.CPTH,a.NBF,a.SEASON,a.W_C,a.CPTH_C, b.LFA,b.COUNTY,b.STAT,b.PORT_CODE,b.LATITUDE,b.LONGITUDE,b.COMMENTS from lobster.CRLOGDATA a, lobster.CRLOCATIONS b where a.port = b.port")
        vlogs34 = connect.command(con, "select * from lobster.logdata_other")
        save( vlog, file=file.path( fnODBC, "vlog.rdata"), compress=T)
        save( vlogs34, file=file.path( fnODBC, "vlogs34.rdata"), compress=T)

        gc()  # garbage collection
        #odbcClose(con)
      }
      load(file.path( fnODBC, "vlog.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "vlogs34.rdata"), .GlobalEnv)
     }


    if (DS %in% c("process.vlog.redo", "process.vlog") ) {

     if (DS=="process.vlog.redo") {
          load(file.path( fnODBC, "vlog.rdata"), .GlobalEnv)
          load(file.path( fnODBC, "vlogs34.rdata"), .GlobalEnv)

         vlogs34$PORT[vlogs34$PORT=="ABBOTS HBR."]<-"ABBOTT S HARBOUR"
         vlogs34$PORT[vlogs34$PORT=="BARRINGTON BAY"]<-"BARRINGTON"
         vlogs34$PORT[vlogs34$PORT=="DENNIS PT."]<-"LOWER WEST PUBNICO"
         vlogs34$PORT[vlogs34$PORT=="PT. MAITLAND"]<-"PORT MAITLAND"
         vlogs34$PORT[vlogs34$PORT=="PINKNEY'S PT."]<-"PINKNEY S POINT"
         vlogs34$PORT[vlogs34$PORT=="WOODS HBR."]<-"WOODS HARBOUR"

          Ports = read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","Ports.csv"))
          Prts34 = subset(Ports,LFA==34,c("Port_Code","Port_Name","County","Statistical_District","LFA" ,"centlat" ,"centlon"))
          names(Prts34)=c("PORT_CODE","PORT","COUNTY","STAT","LFA" ,"Y" ,"X")
          vlogs34 = merge(vlogs34,Prts34,all.x=T)

          vlog$X = convert.dd.dddd(vlog$LONGITUDE)*-1
          vlog$Y = convert.dd.dddd(vlog$LATITUDE)



          vlog = merge(vlog,vlogs34,all=T)


          vlog = addSYEAR(vlog,date.field="FDATE")
          #vlog$SYEAR = as.numeric(substr(vlog$SEASON,6,9))
          vlog$W_KG = vlog$W_TOT*0.4536
          vlog$CPUE = vlog$W_KG/vlog$N_TRP


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
        load(file.path( fnODBC, "processed.vlog.rdata"))
        return(vlog)
    }

if (DS %in% c("greyzone_logs.redo", "greyzone_logs") ) {
    #these are the monitoring doc logs exclusively used for 41 and grey zone fishing
      if (DS=="greyzone_logs.redo") {
        query_md = "select * from marfissci.lobster_md_49_log where mon_doc_defn_id=49"
        db.setup(un=oracle.lobster.user, pw = oracle.lobster.password)
        log_md = connect.command(con, query_md)
        #these are both jonah and lobster, subset for lobster
        log_md = subset(log_md,SSF_SPECIES_CODE==700)
        save( log_md, file=file.path( fnODBC, "greyzonelogs.rdata"), compress=T)
        gc()  # garbage collection

        }
      load(file=file.path( fnODBC, "greyzonelogs.rdata"))
      return(log_md)
    }

if (DS %in% c("uslandings_by_state") ) {
    print('data comes from https://www.fisheries.noaa.gov/foss/f?p=215:200:11583616673315:::::')  
      x = read.csv(file=file.path( fn.root, "US_Landings50-23.csv"))
      x <- x %>%
          mutate(across(c(Pounds,Metric.Tons, Dollars), ~as.numeric(gsub(",","",.))))
    return(x)
      }
    

### Offshore Commercial Logs
    if (DS %in% c("logs41.redo", "logs41") ) {

             if (DS=="logs41.redo") {
                require(RODBC)
                #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

                # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
               query41 = "select * from lobster.logs41"
               print('need to refresh lobster.logs41 materialized view')
               db.setup(un=oracle.lobster.user, pw = oracle.lobster.password)

                slipquery41 = "select  * from lobster.slips41"
                ziffquery41  =  "select * from lobster.ziff41"
                offquery41  =  "select * from lobster.crislog41" # table not view

                slip41 = connect.command(con, slipquery41)
                logs41 = connect.command(con, query41)
                ziff41 = connect.command(con, ziffquery41)
                off41 = connect.command(con, offquery41)

                off41 = subset(off41,DATE_FISHED < '1995-01-01')

                save( logs41, file=file.path( fnODBC, "logs41.rdata"), compress=T)
                save( slip41, file=file.path( fnODBC, "slip41.rdata"), compress=T)
                save( ziff41, file=file.path( fnODBC, "ziff41.rdata"), compress=T)
                save( off41, file=file.path( fnODBC, "off41.rdata"), compress=T)
                gc()  # garbage collection
                #odbcClose(con)
              }
              load (file.path( fnODBC, "logs41.rdata"), .GlobalEnv)
              load (file.path( fnODBC, "slip41.rdata"), .GlobalEnv)
              load (file.path( fnODBC, "ziff41.rdata"), .GlobalEnv)
              load (file.path( fnODBC, "off41.rdata"), .GlobalEnv)
              print("Objects are called 'logs41', 'slip41', 'ziff41', 'off41'")


    }
    
    
    if (DS %in% c("process.logs41.redo", "process.logs41", "process.logs41.unfiltered") ) {
      fo=file.path( fnODBC, "processed_logs41.rds")
      f2=file.path( fnODBC, "processed_logs41.unfiltered.rds")
      
      if (DS=="process.logs41.redo") {
          require(sf)    
        lobster.db('logs41')
        logs41$ADJCATCH_KG = logs41$ADJCATCH / 2.204
        logs41$CPUE = logs41$ADJCATCH_KG / logs41$NUM_OF_TRAPS
        saveRDS( logs41, file=f2, compress=T)
        
      lq = quantile(logs41$CPUE,c(0.01,.9995),na.rm=T)
      logs41p = subset(logs41,CPUE>=lq[1] & CPUE<=lq[2])
      logs41p$yr = lubridate::year(logs41p$FV_FISHED_DATETIME)
       
      gr41 = st_as_sf(readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','LFA41_grid_polys.rds')))
      logs41p$X = logs41p$DDLON * -1
      logs41p$Y = logs41p$DDLAT
      lp = st_as_sf(subset(logs41p, !is.na(X)),coords=c('X','Y'),crs=4326)
      lp = st_join(lp,gr41)
      saveRDS( lp, file=fo, compress=T)
      }
      if (DS=="process.logs41.unfiltered") {return(readRDS(f2))  }
      if (DS=="process.logs41") {return(readRDS(fo))}
      }
        

### Offshore Commercial Logs for Jonah crab
   if (DS %in% c("logs41jonah.redo", "logs41jonah") ) {

           if (DS=="logs41jonah.redo") {
              require(RODBC)
              #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

              # logs from LFA 41 Cheryl's query for adjusted catch and assigning subareas
              query41 = 'NEED TO IDENITFY'

              logs41jonah = connect.command(con, query41)
              logs41jonah$DDLON = logs41jonah$DDLON*-1
              save( logs41jonah, file=file.path( fnODBC, "logs41jonah.rdata"), compress=T)
              gc()  # garbage collection
              #odbcClose(con)
            }
            load (file.path( fnODBC, "logs41jonah.rdata"), .GlobalEnv)

    }

### Offshore Observer
    if (DS %in% c("observer41.redo", "observer41") ) {

        if (DS=="observer41.redo") {
                require(RODBC)

                observer41 = connect.command(con, 'select * from lobster.lobster_atsea_vw') #pulling from a materialized view
                observer41 = subset(observer41, LFA=='41')
                save( observer41, file=file.path( fnODBC, "observer41.rdata"), compress=T)
                gc()  # garbage collection
              }
              load (file.path( fnODBC, "observer41.rdata"), .GlobalEnv)

    }


#vms data
if(DS %in% c('lfa41.vms', 'lfa41.vms.redo')) {
      if(DS == 'lfa41.vms.redo') {
           require(RODBC)
           #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

  #Define a list of VRNs from offshore lobster vrns in rprofile


      vms.q  =  paste("SELECT rownum vesid,
                  p.longitude lon, p.latitude lat,
                 NVL(v.vessel_name,p.vr_number) vessel_name,
                 p.vr_number vrn,
                 to_char(p.POSITION_UTC_DATE, 'YYYY/MM/DD HH24:MI:SS') vmsdate,
                 p.speed_knots
                 FROM mfd_obfmi.vms_all p, mfd_obfmi.marfis_vessels_syn v
                 WHERE p.VR_NUMBER = v.vr_number(+)
                 AND p.vr_number IN ('",paste(vrn.vector.41,collapse="','"),"')",
                  sep="" )

      vms.data  =  connect.command(con, vms.q, believeNRows=FALSE)
      #odbcClose(con)
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
           #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

            # atSea
            atSea = connect.command(con, "select * from lobster.LOBSTER_ATSEA_VW")
            atSea2 = connect.command(con, "select * from lobster.lobster_bycatch_assoc")

            atSea2$PORT = NA
            atSea2$PORTNAME= atSea2$PORT_NAME
            atSea2$SAMCODE = NA
            atSea2$DESCRIPTION = atSea2$OWNER_GROUP
            atSea2$GRIDNO = atSea2$STRATUM_ID
            atSea2$SPECIESCODE = atSea2$SPECCD_ID
            atSea2$CULL = atSea2$MISSING_CLAWS
            #atSea2$CALWT=NA  #BZ Remoed and replaced with line below. Sept 2021
            atSea2$CALWT = atSea2$CALWT_G
            atSea2$STARTDATE = as.Date(NA)
            atSea2$SPECIES = NA

            atSea2$BOARD_DATE = substr(atSea2$BOARD_DATE,1,10)
            atSea2$datechar = nchar(atSea2$BOARD_DATE)


            atSea2$STARTDATE[atSea2$datechar<10] = as.Date( atSea2$BOARD_DATE[atSea2$datechar<10],"%d-%b-%y")
            atSea2$STARTDATE[atSea2$datechar==10] = as.Date( atSea2$BOARD_DATE[atSea2$datechar==10])

            atSea3 = atSea2

            names2=c("TRIP", "STARTDATE", "COMAREA_ID", "PORT", "PORTNAME", "CAPTAIN", "LICENSE_NO", "SAMCODE", "DESCRIPTION", "TRAP_NO",
                     "TRAP_TYPE", "SET_NO", "DEPTH", "SOAK_DAYS", "LATDDMM", "LONGDDMM", "GRIDNO",'NUM_HOOK_HAUL', "SPECIESCODE", "SPECIES", "SEXCD_ID","VNOTCH",
                     "EGG_STAGE","SHELL",  "CULL_ID", "FISH_LENGTH", "DISEASE", "CONDITION_CD", "CLUTCH", "CALWT")

      #BZ. Sept2021- Added "DISEASE", "CONDITION_CD", "CLUTCH" to above list to include these variables and match fields from atSea dataset

            atSea2= subset(atSea2,select=names2)
            atSea2$COMAREA_ID = substr(atSea2$COMAREA_ID,2,nchar(atSea2$COMAREA_ID))
            atSea2$LATDDMM = convert.dd.dddd(atSea2$LATDDMM)
            atSea2$LONGDDMM = convert.dd.dddd(atSea2$LONGDDMM) * -1

            names(atSea2) = names(atSea)
atSea$TRIPNO = as.character(atSea$TRIPNO)
atSea2$LICENCE_ID = as.character(atSea2$LICENCE_ID)
atSea2$TRAPNO = as.character(atSea2$TRAPNO)
atSea2$STRINGNO = as.character(atSea2$STRINGNO)

            atSea = dplyr::bind_rows(list(atSea,atSea2))

            save( atSea, file=file.path( fnODBC, "atSea.rdata"), compress=T)
            gc()  # garbage collection
            ##odbcClose(con)
          }
          load(file.path( fnODBC, "atSea.rdata"), .GlobalEnv)
     }

     if(DS %in% c('atSea.CatchLevel.redo','atSea.CatchLevel')){
           if(DS == 'atSea.CatchLevel.redo') {
            # atSea
           require(RODBC)
           #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

            atSeaCatchLevel = connect.command(con, "select * from lobster.atseacatchlevel")
            save( atSeaCatchLevel, file=file.path( fnODBC, "atSeaCatchLevel.rdata"), compress=T)
            gc()  # garbage collection
          #  #odbcClose(con)
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

                    season.dates = backFillSeasonDates(lobster.db('season.dates'),eyr=year(Sys.time()))
                    aS = subset(aS, !is.na(SDATE))
                     # season.dates = lobster.db('season.dates')
                       aS$WOS = NA
                       m=0
                        lfa = unique(aS$LFA)
                        lfa = na.omit(lfa)
                            for(i in 1:length(lfa)) {
                                  h  = season.dates[season.dates$LFA==lfa[i],]
                                  k = na.omit(unique(aS$SYEAR[aS$LFA==lfa[i]]))
                                  #h = na.omit(h)
                                  k = intersect(k,h$SYEAR)
                               for(j in k){
                                   m=m+1
                                   ll = which(aS$LFA==lfa[i] & aS$SYEAR==j)
                                   aS$WOS[ll] = floor(as.numeric(aS$SDATE[ll]-min(h$START_DATE[h$SYEAR==j]))/7)+1
                                }
                          }
                          if(any(!is.finite(aS$WOS))) {kl = which(!is.finite(aS$WOS)); aS$WOS[kl] = NA}
                          aS = subset(aS,WOS>0)

           atSea.clean = aS

          save( atSea.clean, file=file.path( fnODBC, fname), compress=T)

          }
     }

###DMR
    if (DS %in% c("DMR.redo", "DMR") ) {
      fname = 'Compiled_data.rds'
      fd = file.path(project.datadirectory('bio.lobster'),'data','MaineDMRSurvey')
      print('this is just for Region 5 in Fall and is in per km2')
      if(grepl('redo',DS)){
        print('Data obtained from https://mainedmr.shinyapps.io/MaineDMR_Trawl_Survey_Portal/')
        x = dir(fd,full.names = T)
        x = x[grep('csv',x)]
        ou = list()
        for(i in 1:length(x)){
          ou[[i]] = read.csv(x[i])
        }
        
        tow = grep('Tow',x)
        se = ou[[tow]]
        
        len = grep('Length',x)
        de = ou[[len]]
        
        catc = grep('Catch',x)
        ca = ou[[catc]]
        
        se = subset(se,Season=='Fall')
        se$id = paste(se$Survey,se$Tow_Number,sep="-")
        se$X = (se$Start_Longitude+se$End_Longitude)/2
        se$Y = (se$Start_Latitude+se$End_Latitude)/2
        se$z = ((se$Start_Depth_fathoms+se$End_Depth_fathoms)/2)*1.8288
        se$dist = se$Tow_LengthNM * 1.852
        se = subset(se,Region==5 & id != 'FL15-73',select=c(id,Start_Date,X,Y,z,dist,Region,Depth_Stratum,Tow_Time))
        se$spread = 11 #57 ft rope length; 11m from ASMFC benchmark 2020
        
        se$Dur_m=NA
        
        # Split the time string into minutes and seconds
        for(i in 1:nrow(se)){
          time_parts <- strsplit(se[i,'Tow_Time'], ":")[[1]]
          se[i,'Dur_m'] <- as.numeric(time_parts[2])
        }
        
        
        de$id = paste(de$survey,de$Tow_Number,sep="-")
        de = subset(de,Season=='Fall' & Region==5 & id !='FL15-73')
        de = subset(de,select=c(id,Length,Frequency,Sex))
        
        de = merge(de,se[,c('id','Dur_m','spread','dist')])
        de$Frequency = de$Frequency*(de$Dur_m /20) #back to raw -- frequency data is already corrected for sub sampling
        de$Frequency = de$Frequency/(de$spread/1000 * de$dist)
        de = subset(de,select = c(id,Length,Frequency, Sex))
        
        sc1=seq(3,253,by=1)
        de$SZ = sc1[cut(de$Length,sc1,right=FALSE,labels=F)]
        de$UID = de$ID
        de1 = aggregate(Frequency~id+SZ,data=de,FUN=sum)
        de1$P=de1$Frequency
        bb = reshape(de1[,c('id','SZ','P')],idvar='id',timevar='SZ', direction='wide')
        bb = na.zero(bb)
        
        sp = unique(ca$Common_Name)
        lo = sp[grep('Lobster',sp)]
        ca = subset(ca,Season=='Fall' & Common_Name==lo)
        ca$id = paste(ca$Survey,ca$Tow_Number,sep="-")
        
        ca = subset(ca,ca$id %in% unique(se$id),select=c(id,Year,Date,Expanded_Catch,Expanded_Weight_kg,Number_Caught,Weight_kg,Subsample_Weight_kg))
        ca = na.zero(ca)
        ca = aggregate(cbind(Expanded_Catch,Expanded_Weight_kg,Number_Caught,Weight_kg,Subsample_Weight_kg)~id,data=ca,FUN=sum)
        
        
        de$Rec=ifelse(de$Length<82 &de$Length>=70,de$Frequency,0)
        de$Rec=ifelse(de$Length==82,de$Frequency/2,de$Rec)
        
        de$Comm=ifelse(de$Length>82,de$Frequency,0)
        de$Comm=ifelse(de$Length==82,de$Frequency/2,de$Comm)
        de$sex = ifelse(de$Sex=='Female',2,ifelse(de$Sex=='Male',1,3))
        de$Comm=ifelse(de$sex==3,0,de$Comm)
        
        lobLW1 <- function(row) {
          lobLW(CL=row[1],sex=row[2])
        }
        de$fwt =  apply(de[,c('Length','sex')],1,lobLW1)
        de$Commwt = de$Comm*de$fwt/1000
        
        dea = aggregate(cbind(Comm,Rec,Commwt)~id,data=de,FUN=sum)
        dea = subset(dea,id %in% unique(se$id))
        dea = merge(dea,bb)
        cde = merge(ca,dea)
        scde = merge(se,cde,all.x=T)
        scde = bio.utilities::na.zero(scde)
        aa = st_as_sf(scde,coords = c('X','Y'),crs=4326)
        aa$Date = as.POSIXct(aa$Start_Date, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
        aa$Year = lubridate::year(aa$Date)
        
       saveRDS(aa,file=file.path(fd,fname)) 
      }
      return(readRDS(file=file.path(fd,fname)) )
}
### port sampling
    if (DS %in% c("port.sampling.redo", "port.sampling") ) {

     if (DS=="port.sampling.redo") {
        require(RODBC)
        #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

        # port
        port = connect.command(con, "select a.SAMPLE_SEQ,a.SAMPLE_NO,a.SDATE,a.SEASON,a.NTRAPS,a.LATITUDE,a.LONGITUDE,a.GRADE, b.L_SIZE,b.N_MALES,b.N_FEM,b.NBF, d.LFA,c.PORT,c.COUNTY,c.STAT,c.PORT_CODE,c.LATITUDE port_lat,c.LONGITUDE port_lon from lobster.CRLENGCODE a, lobster.CRLENGFREQ b, lobster.CRLOCATIONS c, frailc.lfa_port d where a.sample_seq = b.sample_seq and a.port = c.port and c.PORT_CODE = d.port(+) and a.type = 'P' ")
        save( port, file=file.path( fnODBC, "port.rdata"), compress=T)
        gc()  # garbage collection
        #odbcClose(con)
      }
      load(file.path( fnODBC, "port.rdata"), .GlobalEnv)
     }


if (DS %in% c("process.port.sampling.redo", "process.port.sampling") ) {

        load(file.path( fnODBC, "port.rdata"), .GlobalEnv)
        port = addSYEAR(port)
        season.dates = lobster.db('season.dates')
         m=0
        # port = subset(port,LFA %in% c('27','28','29','30','31A','31B','32','33'))
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
        save( port, file=file.path( fnODBC, "process.port.rdata"), compress=T)
     load(file.path( fnODBC, "process.port.rdata"), .GlobalEnv)
     }



### CRIS database
    if (DS %in% c("cris.redo", "cris") ) {

     if (DS=="cris.redo") {
        require(RODBC)
        #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

        # cris
        cris.trips = connect.command(con, "select * from cris.crtrips")
        save( cris.trips, file=file.path( fnODBC, "crisTrips.rdata"), compress=T)
        cris.traps = connect.command(con, "select * from cris.crtraps")
        save( cris.traps, file=file.path( fnODBC, "crisTraps.rdata"), compress=T)
        cris.samples = connect.command(con, "select * from cris.crsamples")
        save( cris.samples, file=file.path( fnODBC, "crisSamples.rdata"), compress=T)
        gc()  # garbage collection
        #odbcClose(con)
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
        #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

        # Denton Script Sept 28 2016
        obs.samp  =  connect.command(con, paste("
SELECT trip.trip_id,late, lone, sexcd_id,fish_length,st.nafarea_id,board_date, st.fishset_id
                                        FROM istrips trip, isfishsets st,   iscatches ca, isfish fish,
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

"))

        save( obs.samp, file=file.path( fnODBC, "lfa41.observer.samples.rdata"), compress=T)
        gc()  # garbage collection
        #odbcClose(con)
      }

      load(file=file.path( fnODBC, "lfa41.observer.samples.rdata"),.GlobalEnv)

    }

### FSRS traps
    if (DS %in% c("fsrs.redo", "fsrs") ) {

     if (DS=="fsrs.redo") {
        #require(RODBC)
        #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's
       # md = connect.command(con, "select max(haul_date) mdate from fsrs_lobster.FSRS_LOBSTER_VW") #the sizes are all recoded to be continuous --- the old guage is now reflected in the new numbering AMC
        #load(file=file.path( fnODBC, "fsrs.rdata"))
        #if_md = max(fsrs$HAUL_DATE)
       # if(f_md>=as.POSIXct(md[,1])) stop('No new data to over write old file')
        
       fsrs = connect.command(con, "select * from fsrs_lobster.FSRS_LOBSTER_VW") #the sizes are all recoded to be continuous --- the old guage is now reflected in the new numbering AMC
        
        print("FYI. Starting Fall 2019 (LFA 27-35) and spring 2018 (LFA 36)")
        print("New size groups used 1-27 (5mm bins), replacing 1-16 (10mm bins)")
        print("Users need to specify year and size to ensure you get the right coding")

        #print("If loading data manually use FSRS.load.from.text.r function")
        #Create csv through FSRS.load.from.text.r before running this step
        #
        #if (file.exists(file.path(project.datadirectory("bio.lobster"), "data","inputs","non.db.fsrs.csv")))
        #{
       #non.db.fsrs=read.csv(file.path(project.datadirectory("bio.lobster"), "data","inputs","non.db.fsrs.csv"))
        # non.db.fsrs$SIZE_GRP=non.db.fsrs$SIZE_CD
         #non.db.fsrs=non.db.fsrs[names(fsrs)] #only retain Variables in 'fsrs'
         #non.db.fsrs$RECAPTURED=as.integer(non.db.fsrs$RECAPTURED)
         #non.db.fsrs$HAUL_DATE=as.POSIXct(non.db.fsrs$HAUL_DATE)
         #fsrs= rbind(fsrs, non.db.fsrs[names(fsrs)])
         #}

        fsrs$SIZE_CD=fsrs$SIZE_GRP
        fsrs=within(fsrs, rm(SIZE_GRP))
        fsrs$mn = lubridate::month(fsrs$HAUL_DATE)
        fsrs$SYEAR= lubridate::year(fsrs$HAUL_DATE)
        ii = which(fsrs$LFA>32 & fsrs$mn %in% c(10,11,12))
        fsrs$SYEAR[ii] = fsrs$SYEAR[ii]+1
        save( fsrs, file=file.path( fnODBC, "fsrs.rdata"), compress=T)
        gc()  # garbage collection
        if(!("ROracle" %in% (.packages()))){
          #odbcClose(con)
        }
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

        #mls=read.csv(file.path( project.datadirectory("bio.lobster"), "data","inputs","ccir_inputs.csv"))
        #if (max(fsrs$HAUL_YEAR)>max(mls$Year)){
        #     stop(paste("You need to update", file.path( project.datadirectory("bio.lobster"), "data","inputs","ccir_inputs.csv"),
        #     "to reflect most recent year", sep=" "))
        #  }
        #BZ- To do. Remove the reference to MinLegalSize table and replace with ccir_inputs.csv. Only one table updated annually this way.

          vars = c('RECORD_NUMBER','TRAP_NO','LOBSTER_NO','SEX','SIZE_CD','SHORT','VESSEL_CD','SOAK_DAYS','DEPTH','LFA','LATITUDE','LONGITUDE','TEMP','WIND_DIRECTION','WIND_SPEED','HAUL_DATE','HAUL_YEAR','LFA_GRID')
          fsrs = fsrs[,vars]
          fsrs = rename.df(fsrs,vars, c("Record.Number", "Trap.Number", "Lobster.Number", "Sex", "Size", "Short" , "Vessel.Code" ,"Soak.Days", "Depth", "LFA", "Latitude", "Longitude", 'Temperature',"Wind.Direction", "Wind.Speed" ,"DATE" , "YEAR",'Grid'))
          fsrs$indY = ifelse(month(fsrs$DATE)>8,1,0)
          fsrs$YEAR =  fsrs$YEAR + fsrs$indY #ending year required by CCIR program
          fsrs$indY = NULL
          fsrs$Berried = ifelse(fsrs$Sex == 3, 1, 0)
          fsrs$Sex = ifelse(fsrs$Sex == 3, 2, fsrs$Sex)
          fsrs$julian = round(as.numeric(julian(fsrs$DATE)))

           mls = read.csv(file.path(bio.directory,'bio.lobster.data','misc',"MinLegalSize.csv"))
           if(any(names(mls)=='X')) mls$X = NULL
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
           
           scd.old = read.csv(file=file.path(bio.directory,'bio.lobster.data','fsrs',"FSRS_SIZE_CODES.csv"))
           scd.new = read.csv(file=file.path(bio.directory,'bio.lobster.data','fsrs',"FSRS_SIZE_CODES_NEW2020.csv"))

           mls.old=mls[mls$YEAR<2020,]
          for(i in 1:nrow(mls.old)) {
            a = mls.old[i,'MLS']
            mls.old$MLS_FSRS[i]= scd.old$SIZE_CD[intersect(which(scd.old$MIN_S<=a),which(scd.old$MAX_S>=a))]
             }

           mls.new=mls[mls$YEAR>2019,]
           for(i in 1:nrow(mls.new)) {
             a = mls.new[i,'MLS']
             mls.new$MLS_FSRS[i]= scd.new$SIZE_CD[intersect(which(scd.new$MIN_S<=a),which(scd.new$MAX_S>=a))]
           }
          mls=rbind(mls.old, mls.new)

          fsrs = merge(fsrs,mls,by=c('LFA','YEAR'),all.x=T)
          fsrs=fsrs[is.finite(fsrs$MLS_FSRS),]

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
        scallop.tows = connect.command(con, "select * from SCALLSUR.SCTOWS")
        scallop.tows = subset(scallop.tows,TOW_TYPE_ID %in% c(1,5))# removed this filter AMC Jan 2025 & STRATA_ID %ni% c(50,55))
        save( scallop.tows, file=file.path( fnODBC, "scallopTows.rdata"), compress=T)
       
        scallopSurv = connect.command(con, "select * from SCALLSUR.SCBYCATCH_STD where speccd_id='2550'")

      scallopSurv = subset(scallopSurv,TOW_TYPE_ID %in% c(1,5) )
        save( scallopSurv, file=file.path( fnODBC, "scallopSurv.rdata"), compress=T)

        #these are now saved as a sf object       
        #scallopStratDefs = connect.command(con, "select * from SCALLSUR.scstratadefs")
        #scallopStratDefs = subset(scallopStratDefs,STRATA_ID %ni% c(51,55))
        #saveRDS( sdef, file=file.path( fnODBC, "scallopStratDefs.rds"), compress=T)
        
       
       gc()  # garbage collection
        #odbcClose(con)
     }
    #  scallopStratDefs = readRDS(file=file.path( fnODBC, "scallopStratDefs.rds"))
      load(file.path( fnODBC, "scallopTows.rdata"))
      load(file.path( fnODBC, "scallopSurv.rdata"))
      
      return(list(scallop.tows,scallopSurv))
    }

### lobster survey
    if (DS %in% c("survey.redo", "survey") ) {

      if (DS=="survey.redo") {
        Sys.setenv(TZ = "America/Halifax")
        # survey
        
         ILTSTemp = connect.command(con, "select * from lobster.ILTS_TEMPERATURE")
        ILTSSensor = connect.command(con, "select * from lobster.ILTS_SENSORS")
        ILTSClick =  connect.command(con, "select * from lobster.ILTS_CLICKTOUCH")
        ILTSClickComp =  connect.command(con, "select * from lobster.ILTS_CLICKTOUCH_COMP")
        ILTSOlextracks = connect.command(con, "select * from lobster.ILTS_OLEXTRACKS")
        ILTSOlex = connect.command(con, "select * from lobster.ILTS_OLEX")
        surveyCatch = connect.command(con, "select * from lobster.ILTSSETS_MV")
        surveyMeasurements = connect.command(con, "select * from lobster.ILTSDETAILS_MV")
        fishMeasurements = connect.command(con, "select * from lobster.ILTSFISHLENGTHS_MV")


        with(surveyMeasurements,paste(TRIP_ID,SET_NO,sep=''))->surveyMeasurements$SET_ID
        with(surveyCatch,paste(TRIP_ID,SET_NO,sep=''))->surveyCatch$SET_ID
        surveyCatch$SET_LONG = surveyCatch$SET_LONG*-1
        surveyCatch$HAUL_LONG = surveyCatch$HAUL_LONG*-1
        surveyCatch$YEAR = year(surveyCatch$BOARD_DATE)
        surveyMeasurements$SET_LON = surveyMeasurements$SET_LON*-1
        surveyMeasurements$HAUL_LON = surveyMeasurements$HAUL_LON*-1



 #browser()
       # surveyStationID = connect.command(con, "select * from LOBSTER.ILTS_SURVEY_STATION")
      #  save(list=c("ILTS2016TowDepth","ILTS2016TowSpread","ILTS2016Tracks") , file=file.path( fnODBC, "MarPort2016.rdata"), compress=T)
        save(surveyCatch, file=file.path( fnODBC, "surveyCatch.rdata"), compress=T)
        save(surveyMeasurements, file=file.path(fnODBC, "surveyMeasurements.rdata"), compress=T)
        save(fishMeasurements, file=file.path(fnODBC, "fishMeasurements.rdata"), compress=T)
        save(ILTSTemp, file=file.path(fnODBC, "ILTSTemp.rdata"), compress=T)
        save(ILTSSensor, file=file.path(fnODBC, "ILTSSensor.rdata"), compress=T)
        save(ILTSClick, file=file.path(fnODBC, "ILTSClick.rdata"), compress=T)
        save(ILTSClickComp, file=file.path(fnODBC, "ILTSClickComp.rdata"), compress=T)
        save(ILTSOlextracks, file=file.path(fnODBC, "ILTSOlextracks.rdata"), compress=T)
        save(ILTSOlex, file=file.path(fnODBC, "ILTSOlex.rdata"), compress=T)
        
        #   save(surveyStationID, file=file.path(fnODBC, "surveyStationID.rdata"), compress=T)

        gc()  # garbage collection
      }
      load(file.path( fnODBC, "surveyCatch.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "surveyMeasurements.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "fishMeasurements.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSTemp.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSSensor.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSClick.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSClickComp.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSOlextracks.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "ILTSOlex.rdata"), .GlobalEnv)
      load(file.path( fnODBC, "MarPort2016.rdata"), .GlobalEnv)
      
      load(file.path( fnODBC, "surveyStationID.rdata"), .GlobalEnv)

    }
### lobster sampling from rv survey

if(DS %in% c('rv.survey.samples.redo','rv.survey.samples.samples')) {
   if (DS=="rv.survey.samples.redo") {
        require(RODBC)
        #con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

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
        #odbcClose(con)
      }

      load(file=file.path( fnODBC, "rv.survey.samples.rdata"),.GlobalEnv)

}

    if(DS %in% c('species_codes.redo','species_codes')) {
      if (DS=="species_codes.redo") {
        spp = connect.command(con, "select * from groundfish.gsspecies_andes")
        saveRDS( spp, file=file.path( fnODBC, "species_codes.rds"), compress=T)
      }

      readRDS(file=file.path( fnODBC, "species_codes.rds"))

      }
  }

