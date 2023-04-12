#bait data

require(bio.lobster)
require(bio.utilities)

db.setup()
atSea2 = connect.command(con, "select * from cooka.lobster_bycatch_assoc")
atSea2$ID = paste(atSea2$TRIP_ID,atSea2$TRIP,atSea2$SETCD_ID,atSea2$FISHSET_ID,atSea2$BOARD_DATE,atSea2$TRAP_ID,sep=".")
atSea2 = subset(atSea2,OWNER_GROUP=='SWLSS')
atSea2$BAIT_CD2 = ifelse(is.na(atSea2$BAIT_CD2),44,atSea2$BAIT_CD2)
atSea2$BAIT_TYPE2 = ifelse(is.na(atSea2$BAIT_TYPE2),44,atSea2$BAIT_TYPE2)
x1 = aggregate(cbind(ABUNDANCE)~SPECCD_ID+COMAREA_ID+STRATUM_ID+ID+BAIT_CD+BAIT_TYPE1+BAIT_CD2+BAIT_TYPE2+BOARD_DATE,data=atSea2,FUN=function(x) sum(x,na.rm=T))

 lobster.db('species_codes')

x2=merge(x1,spp[,c('CODE','COMM')],by.x='SPECCD_ID',by.y='CODE')
x2 = rename.df(x2,'COMM','Captured_Species')


x2=merge(x2,spp[,c('CODE','COMM')],by.x='BAIT_CD',by.y='CODE')
x2 = rename.df(x2,'COMM','Bait_Species')


x2=merge(x2,spp[,c('CODE','COMM')],by.x='BAIT_CD2',by.y='CODE')
x2 = rename.df(x2,'COMM','Bait_Species2')

sp = c(11,  23,  60,  70,  42,  2511,  16,  62,  30,300,320,  10,  44,12,  49,  14,  2513,  4355,  301,  2531,  69,  273,  400,  13,  20,  40)

csp = c(8300,30,20,207,2510,112,648,310,2531,636,4330,759,14,4320,40,49,17,4321,65,501,60,41,251,400,23,13,15,11,2526,10,6400,201,
        12,43,70,16,6200,50,640,42,2100,21,2527,2531,636,54,4330,2550,2511,4210,2513,211,300,2559,301,122,320,2520)

x3=subset(x2,SPECCD_ID %in% csp & BAIT_CD %in% sp & BAIT_CD2 %in% sp  )
x3$Bait_Species2 = ifelse(x3$BAIT_CD2==44,'None',x3$Bait_Species2)
x3$BAIT_CD2 = ifelse(x3$BAIT_CD2==44,NA,x3$BAIT_CD2)

x3$BaitType1 = recode(x3$BAIT_TYPE1,c("1='Fresh'; 2='Frozen'; 3='Salted'; 4='Imported'; 44='Not_Reported'"))
x3$BaitType2 = recode(x3$BAIT_TYPE2,c("1='Fresh'; 2='Frozen'; 3='Salted'; 4='Imported'; 44='Not_Reported'"))

names(x3) = c('Bait_species_code2','Bait_species_code1','Species_caught_code','LFA','Logbook_reporting_grid','Unique_Trap_ID','Bait_type_code1','Bait_type_code2',
              'Date_Fished','Abundance_of_Species_Caught','Species_caught_name','Bait_species_name1','Bait_species_name2','Bait_type_name1','Bait_type_name2')

x3 = x3[,c('Unique_Trap_ID',
           'LFA',
           'Logbook_reporting_grid',
           'Date_Fished',
           'Bait_species_code1',
           'Bait_species_name1',
           'Bait_type_code1',
           'Bait_type_name1',
           'Bait_species_code2',
           'Bait_species_name2',
           'Bait_type_code2',
           'Bait_type_name2',
           'Species_caught_code',
           'Species_caught_name',
            'Abundance_of_Species_Caught'
            )]


write.csv(x3,'LobsterTrapCatchByBait.csv')
