#all ILTS data for Erin Miller
require(bio.lobster)
require(devtools)
la()

lobster.db('survey')
g = ILTS_ITQ_All_Data(redo_base_data=F,redo_set_data=F,size = NULL, sex=NULL,aggregate=F,return_tow_tracks=F,biomass=F,extend_ts=F,return_base_data=T)
g = subset(g,select=c(TRIP_ID,SET_NO,SPECCD_ID,YEAR, VESSEL_NAME, GEAR, FISHSET_ID, NUM_CAUGHT, SET_LAT, SET_LONG, SET_DEPTH,SET_TIME, SET_DATE, distance, sweptArea, WEIGHT_KG, temp, SEX, FISH_LENGTH, PRORATED_NUM_AT_LENGTH))  
sp = lobster.db('species_codes')

g = merge(g,sp[,c('CODE','COMM','SPEC')],by.x='SPECCD_ID',by.y = 'CODE')
write.csv(g,'ILTS_catch_data.csv')
