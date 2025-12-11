require(bio.lobster)
require(devtools)
require(ggplot2)
require(dplyr)

la()

y = ILTS_ITQ_All_Data_updates(redo_base_data = T,redo_set_data = F,size=c(70,82),aggregate=T,species=2550,biomass = F,applyGearConversion = F)
y = subset(y,select=c('TRIP_ID','SET_NO','YEAR','SET_DATE','SET_LONG','SET_LAT','sweptArea','GEAR','SA_CORRECTED_PRORATED_N'))
names(y)[9] = 'N_Not_Gear_Corrected'

y1 = ILTS_ITQ_All_Data_updates(redo_base_data = T,redo_set_data = F,size=c(70,82),aggregate=T,species=2550,biomass = F,applyGearConversion = T)
y1 = subset(y1,select=c('TRIP_ID','SET_NO','YEAR','SET_DATE','SET_LONG','SET_LAT','sweptArea','GEAR','SA_CORRECTED_PRORATED_N'))
names(y1)[9] = 'N_Gear_Corrected'

yy1 = merge(y1,y)

with(subset(yy,GEAR=='280 BALLOON'),plot(N_Not_Gear_Corrected,N_Gear_Corrected))

yy$N_Gear_Corrected = round(yy$N_Gear_Corrected*yy$sweptArea)
yy$N_Not_Gear_Corrected = round(yy$N_Not_Gear_Corrected*yy$sweptArea)



dictionary <- data.frame(
  Description = c("Trip Identifier",
            "Set Identifier",
            "Survey Year",
            "Survey Set Date",
            "Longitute",
            "Latitude",
            "Swept area of the Tow (km2)",
            "Trawl Gear",
            "Number of 70-82mm(inclusive) lobster in tow, adjusted by Gear selectivity coefficients (Cook et al 2025)",
            "Number of 70-82mm(inclusive) lobster in tow"
            ),
  type  = sapply(yy, function(x) paste(class(x), collapse = "/"))
)



save_with_metadata(yy,description='ILTS Recruitment Data for JBarss',dictionary=dictionary, file='LobsterILTS_70-82.rds')
