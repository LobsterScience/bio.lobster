
require(bio.lobster)
require(ggplot2)
require(bio.utilities)
require(devtools)
require(ggpubr)
load_all('~/git/bio.utilities')
la()
theme_set(theme_test(base_size = 14))

d = lobster.db('vessels.by.port')
d = subset(d,LFA %ni% c(28) & YR_FISHED>2004 & YR_FISHED<2026)
d = na.zero(d)
d1 = aggregate(cbind(GROSS_TONNAGE, BHP, LOA, BREADTH, DEPTH,YEAR_BUILT)~VR_NUMBER+LFA+YR_FISHED,data=d,FUN=min)
d = na.zero(d1,rev=T)
d$YR_FISHED = as.numeric(d$YR_FISHED)
d = subset(d,YEAR_BUILT>1900)
d$Age_of_Vessel = as.numeric(d$YR_FISHED)-d$YEAR_BUILT
d$YR_FISHED = as.character(d$YR_FISHED)
l = aggregate(Age_of_Vessel~LFA,data=d,FUN=median)
oo = aggregate(Age_of_Vessel~YR_FISHED+LFA,data=d,FUN=function(x) quantile(x,probs=c(0.25,.5,.75)))


#VR key to anonymize
vr_key = data.frame(VR_NUMBER = unique(d$VR_NUMBER),vessel_id = paste0('ID_',seq_along(unique(d$VR_NUMBER))))

require(dplyr)
d_a = d %>%
      left_join(vr_key,by='VR_NUMBER')%>%
      select(-VR_NUMBER)
      


o = lobster.db('licence_ages')
o = subset(o, LFA %ni% 28)
o$id = paste(o$LICENCE_ID,o$FIN,o$LFA)
ui = unique(o$id)

ou = list()
m=0
for(i in 1:length(ui)){
  ju = subset(o,id==ui[i])
  nr = nrow(ju)
  for(j in 1:nr){
    m=m+1
    bd = lubridate::year(ju$BIRTHDATE[j])
    sd = lubridate::year(ju$START_DATE[j])
    ed = lubridate::year(ju$END_DATE[j])
    if(ed==4444) ed=2025
    ou[[m]] = data.frame(fishingyrs=sd:ed,birthyear=bd,lfa=ju$LFA[j],lic=ju$LICENCE_ID[j])
  }
}

o = do.call(rbind,ou)
o$Age = o$fishingyrs-o$birthyear
o = subset(o,fishingyrs>1994 &fishingyrs<2026 & !is.na(Age)) #missing info prior to 1995
#VR key to anonymize
o$ID = paste0(o$birthyear,o$lic)
lic_key = data.frame(ID = unique(o$ID),A_ID = paste0('HID_',seq_along(unique(o$ID))))

require(dplyr)
o_a = o %>%
  left_join(lic_key,by='ID')%>%
  select(-lic, -ID)

names(o_a)[1] = 'YR_FISHED'

d_a$BREADTH[which(d_a$BREADTH<10)] <- NA
d_a$BREADTH[which(d_a$BREADTH>40)] <- NA
d_a$DEPTH[which(d_a$DEPTH>10)] <- NA


saveRDS(list(o_a,d_a),file='MacKeracher_Vessel_Harv_Ages.rds')
