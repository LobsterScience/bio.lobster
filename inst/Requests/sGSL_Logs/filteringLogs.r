require(bio.lobster)
require(devtools)
require(sf)
require(purrr)
require(dplyr)
require(geosphere)

la()

setwd(file.path(project.datadirectory('bio.lobster'),'data','sGSL'))
dir()

x = readxl::excel_sheets("GLF2024_124_Estimated_lobster_landings_traps_community_2014-2023_Adam_Cook.xlsx")
data_list <- map(x, ~ readxl::read_xlsx("GLF2024_124_Estimated_lobster_landings_traps_community_2014-2023_Adam_Cook.xlsx", sheet = .x,skip=4) %>% 
                   mutate(Year=.x))

dl = bind_rows(data_list)
names(dl) = c('Year','CommunityCode','CommNm','Wt','NTraps','NLic')
#dl = subset(dl,Year !=2021)
#port loc

pc = read.csv('port_codes_recode.csv')
pc1 = subset(pc,select=c(code,recode_.code,lfa))

dp = merge(dl,pc1,by.x='CommunityCode',by.y='code',all.x=T)

#group based on redefined port codes
s_dp <- dp %>%
  group_by(Year,recode_.code) %>%
  summarise(
    unique_community_codes = n_distinct(CommunityCode),  # Count of unique codes
    total_landed_weight = sum(Wt,na.rm=T),  # Sum of landed weight
    total_trap_numbers = sum(NTraps,na.rm=T),  # Sum of trap numbers
    total_licences_reporting = sum(NLic,na.rm=T)  # Sum of licences reporting
  )


pcD = pc %>%
  distinct(recode_.code,recode_latitude,recode_longitude)

#what lfa goes with codes
pcL = pc %>%
  distinct(code,lfa)
pcD = merge(pcD,pcL,by.x='recode_.code',by.y='code')

s_dp = merge(s_dp,pcD)

sCL = subset(s_dp, total_licences_reporting>=5)
sMin = subset(s_dp, total_licences_reporting<5)

sMinL = split(sMin,f=list(sMin$Year,sMin$lfa))

# Compute distances and find nearest location within an LFA
for(j in 1:length(sMinL)){
  
  plo = sMinL[[j]]
  pp = subset(sCL,Year==unique(plo$Year) & lfa==unique(plo$lfa))
cc = lapply(1:nrow(plo), function(i) {
  pp = subset(pp,!is.na(recode_latitude))
  distances <- distHaversine(plo[i, c("recode_longitude", "recode_latitude")], pp[, c("recode_longitude", "recode_latitude")])
  (pp$recode_.code[which.min(distances)])
})

plo$closest_code = unlist(lapply(cc,function(x) if(length(x)==0) NA else x))
sMinL[[j]] = plo
}

pcD = bind_rows(sMinL)
pcD$recode_.code = pcD$closest_code
pcD$closest_code <- NULL

ss = bind_rows(sCL,pcD)

ssr = ss %>%
    filter(!is.na(recode_.code)) %>%
      group_by(Year,recode_.code,lfa) %>%
  summarise(
    unique_community_codes = sum(unique_community_codes),
    total_landed_weight = sum(total_landed_weight),
    total_trap_numbers = sum(total_trap_numbers),
    total_licences_reporting = sum(total_licences_reporting)
  )


sss = merge(ssr,subset(pc,select=c(code,name,province,district,latitude, longitude, nafo.subdivision)),by.x='recode_.code',by.y='code')
names(sss)[1] = 'CommunityCode'
write.csv(sss,'PrivacyProtectedGulfLogsbyPort.csv')


#much of a diff with the filtering??
bb =aggregate(NTraps~Year,data=dl,FUN=sum)
cc = aggregate(total_landed_weight~lfa+Year,data=sss,FUN=sum)

bb$sc = 'filt'
cc$sc = 'ori'

dd = bind_rows(bb,cc)

ggplot()+geom_line(data=dd,aes(x=as.numeric(Year),y=total_landed_weight,colour=sc))+facet_wrap(~lfa)
