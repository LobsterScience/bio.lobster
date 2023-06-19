#scallop and ILTS
require(bio.lobster)
require(sf)
require(bio.utilities)
require(devtools)
require(dplyr)
require(lubridate)
la()
aT = compileAbundPresAbs(redo=F,size=F)
aT = subset(aT, SOURCE %in% c('ILTS_ITQ','Scallop survey'))


survey <- aT %>%   
  st_as_sf(coords=c('LONGITUDE',"LATITUDE"),crs=4326) %>% st_transform(32620)

#st_geometry(survey) = st_geometry(survey)/1000
#st_crs(survey) = 32620

survey$date2w= (survey$YEAR + round(survey$DYEAR*26)/26)
survey$date1w= (survey$YEAR + round(survey$DYEAR*52)/52)

sL = subset(survey, SOURCE=='ILTS_ITQ')
sS = subset(survey, SOURCE=='Scallop survey')

sLT = unique(sL$date1w)
out = list()
m=0
for(i in 1:length(sLT)){
      sl = subset(sL,date1w == sLT[i])
      ss = subset(sS,date1w == sLT[i])
      ss = rename.df(ss,c('DATE','Lobster','OFFSET'),c('ScallopDate','ScallopLobster','ScallopOffset'))
    if(nrow(sl)>1 & nrow(ss)>1){
    m=m+1
            ou = st_nearest_feature(sl,ss)
      xc = cbind(sl, ss[ou,c('ScallopDate','ScallopLobster','ScallopOffset')])
      xc$dist = as.numeric(st_distance(sl,ss[ou,],by_element=T))
     out[[m]] = xc   
      }
}

out = bind_rows(out)
out$LobCorr = out$Lobster / out$OFFSET
out$ScalLobCorr = out$ScallopLobster / out$ScallopOffset

with(subset(out,dist<20*1000 & YEAR>2016 & LobCorr>0),plot(LobCorr,ScalLobCorr))
with(subset(out,dist<20*1000  & LobCorr>0 & ScalLobCorr>0),cor.test(log(LobCorr+0.01),log(ScalLobCorr+0.01)))
