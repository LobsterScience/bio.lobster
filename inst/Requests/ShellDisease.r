require(bio.lobster)
require(devtools)
require(bio.utilities)
la()

lobster.db('atSea')
a = atSea

d = lobster.db('fsrs')
s = lobster.db('survey')
f = groundfish.db('special.lobster.sampling')
f = na.zero(f)

#RV survey
f$yr = substr(f$MISSION,4,7)
y=unique(f$yr)
o=list()
for(i in 1:length(y)){
 j= table(subset(f,yr==y[i])$SHELL_DISEASE_INDEX)
 w=sum(j)
 j=j/w
o[[i]]=c(y[i],j,w)  
}
o = dplyr::bind_rows(o)
names(o)=c('year','stage0','stage1','stage2','samplesize1','stage3','samplesize2')

clipr::write_clip(o)
#lobster survey

lm = subset(surveyMeasurements,SPECCD_ID==2550)
lm$yr = lubridate::year(lm$BOARD_DATE)
y=2013:2022
li=list()
for(i in 1:length(y)){
  j= table(subset(lm,yr==y[i])$SHELL_DISEASE)
  w =sum(j)
  j=j/w
  li[[i]]=c(y[i],j,w)  
}
li = dplyr::bind_rows(li)
names(li)=c('year','stage0','stage1','stage2','stage3','samplesize1','samplesize2')

clipr::write_clip(li)