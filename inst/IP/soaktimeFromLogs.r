require(bio.lobster)
require(dplyr)
require(lubridate)
require(devtools)
la()

b = lobster.db('process.logs.unfiltered')

bs = split(b,f=list(b$VR_NUMBER, b$LFA,b$SYEAR), drop=T)

for(i in 1:length(bs)){
      bb = bs[[i]]
      bb$SoakT = c(NA,diff(bb$DATE_FISHED))
      bs[[i]] = bb
}

b = bind_rows(bs)

ggplot(subset(b,LFA==34 & SoakT<30 & SoakT>0),aes(x=as.factor(WOS),y=SoakT))+geom_boxplot()+geom_smooth()
  