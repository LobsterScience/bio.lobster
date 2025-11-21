require(bio.lobster)
require(devtools)
require(dplyr)
require(lubridate)
la()

#lfa 34

g = lobster.db('licence_characteristics')
g = subset(g,LFA==34)
i = grep('CC',g$LIC_TYPE)
gi = g[i,]
gi$SYEAR = year(gi$START_DATE)+1

sl = lobster.db('process_slips')
ou = list()
for(i in 1:nrow(gi)){
      gl = subset(sl,LICENCE_NO==gi$LICENCE_ID[i] & SYEAR>=gi$SYEAR[i] & LFA ==34)
      ou[[i]] = aggregate(adj_wt_kg~SYEAR+LICENCE_NO,data=gl,FUN=sum)
  }

you = do.call(rbind,ou)
v = aggregate(adj_wt_kg~SYEAR,data=you,FUN=sum)
vv = aggregate(LICENCE_NO~SYEAR,data=you,FUN=function(x) length(unique(x)))
v = merge(vv,v)

b = lobster.db('seasonal.landings')
b$SYEAR = as.numeric(substring(b$SYEAR,6,9))
b = subset(b,select=c(SYEAR,LFA34))
v$CC_T  = v$adj_wt_kg/1000

bb = merge(b,v)

bb$propCC = bb$CC_T/bb$LFA34

