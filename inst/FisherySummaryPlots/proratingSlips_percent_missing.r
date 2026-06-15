require(bio.lobster)
require(devtools)
require(tidyr)
require(dplyr)
la()



a = lobster.db('percent_reporting')
a$YR = as.numeric(substr(a$YEARMTH,1,4))
a$MN = as.numeric(substr(a$YEARMTH,5,6))
al <- a %>%
  pivot_longer(
    cols = -c(YR,MN),
    names_to = c("LFA", ".value"),
    names_pattern = "L(\\d+)(MISS|RECD|PERCENT)"
  ) %>%
  tidyr::drop_na()
al$SYEAR = al$YR
al$SYEAR = ifelse(al$LFA %in% c(33:38) & al$MN %in% 11:12,al$SYEAR+1, al$SYEAR)
al$SYEAR = ifelse(al$LFA %in% c(35) & al$MN %in% 10,al$SYEAR+1, al$SYEAR)
al = subset(al,SYEAR==2026 & LFA %in% c(33,34))


b = lobster.db('slips')
b$YR = lubridate::year(b$DATE_LANDED)
b$MN = lubridate::month(b$DATE_LANDED)
b$SYEAR = b$YR
b$SYEAR = ifelse(b$LFA %in% c(33:38) & b$MN %in% 11:12,b$SYEAR+1, b$SYEAR)
b$SYEAR = ifelse(b$LFA %in% c(35) & b$MN %in% 10,b$SYEAR+1, b$SYEAR)

b=subset(b,SYEAR==2026 & LFA %in% 33:34) 
#just 33-34 for now
bl = split(b,f=list(b$LFA))

outs = list()

for(i in 1:length(bl)){
  junk = bl[[i]]
  junk = aggregate(SLIP_WEIGHT_LBS~MN+LFA+SYEAR+LICENCE_ID+SUM_DOC_ID,data=junk,FUN=sum)
  m = unique(junk$MN)
  iter=100
  v = matrix(nrow=iter, ncol=length(m))
  for(j in 1:iter){
    for(k in 1:length(m)){
        aa = subset(al,LFA==unique(junk$LFA) & MN == m[k])
        sl = subset(junk,MN==m[k])$SLIP_WEIGHT_LBS
      v[j,k] = sum(c(sample(sl, size=aa$MISS, replace = T),sl))
      }
  }
  me = apply(v/2204.62,1,sum)
  
  
  outs[[i]]= c(LFA=aa$LFA,SYEAR=aa$SYEAR,meanProrated = mean(me), l95=quantile(me, 0.025),u95 = quantile(me,0.975))

}

ou = bind_rows(outs)
