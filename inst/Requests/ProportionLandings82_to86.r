require(bio.lobster)
require(bio.utilities)
require(lubridate)
lobster.db('atSea')

as = subset(atSea, SPECIESCODE==2550 & SEX %in% c(1,2) & year(STARTDATE)>2007 & is.na(VNOTCH) )
as$fl = round(as$CARLENGTH)
as$n=1
as = aggregate(cbind(CALWT,n)~LFA+fl,data=as,FUN=sum)
as$USMLS_wt = ifelse(as$fl>82 & as$fl<87,as$CALWT,0)
as$USMLS_n = ifelse(as$fl>82 & as$fl<87,as$n,0)

as$CURMLS_wt = ifelse(as$fl>82 ,as$CALWT,0)
as$CURMLS_n = ifelse(as$fl>82 ,as$n,0)

as$USMLS_wt = ifelse(as$fl==82,as$CALWT/2,as$USMLS_wt)
as$USMLS_n = ifelse(as$fl==82,as$n/2,as$USMLS_n)

as$CURMLS_wt = ifelse(as$fl==82,as$CALWT/2,as$CURMLS_wt)
as$CURMLS_n = ifelse(as$fl==82,as$n/2,as$CURMLS_n)


asa = aggregate(cbind(USMLS_wt,USMLS_n,CURMLS_n,CURMLS_wt)~LFA,data=as,FUN=sum)

asa$prop_wt = asa$USMLS_wt/asa$CURMLS_wt
asa$prop_n = asa$USMLS_n/asa$CURMLS_n