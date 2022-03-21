
require(bio.lobster)
require(bio.utilities)
require(RODBC)
require(lubridate)
require(devtools)
require(mgcv)
require(statmod)
options(stringAsFactors=F)
la()
require(PBSmapping)

wd = ('C:\\Users\\Cooka\\OneDrive - DFO-MPO\\BycatchLobster')
setwd(wd)

aA = read.csv(file=file.path('results','CompliedDataForModelling.csv'))


###predicting lobster landings from obs
gPT = gam(LobsterWt~s(WOS)+LFA,data=aA,family=Tweedie(p=1.5, link=log),method='REML')


#subset aA to where we dont have good location info

aAr = subset(aA, !is.na(GridGroup))

gt = glm(LegalWt~Period+GP+SYEAR,data=aAr,family=tweedie(var.power=1.5, link.power=0)) #link power 0 is log

b$GP = paste(b$LFA, b$GridGroup,sep="-")
newd = aggregate(cbind(NUM_OF_TRAPS, WEIGHT_KG)~SYEAR+GP+Period,data=b,FUN=sum)
newd$pred = predict(gt,newdata = newd, type='response')
newd$TotW = newd$pred * newd$NUM_OF_TRAPS

#predictions are biased low at highest catch rates and time intervals
plot(newd$WEIGHT_KG,newd$TotW)
abline(a=0,b=1)
cor.test(newd$WEIGHT_KG,newd$TotW)



#merging matching grid groupings sampled across periods -- when and where sampled did a good job
rW = aggregate(LegalWt~Period+GP+SYEAR,data=aA, FUN=mean)
newdMerge = merge(newd,rW)
newdMerge$Raw2Total = newdMerge$NUM_OF_TRAPS * newdMerge$LegalWt
with(newdMerge,plot(TotW,Raw2Total))
abline(a=0,b=1)
with(newdMerge,cor.test(TotW,Raw2Total))




gt = gam(LegalWt~(Period)+(GP)+SYEAR,data=aA,family=)



