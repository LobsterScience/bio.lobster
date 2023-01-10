require(bio.lobster)
require(bio.utilities)
require(lubridate)
a = lobster.db('atSea')
head(a)
head(atSea)
a = subset(atSea,SPECIESCODE==2550)
a = subset(atSea,SPECIESCODE==2550 & SEX %in% 2:3 & CARLENGTH>40 & CARLENGTH<130)
a$yr=year(a$STARTDATE)
a$syr = round(a$yr/5)*5

re = aggregate(TRIPNO~LFA+syr+CARLENGTH,data=a,FUN=length)
reb = aggregate(TRIPNO~LFA+syr+CARLENGTH,data=subset(a,SEX==3),FUN=length)
reb$N = reb$TRIPNO
reb$TRIPNO=NULL
rea = merge(re,reb,all=T)
rea = na.zero(rea)
rea$pB = rea$N/rea$TRIPNO


xG = subset(a,LFA %in% c('31A','31B'))
xG$Berried = ifelse(xG$SEX==3,1,0)
with(subset(xG,syr==2015),plot(CARLENGTH,pB))

df=data.frame(CARLENGTH=40:120)
xM = glm(Berried~CARLENGTH,data=subset(xG,syr==2015),family=binomial(link = 'logit'))
plot(df$CARLENGTH,predict(xM,type='response',newdata = df))


