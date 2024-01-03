#sex ratio

require(bio.lobster)
require(ggplot2)
require(devtools)

lobster.db('atSea.clean')


a = subset(atSea,SPECIESCODE==2550 & !is.na(CARLENGTH) & !is.na(SEX) &CARLENGTH<140 & CARLENGTH>50)


g = ggplot(subset(a,SEX==1), aes(CARLENGTH)) + geom_histogram( aes(x = CARLENGTH, after_stat(count)),
                                         binwidth = diff(range(a$CARLENGTH))/30, fill="blue") + 
  geom_histogram(data=subset(a,SEX %in% c(2,3)), aes(x = CARLENGTH, -after_stat(count)), binwidth = diff(range(a$CARLENGTH))/30, fill= "green")+
  facet_wrap(~LFA,scales='free_y')
print(g)

a$P=1
a$LEN=round(a$CARLENGTH)
a1 = aggregate(P~LEN+LFA,data=subset(a,SEX==1),FUN=sum)
a2 = aggregate(P~LEN+LFA,data=subset(a,SEX%in%c(2:3)),FUN=sum)
names(a2)[3]='Fem'

a3 = merge(a2,a1,all=T)
a3$PropF = a3$Fem/(a3$Fem+a3$P)

som=data.frame(LFA=c(27,28,29,30,'31A','31B',32, 33, 34, 35, 36, 38, 41),MLS=c(72,NA,79,80,80,80,82,84,84,90,92,92,92))
mls=data.frame(LFA=c(27,28,29,30,'31A','31B',32, 33, 34, 35, 36, 38, 41),MLS=c(82.5,84,84,rep(82.5,10)))

ggplot(a3,aes(x=LEN,y=PropF))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~LFA)+
  geom_vline(data=mls,aes(xintercept=MLS),color='red')+
  geom_vline(data=som,aes(xintercept=MLS),color='purple')


lobster.db('survey')
v = subset(surveyMeasurements,SPECCD_ID==2550 & !is.na(SEX) & !is.na(FISH_LENGTH))
v$Len = round(v$FISH_LENGTH)
v$P=1
a4 = aggregate(P~Len+LFA,data=subset(v,SEX==1),FUN=sum)
a5 = aggregate(P~Len+LFA,data=subset(v,SEX%in%c(2:3)),FUN=sum)
names(a5)[3]='Fem'

a6 = merge(a4,a5,all=T)
a6$PropF = a6$Fem/(a6$Fem+a6$P)

ggplot(a6,aes(x=Len,y=PropF))+
  geom_point()+
  geom_smooth()

ggplot(subset(a6,LFA %in% c('L34','L35','L36','L38')),aes(x=Len,y=PropF))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~LFA)


v$sizeclass = ifelse(v$Len>90,2,1)
v$P=1
v$YEAR = year(v$SET_DATE)
vv =aggregate(P~sizeclass+TRIP_ID+SET_NO+SET_LAT+SET_LON+YEAR,data=subset(v,SEX==1 & LFA =='L34'),FUN=sum)
vv1 =aggregate(P~sizeclass+TRIP_ID+SET_NO+SET_LAT+SET_LON+YEAR,data=subset(v,SEX%in%c(2,3)& LFA =='L34'),FUN=sum)
names(vv1)[7]='NFem'
va = merge(vv,vv1)
va$PropF = va$NFem/(va$P+va$NFem)
attr(va,'projection')<-"LL"

require(mgcv)
require(bio.lobster)
require(bio.utilities)
require(SpatialHub)

va = lonlat2planar(va,"utm20", input_names=c("SET_LON", "SET_LAT"))


f1 = formula(PropF~ as.factor(YEAR)+as.factor(sizeclass) + s(plon, plat,by=as.factor(sizeclass),bs='ts' ,k=100)) 


aa = gam(f1,data=va, family = betar(link='logit')) ##


#Predictions from full model
load(file.path(project.datadirectory('bio.lobster'),'data','predspace.rdata')) #sq km
LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
LFAs = lonlat2planar(LFAs,"utm20", input_names=c("X", "Y"))
LFAs = LFAs[,-which(names(LFAs)%in%c('X','Y'))]
LFAs = rename.df(LFAs,c('plon','plat'),c('X','Y'))

require(PBSmapping)
Ps = data.frame(EID=1:nrow(predSpace),predSpace[,c('plon','plat','z')])
Ps = rename.df(Ps,c('plon','plat','z'),c('X','Y','SET_DEPTH'))
key=findPolys(Ps,subset(LFAs,PID==34))
Ps = subset(Ps,EID%in%key$EID)
Ps = rename.df(Ps,c('X','Y'),c('plon','plat'))
Ps1 = Ps
Ps1$sizeclass=1
Ps2=Ps
Ps2$sizeclass=2

Ps = rbind(Ps1,Ps2)
# annual predictions
R1index=c()
R1area = list()
R1surface=list()
ilink <- family(aa)$linkinv   # this is the inverse of the link function
Years=2022
for(i in 1:length(Years)){
  require(mgcv)
  
  #Ps$dyear =Years[i]+.5
  Ps$YEAR =Years[i]
  
  plo1 = as.data.frame(predict(aa,subset(Ps,sizeclass==1),type='link',se.fit=TRUE))
  plo1$upper = ilink(plo1$fit - (1.96 * plo1$se.fit))  
  plo1$lower = ilink(plo1$1fit - (1.96 * plo1$se.fit))
  plo1$fitted = ilink(plo1$fit)
  xyz = data.frame(Ps[,c('plon','plat')],z=ilink(plo1$fit))
  corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
  planarMap( xyz, save=F,fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0,1,l=30), annot=Years[i],loc=fpf1, corners=corners,log.variable=F)
  
  plo2 = as.data.frame(predict(aa,subset(Ps,sizeclass==2),type='link',se.fit=TRUE))
  plo2$upper = ilink(plo2$fit - (1.96 * plo1$se.fit))  
  plo2$lower = ilink(plo2$1fit - (1.96 * plo1$se.fit))
  plo2$fitted = ilink(plo2$fit)
  xyz = data.frame(Ps[,c('plon','plat')],z=ilink(plo2$fit))
  corners = data.frame(lon=c(-67.8,-65),lat=c(42.5,45))
  planarMap( xyz, save=F,fn=paste("gamtwPAR1",Years[i],sep='.'), datascale=seq(0,1,l=30), annot=Years[i],loc=fpf1, corners=corners,log.variable=F)
  
  
}

