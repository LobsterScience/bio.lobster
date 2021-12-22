#pruning NEFSC surveys

LFA41 = read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFA41Offareas.csv"))
LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

L = subset(LFAS,PID %in% 33:38)
LU = joinPolys(L,operation='UNION')
LU = subset(LU,SID==3)
L4 = joinPolys(LFA41,operation = 'UNION')
L4 = subset(L4,SID==1)
LC = joinPolys(LU,L4,operation='UNION')
LC = subset(LC, SID==1)

a = importShapefile(find.bio.gis('BTS_Strata'),readDBF=T) 
l = attributes(a)$PolyData[,c('PID','STRATA')]
a = merge(a,l,by='PID',all.x=T)
aa = subset(a,STRATA %in% c(1160, 1170, 1180, 1210, 1220, 1290, 1300, 1340, 1360,1330,1351,1352,3900))
aa$PID = aa$STRATA
aaa = joinPolys(aa,LC,operation = 'INT')
aaa = subset(aaa,SID==1)
aa = subset(aa,SID==1)

plotPolys(LFAs)
addPolys(aa,border='red')
caaa = calcArea(aaa)
caa = calcArea(aa)
caaa$Prop=caaa[,3]/caa[,3]

####length weight

a = groundfish.db('gsdet')
b =   nefsc.db(DS='usdet.clean') ### dont use the NEFSC len wt data its from a regression already
a = subset(a, spec==2550 & !is.na(mass) & !is.na(len), select=c(len, mass))
a$lWt = log(a$mass); a$llen = log(a$len)
out = rlm(lWt~llen,data=a)


####Maturity matrix
l = seq(55,228,by=5) #mids of breaks
p$Area = '34'
pMat(p,cl=l,lfa='LFA34')
cX(t(pMat(p,cl=l,lfa='LFA34')))
