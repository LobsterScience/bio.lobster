#SMB
require(bio.lobster)
g = lobster.db('process.logs.unfiltered')
g = subset(g, LFA==34)
gG = unique(g$GRID_NUM)

gS = subset(g, GRID_NUM %in% c(69,81,92) & LFA ==34)
gF = subset(g, GRID_NUM %ni% c(69,81,92) & LFA == 34)

agS = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=gS,FUN=sum)
agF = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=gF,FUN=sum)

agS$CPUE = agS$WEIGHT_KG/agS$NUM_OF_TRAPS
plot(agS$SYEAR,agS$WEIGHT_KG)
plot(agS$SYEAR,agS$NUM_OF_TRAPS)
plot(agS$SYEAR,agS$CPUE)


a <- lm(WEIGHT_KG/1000~SYEAR,data=agS[19:21,])
qq=predict(a)
(qq[3]-qq[1])/qq[1]


a <- lm(NUM_OF_TRAPS/1000~SYEAR,data=agS[19:21,])
qq=predict(a)
(qq[3]-qq[1])/qq[1]

a <- lm(CPUE~SYEAR,data=agS[19:21,])
qq=predict(a)
(qq[3]-qq[1])/qq[1]

#grids

LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
grL = subset(LFAgrid, PID==34)
grL$area=grL$PID
attr(grL,'projection') <- 'LL'

#grs is the grids
#making the list of connections to grids
require(sp)
require(spdep)
g = split(grL,f=grL$SID)
nm = c()
gp = list()
for(i in 1:length(g)){
  gp[[i]] = Polygons(list(Polygon(g[[i]][,c('X','Y')])),unique(g[[i]]$SID))
}
gpp = SpatialPolygons(gp,proj4string=CRS("+proj=longlat +datum=WGS84"))
gpnb = poly2nb(gpp,row.names=names(gpp))
names(gpnb)=names(gpp)

out=list()

for(i in 1:length(gpnb)){
  w=c()
  gr = names(gpnb)[gpnb[[i]]]
  gr = sample(gr,3)
  tr = subset(g, GRID_NUM %in% gr )
    trr = aggregate(cbind(WEIGHT_KG,NUM_OF_TRAPS)~SYEAR,data=tr,FUN=sum)
    
    trr$CPUE = trr$WEIGHT_KG/trr$NUM_OF_TRAPS
    a <- lm(WEIGHT_KG/1000~SYEAR,data=subset(trr, SYEAR %in% c(2017, 2018, 2019)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    a <- lm(NUM_OF_TRAPS/1000~SYEAR,data=subset(trr, SYEAR %in% c(2017, 2018, 2019)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    a <- lm(CPUE~SYEAR,data=subset(trr, SYEAR %in% c(2017, 2018, 2019)))
    qq=predict(a)
    w = c(w,(qq[3]-qq[1])/qq[1])
    out[[i]]=w
    }

oo = do.call(rbind,out)

hist(oo[,1]*100,main='Percent Change in Landings (2017-2019)','fd',xlab="")
abline(v=-44.9, col='red')

hist(oo[,2]*100,main='Percent Change in Effort (2017-2019)','fd',xlab="")
abline(v=-26.7,col='red')


hist(oo[,3]*100,main='Percent Change in CPUE (2017-2019)','fd',xlab="")
abline(v=-25.6,col='red')

median(oo[,3])


#survey data.
lobster.db('survey')

SL<-LobsterSurveyProcess(lfa="34", yrs=1996:2020, mths=c("Aug","Jul","Jun"), bin.size=5, Net='NEST',size.range=c(0,200),biomass=T)
    LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
    grL = subset(LFAgrid, PID==34)
    grL$area=grL$PID
    attr(grL,'projection') <- 'LL'
    SMB = subset(grL, SID %in% c(69,81,92))
    SMB$PID = SMB$SID
    smb = joinPolys(SMB, operation = 'UNION')

SL$EID = 1:nrow(SL)
SL$X = SL$SET_LONG
SL$Y = SL$SET_LAT
Stsmb = findPolys(SL,smb)
Stsmb = subset(SL, EID %in% Stsmb$EID)

aggregate(SET_ID~YEAR, data=Stsmb,FUN=length)

plot(aggregate(NUM_STANDARDIZED~YEAR, data=Stsmb, FUN=mean))
lines(aggregate(NUM_STANDARDIZED~YEAR, data=SL, FUN=mean),col='red')

aa = Stsmb[,c(1,2,grep('CL',names(Stsmb)))]
require(bio.utilities)
aa = na.zero(aa)
aa$NSizes = rowSums(aa>0)-2

aggregate(NSizes~YEAR,data=aa,FUN=median)
