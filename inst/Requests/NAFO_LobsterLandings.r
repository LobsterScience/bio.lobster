###4x landings

excList = c(61,62,74,75,86,87,97,108,98,109,322)
halfList = c(49,63,76,88,99,110)

b = fisheryFootprintData(yrs=2015:2024)


b$Mult = ifelse(b$Grid %in% excList,0,1)
b$Mult = ifelse(b$Grid %in% halfList,0.5,b$Mult)
b = subset(b,LFA %in% c(33,34,35,36,38))

bb = aggregate(Landings*Mult~FishingYear,data=b,FUN=sum)

n = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','NAFO_sf.rds'))
n = subset(n, NAFO_1=='4X')

f = lobster.db('process.logs41')

require(sf)

nf = st_join(f,n)
nf = subset(nf,!is.na(AREA_ID) & LFA==41)

nfs = aggregate(ADJCATCH_KG~yr,data=nf,FUN=sum)

bx = merge(bb,nfs,by.x='FishingYear',by.y='yr')

bx$FourX = bx$`Landings * Mult`+bx$ADJCATCH_KG
write.csv(bx,'FourXLobsterLandings.csv')
###############################################################################################################
###4W landings

n = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','NAFO_sf.rds'))
gg = ggLobsterMap('27-33',return.object = T)
n = subset(n, NAFO_1=='4W')
gg+geom_sf(data=n)

#all of 29,30,31a,31b,32 and grid 322 in 33)

b = fisheryFootprintData(yrs=2005:2024)


b1 = subset(b,LFA %in% c(29,30,'31A','31B',32))
b2 = subset(b,LFA %in% 33 & Grid==322)
b = rbind(b1,b2)
bb = aggregate(Landings~FishingYear,data=b,FUN=sum)


f = lobster.db('process.logs41')

require(sf)

nf = st_join(f,n)
nf = subset(nf,!is.na(AREA_ID) & LFA==41)

nfs = aggregate(ADJCATCH_KG~yr,data=nf,FUN=sum)

bx = merge(bb,nfs,by.x='FishingYear',by.y='yr',all.x=T)

bx$FourW = rowSums(bx[,c('Landings','ADJCATCH_KG')],na.rm=T)
write.csv(bx,'FourWLobsterLandings.csv')
###############################################################################################################
###4vsw landings

n = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','NAFO_sf.rds'))
gg = ggLobsterMap('27-33',return.object = T)
n = subset(n, NAFO_2=='4VN')
gg+geom_sf(data=n)

#all of 27

b = fisheryFootprintData(yrs=2005:2024)


b1 = subset(b,LFA %in% c(27))
bb = aggregate(Landings~FishingYear,data=b1,FUN=sum)


write.csv(bb,'FourVNLobsterLandings.csv')

