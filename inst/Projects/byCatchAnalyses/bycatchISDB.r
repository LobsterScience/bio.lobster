#bycatch

require(bio.lobster)
require(bio.utilities)
require(lubridate)
require(PBSmapping)

lobster.db('atSea')

da = subset(atSea,DESCRIPTION =='ISDB' & LFA %in% c(33,34,35) & STARTDATE>'2018-10-10')
da = makePBS(da,polygon=F)

length(unique(da$TRIPNO))

gg = read.csv(file.path(project.datadirectory('bio.lobster'),'ByCatchProject','Grids.csv'))

LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), 
"data","maps","GridPolys.csv"))

a = findPolys(da,LFAgrid)
da = merge(da,a[,c('EID','SID')],all=T)
da = rename.df(da,'SID','GRID_NUM')
da = merge(da,gg[,c('LFA','GRID_NUM','GridGrouping')],by=c('LFA','GRID_NUM'))

da$SYEAR = year(da$STARTDATE)
da$mon = month(da$STARTDATE)
da$SYEAR = ifelse(da$mon %in% 10:12,da$SYEAR+1,da$SYEAR)

#ntraps per trip and grid group
##something not adding up correctly here I think. Need to check with CD
##missing trips 100053750 and 100053869

lobster.db('atSea')

da = subset(atSea,DESCRIPTION =='ISDB' & LFA %in% c(33,34,35) & STARTDATE>'2018-10-10')
da = makePBS(da,polygon=F)


da$ids = paste(da$STRINGNO,da$TRAPNO,sep='-')
tt = aggregate(ids~LFA+GridGrouping+TRIPNO,data=da,FUN=function(x) length(unique(x)))

aggregate(TRIPNO~LFA+GridGrouping,data=da,FUN=function(x) length(unique(x)))