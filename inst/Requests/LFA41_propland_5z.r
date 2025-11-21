r = readRDS(file.path(git.repo,'bio.lobster.data','mapping_data','NAFO_sf.rds'))
g = lobster.db('process.logs41.unfiltered')
g$DDLON = g$DDLON*-1
r = subset(r,NAFO %in% c('5ZEJC','5ZEMC'))
gs = st_as_sf(subset(g,!is.na(DDLON)),coords=c('DDLON','DDLAT'),crs=4326)

grs = st_join(gs,r,join=st_within)

grs$yr = lubridate::year(grs$FV_FISHED_DATETIME)
to = aggregate(ADJCATCH_KG~yr,data=grs,FUN=sum)
fz = aggregate(ADJCATCH_KG~yr,data=subset(grs,!is.na(NAFO)),FUN=sum)
names(fz)[2] = 'AdjCatch5z'

tf = merge(to,fz)
