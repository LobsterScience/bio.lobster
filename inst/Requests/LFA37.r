#LFA 37

require(bio.lobster)
require(bio.utilities)
require(PBSmapping)
require(devtools)
require(SpatialHub)
la()
p = list()
p$lfas = 38
 logsInSeason<-lobster.db('process.logs.unfiltered')

grids = c(39,40,41,42) 

s = subset(logsInSeason,LFA==38 & SYEAR>2020)
s$id = paste(s$DATE_FISHED,s$LICENCE_ID)
sr = subset(s,GRID_NUM %in% grids) 
tr = aggregate(id~SYEAR+LICENCE_ID,data=sr,FUN=function(x) length(unique(x)))
tri = aggregate(id~SYEAR+LICENCE_ID,data=subset(s,LICENCE_ID %in% tr$LICENCE_ID),FUN=function(x) length(unique(x)))
names(tri)[3] = 'TotalTrips'
names(tr)[3] = 'L37Trips'

tt = merge(tri,tr,all.x=T)
tt = bio.utilities::na.zero(tt)
tt$propTrips = tt$L37Trips/tt$TotalTrips


s$L37 = ifelse(s$GRID_NUM %in% grids,1,0)

s$W37 = s$WEIGHT_KG*s$L37

gr = aggregate(cbind(WEIGHT_KG,W37)~SYEAR+LICENCE_ID,data=subset(s,LICENCE_ID %in% unique(tr$LICENCE_ID)),FUN=sum)
gr$prop37 = gr$W37/gr$WEIGHT_KG

####################################################

#how many 38 licences have used LFA 37 grids in the last 5 years
length(unique(tt$LICENCE_ID)) #28

#how many 38 licences use LFA 37 grids in 3 of last 5 years
v =aggregate(SYEAR~LICENCE_ID,data=subset(tt,L37Trips>0),FUN=length)
nrow(v[v$SYEAR>2,]) #9

freq37 = (v[v$SYEAR>2,'LICENCE_ID']) #9
gt = aggregate(cbind(W37,WEIGHT_KG)~LICENCE_ID,data=subset(gr,LICENCE_ID %in% freq37),FUN=sum)
gt$prop = gt$W37/gt$WEIGHT_KG
#For those harvesters that fish in these grids what is the total proportion of  landings caught in LFA 37 over hte last 5 years
sum(gr$W37)/sum(gr$WEIGHT_KG) # 0.23

#how many harvesters consistently (min 3 of last 5 yrs) land >50% of landings in 37
bb = aggregate(SYEAR~LICENCE_ID,data=subset(gr,prop37>.20),FUN=length)
nrow(bb[bb$SYEAR>2,]) #6

#when do people fish in LFA 37
sr = subset(s,GRID_NUM %in% grids) 
aggregate(id~WOS,data=sr,FUN=function(x) length(unique(x)))







###################################################

ggplot(gr,aes(x=SYEAR,y=prop37,colour=LICENCE_ID))+geom_path()


catchLevels = c(0,100000,200000,300000,400000,500000,600000,700000,800000)
	yrs = 2014:2020
	for(i in 1:length(yrs)){
		catchgrids = lobGridPlot(subset(logsInSeason,LFA%in%p$lfas&SYEAR==yrs[i],c("LFA","GRID_NUM","TOTAL_WEIGHT_KG")),FUN=sum,lvls=catchLevels)
		saveRDS(catchgrids,file=file.path(fd,paste('Figure3',yrs[i],'.rds')))
#		pdf(file.path(figdir,paste0("FisheryFootprint",yrs[i],".pdf")))
		LobsterMap('37',poly.lst=catchgrids)
	  	title(yrs[i],line=-3,cex.main=2,adj=0.3)
	    SpatialHub::contLegend('bottomright',lvls=catchgrids$lvls/1000,Cont.data=catchgrids,title="Catch (tons)",inset=0.02,cex=0.8,bg='white')
#	    dev.off()
	}
