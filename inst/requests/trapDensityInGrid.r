loadfunctions( "lobster", functionname="initialise.local.environment.r") 
loadfunctions('utility')

LobsterMap('27',labels='grid',labcex=0.5)

LFAgrid<-read.csv(file.path( project.datadirectory("lobster"), "data","maps","GridPolys.csv"))
Grids = c(355,356,354)
Lgv = LFAgrid[which(LFAgrid$PID==27 & LFAgrid$SID %in% Grids),]

Lgvmc = cutOutLand(Lgv)
attr(Lgvmc,"projection") <-"LL"
#area of grid in sq km
g = calcArea(Lgvmc)
names(g)[2] = 'GRID_NUM'

 a = lobster.db('process.logs')
 a = a[which(a$GRID_NUM %in% Grids),]
 
 #sum of trap hauls
 	b = aggregate(NUM_OF_TRAPS~SYEAR+GRID_NUM+VR_NUMBER,data=a,FUN=sum)

 #season length
 	#d = aggregate(DATE_FISHED~SYEAR+GRID_NUM,data=a, FUN=function(x) c(max(x)-min(x)))

 	de = aggregate(DATE_FISHED~SYEAR+GRID_NUM+VR_NUMBER,data=a, FUN=length)


#trap hauls per day
 	d = merge(b,de)
 	d$TRAP_HAULS_P_D = d$NUM_OF_TRAPS / d$DATE_FISHED
 	da = aggregate(TRAP_HAULS_P_D~GRID_NUM+SYEAR,data =d, FUN=sum)
 #merge area and trap hauls per day to determine trap hauls per 100m2
 	da = merge(da,g,all.x=T)
 	da$area = da$area * 10000 

 	da$TrapDensityp100m2 = da$TRAP_HAULS_P_D / da$area 

dr = aggregate(TRAP_HAULS_P_D~GRID_NUM+area,data=da,FUN=mean)

 	dr$TrapDensityp100m2 = dr$TRAP_HAULS_P_D / (dr$area )
