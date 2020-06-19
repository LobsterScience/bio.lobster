#lfa41 soak times
require('bio.lobster')
require('bio.utilities')
lobster.db('logs41')

logs41$FV_FISHED_DATETIME = as.Date(logs41$FV_FISHED_DATETIME)

yr = unique(year(logs41$FV_FISHED_DATETIME))

logs41$YR = year(logs41$FV_FISHED_DATETIME)

l15 = subset(logs41,YR==2019)
l = makePBS(l15,polygon=F)
l = na.omit(l)

l = l[order(l$FV_FISHED_DATETIME),]

a = unique(l$FV_FISHED_DATETIME)


y = l
l$Poly = NA
o=0
center.circle = NULL
circle.list = list()
LobsterMap('41')
while(nrow(y)>0) {
	o=o+1
	center.circle = rbind(center.circle,c(as.numeric(y[1,c('X','Y')]),o))
	Ci = bufferCircle(center.circle[o,1:2],5)
	Ci$PID = o
	Ci$POS = 1:nrow(Ci)
	Ci$X = Ci$lon
	Ci$Y = Ci$lat
	attr(Ci,'projection') = "ll"
	circle.list[[o]] = Ci
	id = findPolys(y,Ci)
	addPolys(Ci)
	l[which(l$EID %in% id$EID),'Poly'] = o

	y = y[-which(y$EID %in% id$EID),]
}

aggregate(EID~Poly,data=l,FUN=length)


gr = makeGrid(x=seq(min(l$X),max(l$X),length.out=100),y=seq(min(l$Y),max(l$Y),length.out=160),projection="LL",zone=20)
g = findPolys(l,gr)
l = merge(l,g,by=c('EID'),all.x=T)
l$id = paste(l$PID,l$SID,sep="-")

io = unique(l$id)
w=NULL
for(i in 1:length(io)) {
		x = l[which(l$id==io[i]),] 	
		if(length(x)>1){
		x = x[order(x$FV_FISHED_DATETIME),'FV_FISHED_DATETIME']
		w = c(w,x[2:length(x)] - x[1:(length(x)-1)] )
			}
	}