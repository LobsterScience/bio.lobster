require(PBSmapping)
load(file='/SpinDr/backup/bio_data/bio.lobster/data/maps/LFA27-33100mIsobath.rdata') #Isob100 the 100 m isobath for 27-33


	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
out=list()
j=0
#io = c(27,29,30,311,312,32,33)
io = c(27,29,30,311,312,32)
for(i in io){
	j = j+1
	ij = subset(LFAs,PID==i)
a = joinPolys(ij,Isob100,operation='INT')
attr(a,'projection') <- "LL"
aU = convUL(a)

out[[j]] = c(i,sum(calcArea(aU)$area))
}
o = do.call(rbind,out)

	p = lobster.db('seasonal.landings')
	g = lobster.db('annual.landings')

gg = apply(g,2,max,na.rm=T)

#max land
 as.data.frame(cbind(o,c(3844, 1088 ,  461, 962,   1270 ,  1289 ,  10049)))


  a=as.data.frame(cbind(o,gg[c(2,4,5,6,7,9)]))
  plot(log(a$V2),log(a$V3))


##
a27 = g$LFA27/o[1,2]
a29 = g$LFA29/o[2,2]
a30 = g$LFA30/o[3,2]
a311 = g$LFA31A/o[4,2]
a312 = g$LFA31B/o[5,2]
a32 = g$LFA32/o[6,2]

ggg = as.data.frame(cbind(g$YR,a27,a29,a30,a311,a312,a32))

matplot(ggg[,1],ggg[,2:7],type='l')
nn <- ncol(ggg[,2:7])
legend("top", colnames(ggg[,2:7]),col=seq_len(nn),cex=0.8,fill=seq_len(nn))




#just prune the polys
load(file='/SpinDr/backup/bio_data/bio.lobster/data/maps/LFA27-33100mIsobath.rdata') #Isob100 the 100 m isobath for 27-33


	LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))

out=list()
j=0
#io = c(27,29,30,311,312,32,33)
io = c(27,29,30,311,312,32)
for(i in io){
	j = j+1
	ij = subset(LFAs,PID==i)
a= joinPolys(ij,Isob100,operation='INT')
attr(a,'projection') <- "LL"
a$LFA==i
a$PID = a$PID+j 
out[[j]] = a
}

o = do.call(rbind,out)

