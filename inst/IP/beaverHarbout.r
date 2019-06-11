#beaver harbour

require(bio.lobster)
wd = file.path(project.datadirectory('bio.lobster'),'data','beaverharbour')

setwd(wd)

a = dir()
out = list()
m=0
for(i in a) {
	m=m+1
	out[[m]] = read.csv(i)
}

out = do.call(rbind,out)
out$DATE = as.Date((out$Date),format='%m/%d/%Y')
out$yr = year(out$DATE)
names(out)= c('Area','Site','Date','Quad','QuadSize','Kelp','RedFol','Encrust','Other','Muss','Soft','Shell','FG','Cob','Bould','Larger','Ledge','Species','SpeciesC','Size','Sex','Claws','Notes','DATE','yr')
out$ID = paste(out$Site,out$Date,out$Quad,sep="-")



a = aggregate(QuadSize~ID+yr,data=out,FUN=length)
ab = aggregate(QuadSize~yr,data=a,FUN=length)


dat = aggregate(Species~ID, data=subset(out,Species== "Homarus americanus" & Size<15),FUN=length)
names(dat)[2]='Yoy'


data = merge(a,dat,all=T)
data$Yoy[is.na(data$Yoy)] <- 0

#overall means
aa = aggregate(Yoy~yr,data=data,FUN=mean)


#bootstrapping

yy = unique(aa$yr)
niter = 100
out = matrix(NA,nrow=niter, ncol=length(yy))
for(j in 1:length(yy)){
	d = subset(data,yr==yy[j])
for(i in 1:niter){
		oo = sample(d$Yoy,length(d$Yoy),replace=T)
		out[i,j]=c(mean(oo))
	}
}

m = apply(out,2,quantile,c(0.025,.5,.975))
 plot(1991:2018,m[2,]/.25,type='b',ylim=c(0,11),xlab='Year',ylab='Density (n/m2)')
 arrows(x0=1991:2018,y1=m[2,]/.25,y0=m[1,]/.25,length=0)
 arrows(x0=1991:2018,y1=m[2,]/.25,y0=m[3,]/.25,length=0)



 