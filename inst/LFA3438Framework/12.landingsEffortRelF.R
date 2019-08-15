#landings

require(bio.lobster)
require(PBSmapping)

a = lobster.db('annual.landings')
b = lobster.db('seasonal.landings')

b$YR = substr(b$SYEAR,6,9)
a = subset(a,YR<1976)
b = subset(b,YR>1975 & YR<2019)
fpf1 = file.path(project.figuredirectory('bio.lobster'),"LFA3438Framework2019")

LFA = c('LFA34','LFA35','LFA36','LFA38')
for(i in LFA){
		aa = a[,c('YR',i)]
		bb = b[,c('YR',i)]
		aa = rbind(aa,bb)
		file.name = paste('Landings',i,'.png',sep="")
		 png(file=file.path(fpf1,file.name),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
		plot(aa$YR,aa[,i],type='h',lwd=4,col='black',xlab='Year',ylab='Landings (t)')
		lines(aa$YR,runmed(aa[,i],3),col='salmon',lwd=3)
		dev.off()
}


