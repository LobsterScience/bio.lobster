#temperature

load(file.path(project.datadirectory('bio.lobster'),'Temperature Data','tempIndicators.rdata'))
aa  = tempData
#aa$area = recode(aa$area,c("'27N'=27;'27S'=27;'33E'=33;'33W'=33"))

hj = unique(aa$area)

for(i in hj){
	li = subset(aa,area==i)

	ylims = range(c(li$t+li$t.sd,li$t-li$t.sd))
	plot(li$year,li$t,col='blue',type='b',lty=1,xlab='Year',ylab='Bottom Temperature',main=paste('LFA',i),ylim=c(1,13))
	arrows(li$year,y1=li$t+li$t.sd,y0 = li$t, length=0,col='blue')
	arrows(li$year,y1=li$t-li$t.sd,y0 = li$t, length=0,col='blue')
savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors',paste('FSRSTemp',i,'.png',sep="")),type='png')

	plot(li$year,li$t.sd/li$t,col='blue',type='b',lty=1,xlab='Year',ylab='CV Temperature',main=paste('LFA',i),ylim=c(.1,.8))
savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors',paste('FSRSTempCV',i,'.png',sep="")),type='png')	

}



boxplot(t.sd/t~area,data=aa,xlav='LFA',ylab = 'CV of temperature')
savePlot(file.path(project.figuredirectory('bio.lobster'),'AtSeaIndictors',paste('FSRSTempCVallLFAs.png',sep="")),type='png')	
