
############# Scallop Survey	


	scalSurv<-ScallopSurveyProcess(size.range=c(0,200),bin.size=5)

 	#stupid little map
 	LobsterMap('34-38',title="Scallop Survey Tows by Month")
 	colors = brewer.pal(5,"Dark2")[c(rep(NA,4),1:5,NA,NA)]
	points(lat~lon,scalSurv,pch=16,col=colors[month(TOW_DATE)])
	legend('bottomright',legend=5:9,col=colors[5:9],pch=16,)


	# R1

	scalSurv<-ScallopSurveyProcess(size.range=c(70,82.5),bin.size=2.5)

	R1.ab.35=with(subset(scalSurv,LFA==35&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))
	R1.ab.36=with(subset(scalSurv,LFA%in%c(36,37)&YEAR>1998),tapply(LobDen,YEAR,mean,na.rm=T))


#plot scallop surveys

ff = "LFA35-38Assessment"
fpf1 = file.path(project.figuredirectory('bio.lobster'),ff)


#### lfa 35

png(file=file.path(fpf1,'S35recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(1999:2018,R1.ab.35,pch=16,xlab='Year',ylab = 'Recruit Abundance')
lines(1999:2018,runmed(R1.ab.35,3),lwd=2, col='salmon')
dev.off()


#### lfa 36

png(file=file.path(fpf1,'S36recruitabund.png'),units='in',width=15,height=12,pointsize=18, res=300,type='cairo')
plot(1999:2018,R1.ab.36,pch=16,xlab='Year',ylab = 'Recruit Abundance')
lines(1999:2018,runmed(R1.ab.36,3),lwd=2, col='salmon')
dev.off()


