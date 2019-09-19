#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','LobsterMaturityDatabase.csv'))

unique(a$X)

 a$Y = a$Lat
 a$X = a$Long*-1
 a$EID = 1:nrow(a)
 a$Date = as.Date(a$Date,"%d/%m/%Y")
 a$mon = month(a$Date)


 LobsterMap('SWN')
 addPoints(na.omit(a[,c('X','Y','EID')]),col='red')

 b = subset(a,Location=='Lobster Bay')

b$Mat = ifelse(b$CEMGLAND_STAGE<2,0,1)
b = subset(b,CARLENGTH<120 & CARLENGTH>60)
g = glm(Mat~CARLENGTH,data=subset(b,mon %in% 6:7 & SEX==2),family=binomial(link='logit'))

plot(b$CARLENGTH, b$Mat, pch = 16, xlab = "Carapace Length", ylab = "Maturity")
l = seq(min(b$CARLENGTH),max(b$CARLENGTH),by=.1)
mm =predict(g,list(CARLENGTH=l),type='response')
lines(l,mm)
l[which.min(abs(mm-.5))]



#correct CI's
ilink <- family(g)$linkinv 
ndata <- as.data.frame(do.call(cbind,predict(g, list(CARLENGTH=l), se.fit = TRUE,type='link')[1:2]))
ndata <- within(ndata, {	CL = l
						fit_resp = ilink(fit)
						upr = ilink(fit + (2*se.fit))
						 lwr = ilink(fit - (2 *se.fit))
						 })

plot(b$CARLENGTH, b$Mat, pch = 16, xlab = "Carapace Length", ylab = "Maturity")
lines(ndata$CL, ndata$fit_resp)
lines(ndata$CL, ndata$upr, lty=2)
lines(ndata$CL, ndata$lwr,lty=2)

with(ndata,{
	print( CL[which.min(abs(fit-.5))])
	 print(CL[which.min(abs(upr-.5))])
	 print(CL[which.min(abs(lwr-.5))])
})	
