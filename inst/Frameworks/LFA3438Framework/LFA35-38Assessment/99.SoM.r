#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
a = read.csv(file.path(project.datadirectory('bio.lobster'),'BoFSoM2011UnpublishedResDocRecaptured.csv'))
nn = names(a)[2:7]

#simulating the binomial distribution based on the modelled probs
binSim <- function(data,ninds=100){
			names(data) = c('CL','P')
		ou = list()
		for(i in 1:nrow(data)){
			ou[[i]] = cbind(rep(data[i,'CL'],times=ninds),rbinom(n=ninds,size=1,prob=data[i,'P']))
			}
		out = as.data.frame(do.call('rbind',ou))
		names(out) = c('CL','Mat')
		return(out)
}

#combining areas for an LFA

Alma = a[,c(1,6)]
Alma = binSim(Alma)

#LFA 36
b = a[,c('CL',nn[5])]
names(b)[2]='xx'
g = a[,c('CL',nn[6])]
names(g)[2] = 'xx'
LFA36 = rbind(b,g)
Alma = binSim(LFA36)

##LFA 38

b = a[,c('CL',nn[3])]
names(b)[2]='xx'
g = a[,c('CL',nn[4])]
names(g)[2] = 'xx'
h = a[,c('CL',nn[5])]
names(h)[2] = 'xx'

LFA38 = rbind(rbind(b,g),h)
Alma = binSim(LFA38)

##
Alma = a[,c(1,6)]
Alma = binSim(Alma)

g = glm(Mat~CL,data=Alma,family=binomial(link='logit'))
l = seq(50,140,by=.1)

#correct CI's
ndata <- list(CL=l)
ndata = glmCIs(g,ndata)

with(Alma, plot(CL, Mat, pch = 16, xlab = "Carapace Length", ylab = "Maturity"))
lines(ndata$CL, ndata$fit_resp)
lines(ndata$CL, ndata$upr, lty=2)
lines(ndata$CL, ndata$lwr,lty=2)

with(ndata,{
	print( CL[which.min(abs(fit_resp-.5))])
	 print(CL[which.min(abs(upr-.5))])
	 print(CL[which.min(abs(lwr-.5))])
})	

ndata$p = ndata$fit_resp
ndata$cl = ndata$CL
	m1 = formula(p~1/(1+(exp(a+(b*cl)))))




ndata$p = ndata$p + runif(nrow(ndata),-.02,0.02)
	nls(m1,data=ndata,start=list(a=10,b=-.1))
#LFA 35 a=22.96, b=-0.24
#LFA 36 a=12.41 b=-0.127
#LFA 38 a=16.979 b=-0.178



plot(ndata$cl,ndata$p)
cc = function(x,a=22.96,b=-.24) 1/(1+(exp(a+(b*x))))
curve(cc,from=50, to=140,add=T)

cc = function(x,a=12.41,b=-.127) 1/(1+(exp(a+(b*x))))
curve(cc,from=50, to=140,add=T,col='blue')


cc = function(x,a=16.979,b=-.178) 1/(1+(exp(a+(b*x))))
curve(cc,from=50, to=140,add=T,col='red')




