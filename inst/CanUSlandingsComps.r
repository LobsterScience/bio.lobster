require(bio.lobster)

a = lobster.db('seasonal.landings')

b = read.table(file.path(project.datadirectory('bio.lobster'),'data','MaineLandingsto2015Tons.txt'),sep="\t",header=T)



a$Cana = rowSums(a[,c('LFA33','LFA34','LFA35','LFA36','LFA38')])
a$Year = substr(a$SYEAR,1,4)
a = a[,c('Year','Cana')]
a = subset(a,Year<2016)

b = subset(b,Year %in% 1975:2015)

plot(a,type='l')
lines(b,col='red')

require(bcp)
      h = bcp(log(a$Cana),w0 = 0.2, p0 = 0.05)
      plot(h,xaxlab=a$Year,xlab='Year')

     x11()
	 h = bcp(log(b$tons),w0 = 0.2, p0 = 0.05)
      plot(h,xaxlab=b$Year,xlab='Year')


#Annual

a = lobster.db('annual.landings')
a$LFA41 <- NULL
a$all = rowSums(a[,2:14],na.rm=T)
a$Year=a$YR
a = a[,c('Year','all')]
b = read.table(file.path(project.datadirectory('bio.lobster'),'data','MaineLandingsto2015Tons.txt'),sep="\t",header=T)

bb = merge(a,b)

bb$diff = bb$all-bb$tons


#what happened between 1984:1993
plot(bb$Year,bb$diff,type='h')