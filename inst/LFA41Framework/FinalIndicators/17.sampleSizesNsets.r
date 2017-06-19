#sample sizes

require(bio.utilities)
options(stringsAsFactors=F)
#base

fd = file.path(project.datadirectory('bio.lobster'),'analysis','indicators')

a = dir(fd)

i = grep('restratified',tolower(a),invert=T)

dat = data.frame(yr = 1969:2015)

ll = c('sexratio','obslength','medianL')

for(j in i) {		
		k = read.csv(file.path(fd,a[j]))
		if(any(names(k)=='X')) k$X <- NULL
		if(any(names(k)=='Nsets')) {
	 k = k[,c('yr','Nsets','NsetswithLobster','ObsLobs')]
	
	l = strsplit(a[j],'\\.csv')
	names(k)[2:ncol(k)] = paste(names(k)[2:ncol(k)],l,sep='_')
			dat = merge(dat,k,by='yr',all.x=T)
	}
}
		
		write.csv(dat,file.path(project.datadirectory('bio.lobster'),'analysis','SummaryOfSetsSampleSizesLFA41Base.csv'))

#restratified not adjacent

fd = file.path(project.datadirectory('bio.lobster'),'analysis','indicators')

a = dir(fd)

i = grep('restratified',tolower(a),invert=F)
i2 = grep( 'adj',tolower(a),invert=T)
 i = intersect(i,i2)
dat = data.frame(yr = 1969:2015)

for(j in i) {		
		k = read.csv(file.path(fd,a[j]))
		if(any(names(k)=='X')) k$X <- NULL
		if(any(names(k)=='Nsets')) {
	 k = k[,c('yr','Nsets','NsetswithLobster','ObsLobs')]
	
	l = strsplit(a[j],'\\.csv')
	names(k)[2:ncol(k)] = paste(names(k)[2:ncol(k)],l,sep='_')
			dat = merge(dat,k,by='yr',all.x=T)
	}
}
		
		write.csv(dat,file.path(project.datadirectory('bio.lobster'),'analysis','SummaryOfSetsSampleSizesLFA41Restratified.csv'))


#restratified not adjacent

fd = file.path(project.datadirectory('bio.lobster'),'analysis','indicators')

a = dir(fd)

i = grep('restratified',tolower(a),invert=F)
i2 = grep( 'adj',tolower(a),invert=F)
 i = intersect(i,i2)
dat = data.frame(yr = 1969:2015)

for(j in i) {		
		k = read.csv(file.path(fd,a[j]))
		if(any(names(k)=='X')) k$X <- NULL
		if(any(names(k)=='Nsets')) {
	 k = k[,c('yr','Nsets','NsetswithLobster','ObsLobs')]
	
	l = strsplit(a[j],'\\.csv')
	names(k)[2:ncol(k)] = paste(names(k)[2:ncol(k)],l,sep='_')
			dat = merge(dat,k,by='yr',all.x=T)
	}
}
		
		write.csv(dat,file.path(project.datadirectory('bio.lobster'),'analysis','SummaryOfSetsSampleSizesLFA41adjRestratified.csv'))


