# combine indicators
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
		if(any(names(k)=='w.yst')) {
	if(any(names(k)=='gini')) k = k[,c('yr','n.yst','gini','ObsLobs')]
	if(!any(names(k)=='gini')) k = k[,c('yr','n.yst','w.yst')]
	}
		if(any(names(k)=='fishingYear')) {
				k = rename.df(k,'fishingYear','yr')
				k = k[,c('yr','CPUE')]
				}
		l = strsplit(a[j],'\\.csv')
	if(grepl('All.csv',a[j])) k = k[,c('yr','n.yst','gini')]
	if(!grepl('All.csv',a[j]) & any(grep('n.yst',names(k)))) k = k[,c('yr','n.yst')]
		names(k)[2:ncol(k)] = paste(names(k)[2:ncol(k)],l,sep='_')
		if(grepl(paste(ll,collapse="|"),a[j])){
			lu = grep('ObsLobs',names(k))
			u = which(k[,lu]<20)
			k[u,2:ncol(k)] <- NA
		}
	
			lu = grep('ObsLobs',names(k))
			k[,lu] <- NULL
			dat = merge(dat,k,by='yr',all.x=T)
	}
		
		write.csv(dat,file.path(project.datadirectory('bio.lobster'),'analysis','SummaryIndicatorsbase.csv'))

rn = read.csv(file.path(project.datadirectory('bio.lobster'),'data','RENAME_INDICATORS.csv'),header=F)
dat = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','SummaryIndicatorsbase.csv'))

dat = dat[,which(names(dat) %in% rn[,1])]
dd = rename.df(dat,rn[,1],rn[,2])
ll = c('MEDLO','MEDUP')
dj = grep(paste(ll,collapse="|"),names(dd))
dd = dd[,-dj]

t0 = 1970
t1 = 2015
rownames(dd) = dd$YR
dd$YR = NULL
fname = file.path(project.figuredirectory('bio.lobster'),'indicators')
dir.create(fname)
Y = pcaAnalyseData(dd, t0, t1,fname=fname)

