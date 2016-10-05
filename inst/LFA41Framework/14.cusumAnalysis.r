#cusum plots for reference points

require(bio.lobster)
la()
fp = file.path(project.datadirectory('bio.lobster'),'analysis')

a = c('nefsc.spring.restratified.bigci.rdata',
	'nefsc.fall.restratified.bigci.rdata',
	'dfo.summer.restratified.bigci.rdata',
	'dfo.georges.restratified.bigci.rdata'
	)


load(file.path(fp,a[1]))

ranges = aout[,c(26,76)] #first and third quartile