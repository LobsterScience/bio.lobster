#cusum plots for reference points

require(bio.lobster)
la()
fp = file.path(projectd.datadirectory('bio.lobster'),'analysis')

a = c('stratified.nefsc.fall.LFA41.restratified.length.50-300.not.sexed.rdata',
	'stratified.nefsc.spring.LFA41.restratified.length.50-300.not.sexed.rdata',
	'stratified.summer.LFA41.restratified.length.all.not.sexed.rdata',
	'stratified.georges.Georges.Canada.base.length.all.not.sexed.rdata'
	)