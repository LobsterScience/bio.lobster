#seasonal catch rates
	require(bio.lobster)
	require(lubridate)

a = lobster.db('cris')

#LFA 27
	ports = subset(a$crports, LFA_ID ==2 ) #LFA 27
	g = subset(cris.trips, PORT %in% ports$PORT)

	g$yr = year(g$STARTDATE)
	g$mon = month(g$STARTDATE)

#which years have Autumn samples
	yrU = unique(subset(g,mon %in% 9:11,select=yr))[,1]

 gS = subset(g, yr %in% yrU)
 gS = merge(gS,a$crports.csv[,c('PORTNAME','STATDISTRICT','PORT')],all.x=T, by='PORT')
 gS$NoSo = ifelse(gS$STATDISTRICT %in% 1:4, 'N','S')