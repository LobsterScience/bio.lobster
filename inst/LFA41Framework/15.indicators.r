# combine indicators
require(bio.utilities)
options(stringsAsFactors=F)
#base

fd = file.path(project.datadirectory('bio.lobster'),'analysis','indicators')

a = dir(fd)

i = grep('restratified',tolower(a),invert=F)
j = grep('adj',tolower(a),invert=T)
k = grep('base',tolower(a),invert=T)
l = grep('restratified',tolower(a),invert=T)

dat = data.frame(yr = 1969:2015)


		a =	c("DFO.restratified.All.csv",                                         
			 "DFO.restratified.commercial.csv",                                  
			 "DFO.restratified.immature.SexRatio.csv  ",                         
			 "DFO.restratified.LargeFemale.csv",                                 
			 "DFO.restratified.Mature.SexRatio.csv  ",                           
			 "DFO.restratified.recruits.csv",                                    
			 "Fec.maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.csv",  
			 "Fec.maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.csv",
			 "medianL.LengthFrequenciesLFA41NEFSCfallrestratified.csv",          
			 "medianL.LengthFrequenciesLFA41NEFSCspringrestratified.csv",        
			 "NEFSC.fall.restratified.commercial.csv",                           
			 "NEFSC.Fall.Restratified.All.csv",                                      
			 "NEFSC.fall.restratified.immature.SexRatio.csv  ",                  
			 "NEFSC.fall.restratified.Mature.SexRatio.csv  ",                    
			 "NEFSC.fall.restratified.recruits.csv",                             
			 "NEFSC.spring.restratified.commercial.csv",                         
			 "NEFSC.Spring.Restratified.All.csv",                                    
			 "NEFSC.spring.restratified.immature.SexRatio.csv  ",                
			 "NEFSC.spring.restratified.Mature.SexRatio.csv  ",                  
			 "NEFSC.spring.restratified.recruits.csv",                          
			 "restratified.NEFSC.Fall.LargeFemale.csv",                          
			 "restratified.NEFSC.Spring.LargeFemale.csv",
			"amo.csv",                                                          
			"commercialCatchrates.csv",                                         
			"DFO.Georges.All.csv",                                              
			"DFO.Georges.commercial.csv",                                       
			"DFO.Georges.LargeFemale.csv",                                      
			"DFO.Georges.Mature.SexRatio.csv  ",                                
			"DFO.Georges.recruits.csv",                                         
			"Fec.maturefemaleLengthFrequenciesLFA41dfogeorges.csv",             
			"Georges.Bank.Summer_obslength.csv",                                
			"Georges.Basin.Spring_obslength.csv",                               
			"Georges.Basin.Summer_obslength.csv",                               
			"giniCPUE.csv",                                                     
			"lfa41DFOGeorgesTemps.csv",                                         
			"lfa41DFOSummerTemps.csv",                                          
			"lfa41NEFSCFallTemps.csv",                                          
			"lfa41NEFSCSpringTemps.csv",                                        
			"medianL.LengthFrequenciesLFA41dfogeorges.csv",
			"medianL.LengthFrequenciesLFA41polygonSummerRV.csv",
			"predatorIndex.csv",                                                
			"SE.Browns.Spring_obslength.csv",                                   
			"SW.Browns.Autumn_obslength.csv",                                   
			"SW.Browns.Summer_obslength.csv",
			'sdmhabitat.csv',
			"Fec.maturefemaleLengthFrequenciesLFA41polygonSummerRV.csv"
			)        

ll = c('sexratio','obslength','medianL')

for(j in 1:length(a)) {		
		k = read.csv(file.path(fd,a[j]))
		if(any(names(k)=='X')) k$X <- NULL
		if(any(names(k)=='w.yst')) {
	if(any(names(k)=='gini')) k = k[,c('yr','n.yst','w.yst','gini','dwao','ObsLobs')]
	if(!any(names(k)=='gini')) k = k[,c('yr','n.yst','w.yst')]
	}
		if(any(names(k)=='fishingYear')) {
				k = rename.df(k,'fishingYear','yr')
				k = k[,c('yr','CPUE')]
				}
		l = strsplit(a[j],'\\.csv')

	if(grepl('All.csv',a[j])) k = k[,c('yr','n.yst','gini','dwao','ObsLobs')]
	if(grepl('NEFSC.Fall',a[j])) k = k[,c('yr','n.yst','gini','dwao','ObsLobs')]
	if(grepl('NEFSC.Spring',a[j])) k = k[,c('yr','n.yst','gini','dwao','ObsLobs')]
		if(!grepl('pred',a[j])){
	if(!grepl('All.csv',a[j]) & any(grep('n.yst',names(k)))) k = k[,c('yr','n.yst','ObsLobs')]
		}
		names(k)[2:ncol(k)] = paste(names(k)[2:ncol(k)],l,sep='_')
		#if(grepl(paste(ll,collapse="|"),a[j])){
			if(any(grepl('ObsLobs',names(k)))){
			lu = grep('ObsLobs',names(k))
			u = which(k[,lu]<20)
			k[u,2:ncol(k)] <- NA
		}
		if(any(grepl('Shann',names(k)))) {
			o = grep('Shann',names(k))
			k = k[,-o]
		}
	
			lu = grep('ObsLobs',names(k))
			k[,lu] <- NULL
			dat = merge(dat,k,by='yr',all.x=T)
	}
		
#		write.csv(dat,file.path(project.datadirectory('bio.lobster'),'analysis','SummaryIndicatorsrestrat.csv'))

	rn = read.csv(file.path(project.datadirectory('bio.lobster'),'data','RENAME_INDICATORS_RESTRAT.csv'),header=F)
	dat = read.csv(file.path(project.datadirectory('bio.lobster'),'analysis','SummaryIndicatorsrestrat.csv'))

	dat = dat[,which(names(dat) %in% rn[,1])]
	dd = rename.df(dat,rn[,1],rn[,2])

	dd

	t0 = 1970
	t1 = 2015
	rownames(dd) = 1969:2015
	dd$YR = NULL
	dd = dd[,c(-5, -6, -8, -21, -26, -28, -37,-39,-19)]
	jk = c(1,3,4,6,7,12,13,15,17,18,20,22,23,25,26,28,29,31,47,48,56)

	for(i in jk){
		dd[,jk] <- log(dd[,jk]+1)
	}

	fname = file.path(project.figuredirectory('bio.lobster'),'indicators')
	dir.create(fname)
	Y = pcaAnalyseData(dd, t0, t1,fname=fname)

