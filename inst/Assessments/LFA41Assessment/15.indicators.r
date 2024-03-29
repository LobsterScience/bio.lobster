# combine indicators
require(bio.utilities)
options(stringsAsFactors=F)
#base
p = bio.lobster::load.environment()
assessment.year = p$current.assessment.year ########### check the year ############### !!!!!!!!!!!

	fd = file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','indicators')

	dat = data.frame(yr = 1969:assessment.year)
	ny = nrow(dat)


			a =	c("DFO.restratified.All.csv",                                         			
				 "DFO.restratified.Mature.SexRatio.csv",                           
				 "Fec.maturefemaleLengthFrequenciesLFA41NEFSCfallrestratified.csv",  
				 "Fec.maturefemaleLengthFrequenciesLFA41NEFSCspringrestratified.csv",
				 "medianL.LengthFrequenciesLFA41NEFSCfallrestratified.csv",          
				 "medianL.LengthFrequenciesLFA41NEFSCspringrestratified.csv",        
				 "NEFSC.Fall.Restratified.All.csv",                                      
				 "NEFSC.fall.restratified.Mature.SexRatio.csv",                    
				 "NEFSC.Spring.Restratified.All.csv",                                    
				 "NEFSC.spring.restratified.Mature.SexRatio.csv",                  
				 "amo.csv",                                                          
				"commercialCatchrates.csv",                                         
				"DFO.Georges.All.csv",                                              
				"DFO.Georges.Mature.SexRatio.csv",                                
				"Fec.maturefemaleLengthFrequenciesLFA41dfogeorges.csv",             
				"Georges.Bank_no.season.obslength.csv",                                
				"Georges.Basin_no.season.obslength.csv",                               
				"lfa41DFOGeorgesTemps.csv",                                         
				"lfa41DFOSummerTemps.csv",                                          
				"lfa41NEFSCFallTemps.csv",                                          
				"lfa41NEFSCSpringTemps.csv",                                        
				"medianL.LengthFrequenciesLFA41dfogeorges.csv",
				"medianL.LengthFrequenciesLFA41polygonSummerRV.csv",
				"predatorIndex.csv",                                                
				"SE.Browns_no.season.obslength.csv",                                   
				"SW.Browns_no.season.obslength.csv",                                   
				"Fec.maturefemaleLengthFrequenciesLFA41polygonSummerRV.csv"
				)        

	ll = c('sexratio','obslength','medianL')


for(j in 1:length(a)) {		
		k = read.csv(file.path(fd,a[j]))
		if(any(names(k)=='X')) k$X <- NULL
		if(any(names(k)=='w.yst')) {
	if(any(names(k)=='gini')) {
			if(grepl('predator',a[j])) {
				k = k[,c('yr','n.yst','w.yst','NObsSPP')]; k = rename.df(k,'NObsSPP','ObsLobs')
			}else{
			k = k[,c('yr','n.yst','w.yst','gini','dwao','ObsLobs')]
			}
		
	}
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

			if(nrow(dat)>ny)browser()
	}
		
			
	write.csv(dat,file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','indicators','ReducedSummaryIndicatorsrestrat.csv'))

	rn = read.csv(file.path(project.datadirectory('bio.lobster'),'data','RENAME_INDICATORS_RESTRAT.csv'),header=F)
	dat = read.csv(file=file.path(project.datadirectory('bio.lobster'),'analysis','lfa41Assessment','indicators','ReducedSummaryIndicatorsrestrat.csv'))

	dat = dat[,which(names(dat) %in% rn[,1])]
	dd = rename.df(dat,rn[,1],rn[,2])

	t0 = 1970
	t1 = assessment.year
	rownames(dd) = 1969:assessment.year
	dd$YR = NULL
	dd$DFO_PREDATOR_BIOMASS <- NULL
jk = c("DFO_ABUNDANCE" , "NEFSC_FALL_REP_POT",     "NEFSC_SPRING_REP_POT",  "NEFSC_FALL","NEFSC_SPRING", "GEORGES_ABUNDANCE","GEORGES_REP_POT" ,"DFO_PREDATOR_ABUNDANCE", 
"DFO_REP_POT")       

			dd[,jk] <- log(dd[,jk]+1)

	fname = file.path(project.figuredirectory('bio.lobster'),'indicators')
	dir.create(fname)
                

    colorfun = colorRampPalette ( RColorBrewer::brewer.pal(9,"PRGn"))
 
	Y = pcaAnalyseData(dd, t0, t1,fname=fname,OFN='ReducedIndicatorsAssessment1', colscheme=colorfun,groupings=list(c(-0.5,19),c(19,40.5)))

	
 