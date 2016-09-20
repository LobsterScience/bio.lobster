#habitat model

			      p = bio.lobster::load.environment()
	require(bio.lobster)
	require(bio.utilities)
	loadfunctions('bio.utilities')
	loadfunctions('bio.temperature')
	loadfunctions('bio.habitat')
	loadfunctions('bio.indicators')
	loadfunctions('bio.spacetime')
	
	#Prediction surface




	require(mgcv)
	la()

	p$years = c(1969:2016)

	b = habitat.model.data('nefsc.surveys',p=p)
	a = habitat.model.data('dfo.summer',p=p)
	d = habitat.model.data('dfo.georges',p=p)
	pre = habitat.model.data('dfo.georges',p=p)


dat = rbind(a,b,d)
	dat$yr = year(dat$timestamp)
	dat$z = log(dat$z)
	      #cant run    bM = formula( Y ~ te(dyear, bs="cs" ) + te(t, bs ='cs') + te(ddZ, bs="cs" )
	       #     + te(z,bs='cs') + te(dZ, bs="cs" )  + te(substrate.mean, bs="cs" )             
	      #     + te(plon, plat, k=100, bs="cs", by=as.factor(yr) ) + as.factor(yr)  )

save(dat,file=file.path(project.datadirectory('bio.lobster'),'analysis','habitatmodellingdata.rdata'))

###GAM
    bM = formula( Y ~ s(dyear, bs="cr" ) + s(t, bs ='cr') + 
	            + s(z,bs='cr') + s(dZ, bs="cr" )         
	            + ti(plon, plat, k=100, bs="cs") + as.factor(yr)  )



	dat$Y = ifelse(dat$B>0,1,0)
#	dat = subset(dat,yr %in% 1999:2016)

			   W = bam( bM, data=dat, family=binomial())
			   #model started on hyperion 901am Sept6 using screen finished sometime between friday night and monday am
			   save(W,file='/backup/bio_data/bio.lobster/R/LobitatModelSept122016.rdata') 

load(file='/backup/bio_data/bio.lobster/R/LobitatModelSept122016.rdata') 

			   #screen -r to reattach
			   #ctrl-a d to detach



###Simple no space GAM
    bM = formula( Y ~ s(dyear, bs="cr" ) + s(t, bs ='cr') + 
	            + s(z,bs='cr') + s(dZ, bs="cr" )         
	            + s(yr)  )
	dat$Y = ifelse(dat$B>0,1,0)
#	dat = subset(dat,yr %in% 1999:2016)

			   W2 = bam( bM, data=dat, family=binomial())
			   #model started on hyperion 901am Sept6 using screen finished sometime between friday night and monday am
			   save(W,file='/backup/bio_data/bio.lobster/R/LobitatModelnospace') 

			   #screen -r to reattach
			   #ctrl-a d to detach

#temp by year			   

    bM = formula( Y ~ s(dyear, bs="cr" ) + s(t, bs ='cr',by=as.factor(yr)) + 
	            + s(z,bs='cr') + s(dZ, bs="cr" )         
	            + s(yr)  )



#	dat = subset(dat,yr %in% 1999:2016)

			   W3 = bam( bM, data=dat, family=binomial())



#Climate Envelop Model Booth 2014 Diversity and Distributions 20:1-9

require(dismo)

load(file.path(project.datadirectory('bio.lobster'),'analysis','habitatmodellingdata.rdata'))
#presence only
	dat$Y = ifelse(dat$B>0,1,0)
	dat.p = subset(dat,Y==1 & dyear <0.5)
	bc <- bioclim(dat.p[,c('t','z','dZ','ddZ')])

bcp = predict(dat.p,bc)
