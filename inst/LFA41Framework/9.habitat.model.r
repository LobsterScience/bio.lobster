#habitat model

			      p = bio.lobster::load.environment()
	require(bio.lobster)
	require(bio.utilities)
	loadfunctions('bio.utilities')
	loadfunctions('bio.temperature')
	loadfunctions('bio.habitat')
	loadfunctions('bio.indicators')
	loadfunctions('bio.spacetime')
	
	
	require(mgcv)
	la()

	b = habitat.model.data('nefsc.surveys',p=p)
	a = habitat.model.data('dfo.summer',p=p)
	d = habitat.model.data('dfo.georges',p=p)

dat = rbind(a,b,d)
	dat$yr = year(dat$timestamp)
	dat$z = log(dat$z)
	      #cant run    bM = formula( Y ~ te(dyear, bs="cs" ) + te(t, bs ='cs') + te(ddZ, bs="cs" )
	       #     + te(z,bs='cs') + te(dZ, bs="cs" )  + te(substrate.mean, bs="cs" )             
	      #     + te(plon, plat, k=100, bs="cs", by=as.factor(yr) ) + as.factor(yr)  )

save(dat,file=file.path(project.datadirectory('bio.lobster'),'analysis','habitatmodellingdata.rdata'))


    bM = formula( Y ~ s(dyear, bs="cr" ) + s(t, bs ='cr') + 
	            + s(z,bs='cr') + s(dZ, bs="cr" )         
	            + ti(plon, plat, k=100, bs="cs") + as.factor(yr)  )



	dat$Y = ifelse(dat$B>0,1,0)
	dat = subset(dat,yr %in% 1999:2016)

			   W = bam( bM, data=dat, family=binomial())
			   #model started on hyperion 901am Sept6 using screen 

			   #screen -r to reattach
			   #ctrl-a d to detach

			   
