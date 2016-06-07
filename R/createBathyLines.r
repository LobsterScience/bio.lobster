createBathyLines<-function(input.fp=file.path( project.datadirectory("lobster"), "data", "maps","topex"), input.fn="topex.xyz",redo=T,interval=10,batch=100,range=c(10,5000),save.by=1000,digits=3,dneg=T,output.fn="bathyPoly"){

	# function to create bathy lines from xyz data
	# topex bathymetry downloaded from http://topex.ucsd.edu/cgi-bin/get_data.cgi specify extent in website to get data)
	require(PBSmapping)

		
	if(redo){
		bathy.dat<-read.table(file.path( input.fp, input.fn),header=F)
		names(bathy.dat)<-c("X","Y","Z")
		bathy.dat$X<-bathy.dat$X-360
		bathy.lst<-makeTopography(bathy.dat,digits=digits)
		save( bathy.lst, file=file.path(input.fp,paste0(input.fn,".rdata")))
	}
	else load(file.path(input.fp,paste0(input.fn,".rdata")))

	tmp.lst<-list()
	for(i in 1:(range[2]/batch)){

		print(i/(range[2]/batch)*100)
		
		isobath=seq(interval+batch*(i-1),batch*i,interval)*ifelse(dneg,-1,1)
		bathy.cl<-contourLines(bathy.lst,levels=isobath)
		bathy.cp <- convCP(bathy.cl)
		tmp.lst[[i]] <- bathy.cp$PolySet
		tmp.lst[[i]]$PID <- tmp.lst[[i]]$PID+batch*(i-1)/interval
		tmp.lst[[i]]$Z <- tmp.lst[[i]]$PID*interval

	}

	for(i in 1:(range[2]/save.by)){
		x<-save.by/batch
		bathy.poly<-do.call(rbind,tmp.lst[(1+x*(i-1)):x*i])
		save( bathy.poly, file=file.path( input.fp, paste0(output.fn,i,".rdata")))
	}
}

