#	source("fn/interpolation.r")


	#|------------|#
   ##| Arguements |##
	#|------------|#

# ticks = contour lines or strata definitions, 'define' will determine them according to the f(x) rule from Cochran 1977 (see Hubley et al. 2009)
# nstrata = number of strata when ticks='define'
# str.max = maximum value for stratifying variable (all values greater will be set at maximum)
# str.min = minimum value for stratifying variable (all values lesser will be set to NA )
# place = rounding place for defining strata
# aspr = aspect ratio for for creating square grid, will determine from data if missing
# interp.method = 'gstat' = gstat (idw) function from gstat library, 'krige' = krige function from gstat library, 'none' = no interpolation function
# res = resolution for interpolation
# maxdist, nmax, idp, mod.type = arguments to be passed to interpolation function (idp is inverse distance power)
# smooth = logical, TRUE calls smooth.bank function
# smooth.fun = applies smooth.fun to data over a grid
# sres = resolution for smoothing (grid)
# no.data = how to treat missing data when smoothing, default is to assume zero
# blank = TRUE calls blank.bank function, included blanking distance, beyond which if no data are present zeros are assigned or blank.eff
# blank.dist = blanking distance if missing will select the shortest distance to the most isolated point
# blank.type = how spaced out the zeros are, 1 = avg. nearest neighbour distance, 2 = blanking distance
# log.dat = logical, whether to log data
# covariate.dat = covariate data typically used in kriging
# subset.poly = inclusion polygon to subset spatially, 'square' used to close polygons (useful when not smoothing or blanking)
# subset.eff = sets values to this outside the subset.poly
# subscale = size of inset when subset.poly = 'square'





interpolation<-function(contour.dat,ticks,nstrata,str.max,str.min,place=0,aspr,interp.method='gstat',res=0.01,maxdist=Inf,nmax=8,idp=0.5,mod.type="Sph",smooth=F,smooth.fun=median,smooth.procedure=1,sres=1/60.1,no.data='0',blank=T,blank.dist,blank.eff=0,blank.type=2,log.dat=F,covariate.dat=NULL,subset.poly=NULL,regrid=F,subset.eff=NA,subscale=res){
	

	print("contour start")
	print(Sys.time())
	image.mod<-NULL

	names(contour.dat)[1:4]<-c("EID","X","Y","Z")
	if(!is.null(covariate.dat))names(contour.dat)[5]<-"CoV"
	dataPoints1<-contour.dat[,1:3]
	if(is.numeric(dataPoints1$EID)==F)dataPoints1$EID<-1:nrow(dataPoints1)
	
	if(interp.method=='krige')blank=F
	
	# required inputs
	require (PBSmapping)
	require (gstat)
	require (fields)
	require (splancs)
	
	
	# Aspect ratio
	if(missing(aspr)){
		require(CircStats)
		aspr=1/cos(rad(mean(contour.dat$Y)))
		print(paste('Aspect ratio',aspr))
	}
	
	
	# SMOOTHING	
	if(smooth==T){
		if(interp.method!='none'){
			contour.dat<-smooth.bank(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,no.data=no.data,subset.poly=subset.poly,procedure=smooth.procedure)
			contour.dat<-contour.dat[!is.na(contour.dat$Z),]
		}
		if(interp.method=='none'){
			image.dat<-smooth.bank(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,matrix=T,no.data=no.data,subset.poly=subset.poly,procedure=smooth.procedure)
			names(image.dat)<-c('x','y','z')
		}
	}
	
	# BLANKING
	if(blank==T) {
		if(missing(blank.dist))contour.dat<-blank.bank(contour.dat,aspr=aspr,type=blank.type,eff=blank.eff)
		if(!missing(blank.dist))contour.dat<-blank.bank(contour.dat,blank.dist=blank.dist,aspr=aspr,type=blank.type,eff=blank.eff)
	}
		
	dataPoints2<-contour.dat[,1:3]
	dataPoints2$EID<-1:nrow(dataPoints2)
	# INTERPOLATION	
	
	if(!missing(ticks))if(ticks[1]=='define'){
		ticks<-unique(tick.def(contour.dat$Z,nstrata,str.min,str.max,place))
		nstrata<-length(ticks)-1
	}
	if(missing(nstrata)){
		if(missing(ticks))print("ticks or nstrata must be specified")
		nstrata<-length(ticks)-1
	}
	
	if(!is.null(subset.poly)){
		if(subset.poly=='square'){
			inset=subscale*0.8
			subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
		}
	}

	if(interp.method!='none'){
		image.lst<-image.prep(dat=contour.dat,method=interp.method,nmax=nmax,idp=idp,log.dat=log.dat,res=res,aspr=aspr,linear=linear,covariate.dat=covariate.dat,regrid=regrid,mod.type=mod.type,subscale=subscale, subset.poly=subset.poly)
		image.dat<-image.lst[[1]]
		image.var<-image.lst[[2]]
		image.mod<-image.lst[[3]]
		if(!missing(ticks)){
			if(missing(str.min))str.min<-min(ticks)
			if(missing(str.max))str.max<-max(ticks)
		}
		if(missing(ticks)){
			if(missing(str.min))str.min<-min(image.dat$z,na.rm=T)
			if(missing(str.max))str.max<-max(image.dat$z,na.rm=T)
			ticks<-seq(str.min,str.max,length=nstrata+1)
		}
		image.dat$z[image.dat$z>str.max]<-str.max
		image.dat$z[image.dat$z<str.min]<-subset.eff		##### not tested for other applications!!!
	}
	
		
	# SUBSET POLYGON
	if(!is.null(subset.poly)){
		if(subset.poly=='square'){
			inset=subscale*0.8
			subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
		}
		Y<-sort(rep(image.dat$y,length(image.dat$x)))
		X<-rep(image.dat$x,length(image.dat$y))
		tmp<-data.frame(X,Y,Z=as.vector(image.dat$z))
		tmp$Z[!with(tmp, inout(cbind(X,Y), subset.poly[c("X","Y")], bound = T))]<-subset.eff
		image.dat$z<-matrix(tmp$Z,length(image.dat$x),length(image.dat$y))
	}

	

	print("contour end")
	print(Sys.time())

	areas<-areacal(image.dat,units='km2',strata.def=ticks)
	
	output<-list(contour.dat=contour.dat,image.dat=image.dat,image.var=image.var,image.mod=image.mod,str.def=ticks,areas=areas)
	
	
	return(output)
}

#########################################################
#	 _______________________________________________	#
#	|												|	#
#	|	 ADDITIONAL REQUIRED FUNCTIONS AND OBJECTS	|	#
#	|_______________________________________________|	#
#														#
#########################################################


# tick.def function: ("Y:/Development/Georges/Survey Design/r/fn/tick.def.r"): 
# defines ticks or strata boundaries using the sqrt(f(y)) rule

tick.def<-function(char,nstrata=4,min.str=0,max.str,place=0){
	
	
	bin<-round(char,place)
	if(missing(max.str))max.str<-max(bin)

	bins<-sort(unique(bin[bin>0]))
	vars<-c()
	avg<-c()
	N<-c()
	srN<-c()
 
 
 	for(i in 1:length(bins)){
	 	
		avg[i]<-mean(char[bin==bins[i]])
		vars[i]<-var(char[bin==bins[i]])
		N[i]<-length(char[bin==bins[i]])
		srN[1]<-sqrt(N[1])
		if(i>1) srN[i]<-sqrt(N[i])+srN[i-1]
 	}
 
  	ideal.div<-max(srN)/nstrata*(1:nstrata)
  	ind<-c()	
  	for(i in 1:length(ideal.div)){
		ind[i] <- which(srN==srN[abs(srN-ideal.div[i])==min(abs(srN-ideal.div[i]))])
	}	

	str<-c(min.str,bins[ind]) 
	str[length(str)]<-max.str
	
	str
	
}
# blank.bank.r ("Y:/Development/Georges/Survey Design/r/fn/blank.bank.r"): 
# incorporates blanking distance by including zeros spaced eqully at the average nearest nieghbour distance, default blanking distance is the nearest neighbour distance of the most isolated point

blank.bank<-function(surv.dat,blank.dist,aspr=aspr, type=1, eff=0,scale=0.5,type.scaler=0.5){
	
	require(spatstat)
#    browser()
    surv.pts<-subset(surv.dat,select=c('X','Y'))
    xmin<-min(surv.pts$X)
    xmax<-max(surv.pts$X)
    ymin<-min(surv.pts$Y)
    ymax<-max(surv.pts$Y)
    W<-owin(c(xmin-scale,xmax+scale),c(ymin-scale,ymax+scale))
    surv.ppp<-as.ppp(surv.pts,W)
    if(missing(blank.dist))blank.dist<-max(nndist(surv.ppp))
    if(type==1)dims<-c(round((ymax-ymin)/(mean(nndist(surv.ppp))*type.scaler)*aspr),round((xmax-xmin)/(mean(nndist(surv.ppp))*type.scaler)))
    if(type==2)dims<-c(round((ymax-ymin)/(blank.dist*type.scaler)*aspr),round((xmax-xmin)/(blank.dist*type.scaler)))
    blank.map<-distmap(surv.ppp,dim=dims)
    blank.dat<-data.frame(X=sort(rep(blank.map$xcol,blank.map$dim[1])),Y=rep(blank.map$yrow,blank.map$dim[2]),dist=as.vector(blank.map$v))
    blank.dat<-subset(blank.dat,dist>blank.dist,c('X','Y'))
    blank.dat<-merge(surv.dat,data.frame(EID=1:nrow(blank.dat)+1000,blank.dat,Z=eff),all=T)
    print(paste("blanking distance",blank.dist))

    blank.dat
    
}    

# smooth.bank.r ("Y:/Development/Georges/Survey Design/r/fn/smooth.bank.r"):
# applies a function on spatial data over a grid

smooth.bank<-function(dat,fun=mean,res=0.01,aspr=1.345640,no.data='0',matrix=F,procedure=1,subset.poly=NULL,expand=0.1){
	
	print("smooth.bank start")
	print(Sys.time())
	if(is.null(subset.poly)){
	   	Xs<-seq(min(dat$X)-expand,max(dat$X)+expand,res*aspr)
		Ys<-seq(min(dat$Y)-expand,max(dat$Y)+expand, res)
	}
	
   	if(!is.null(subset.poly)){
	   	Xs<-seq(min(subset.poly$X)-res,max(subset.poly$X)+res,res*aspr)
		Ys<-seq(min(subset.poly$Y)-res,max(subset.poly$Y)+res, res)
	}
	Z<-matrix(NA,length(Xs),length(Ys))
	CoV<-matrix(NA,length(Xs),length(Ys))
	
	for(i in 1:(length(Xs)-1)){
		for(j in 1:(length(Ys)-1)){
			square<-data.frame(X=c(Xs[i],Xs[i+1],Xs[i+1],Xs[i],Xs[i]),Y=c(Ys[j],Ys[j],Ys[j+1],Ys[j+1],Ys[j]))
			sq.dat<-dat[with(dat, inout(cbind(X,Y), square, bound = T)),]
			if(procedure==1){
				if(no.data=='0') Z[i,j]<-sum(fun(sq.dat$Z),na.rm=T)
				if(no.data=='NA') Z[i,j]<-fun(sq.dat$Z)
			}
			if(procedure==2){
				if(no.data=='0') Z[i,j]<-sum(sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T),na.rm=T)
				if(no.data=='NA') Z[i,j]<-sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T)
			}
			if(procedure==3){
				Z[i,j]<-sum(sq.dat$Z,na.rm=T)
			}
			if(procedure==4){
				Z[i,j]<-sum(sq.dat$CoV,na.rm=T)
			}
			if(procedure==5){
				n<-nrow(sq.dat)
				if(no.data=='0') Z[i,j]<-sum(mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))})),na.rm=T)
				if(no.data=='NA') Z[i,j]<-mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))}))
			}
			Y<-sort(rep(Ys,length(Xs)))
			X<-rep(Xs,length(Ys))
			if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z))
			if(matrix==T)	result<-list(X=Xs,Y=Ys,Z=Z)
			if(procedure==6){
				Z[i,j]<-fun(sq.dat$Z)
				CoV[i,j]<-fun(sq.dat$CoV)
				if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z),CoV=as.vector(CoV))
				if(matrix==T)	result<-list(X=Xs+0.5*res,Y=Ys+0.5*res,Z=Z,CoV=CoV)

			}
		}
		if(i %in% round(seq(length(Xs)/100,length(Xs),length(Xs)/100)))print(paste(round(i/length(Xs)*100),"%"))
	}

	
	print("smooth.bank end")
	print(Sys.time())
	
	return(result)
}



# image prep : preforms an interpolation and returns image data

# Arguments:
# dat = a dataframe with 3 columns (longitude, latitude, variable to be mapped)
# aspr = aspect ratio for a given latitude (default is for 45 deg.)
# res = resolution of image in decimal degrees

image.prep<-function(X,Y,Z,dat,aspr=1.345640,res=0.02,summary.dat=F,log.dat=T,method='gstat',matrix.dat=T,idp=0.5,nmax=7,maxdist=Inf,linear=F, subset.poly=NULL, covariate.dat=NULL,regrid=F,mod.type="Sph",subscale=0.01){

	require (splancs)
	require (akima)
	require (gstat)
	require (fields)
	
	print("image.prep start")
	print(Sys.time())

	if(missing(dat))dat<-data.frame(X,Y,Z)
#	if(ncol(dat)==4)names(dat)[4]<-"co.v"

	if(log.dat)dat$Z<-log(dat$Z+0.0000000001)
   	# get grid for prediction
	if(is.null(covariate.dat)&&is.null(subset.poly)){
		Xs<-seq(min(dat$X)-subscale,max(dat$X)+subscale,res*aspr)
		Ys<-seq(min(dat$Y)-subscale,max(dat$Y)+subscale, res)
	}
	if(!is.null(subset.poly)){
		Xs<-seq(min(subset.poly$X)-subscale,max(subset.poly$X)+subscale,res*aspr)
		Ys<-seq(min(subset.poly$Y)-subscale,max(subset.poly$Y)+subscale, res)
	}
	if(!is.null(covariate.dat)){
		names(covariate.dat)<-c("X","Y","CoV")
		Xs<-seq(min(covariate.dat$X),max(covariate.dat$X),res*aspr)
		Ys<-seq(min(covariate.dat$Y),max(covariate.dat$Y), res)
	}
	tow.xy <- data.frame(X = dat$X, y = dat$Y)
	poly <- tow.xy[chull(tow.xy), ]
	names(poly) <- c("X", "Y")
	grid.dat <- with(poly, expand.grid(X = Xs, Y = Ys))
		
	if(!is.null(covariate.dat)){
		if(regrid==T)grid.dat<-grid.data(covariate.dat,grid.dat)
		if(regrid==F)grid.dat<-covariate.dat
	}	

	# interpolation methods
	if(method=='gstat'){
		Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = dat,maxdist=maxdist, nmax = nmax, set = list(idp = idp))
		Z.dat<- predict(Z.gstat, grid.dat)
		image.data<-makeTopography(Z.dat[c('X','Y','Z.pred')],digits=5)
		var.data<-NULL
		if(summary.dat)print(summary(Z.dat$Z.pred))
		if(matrix.dat==F)image.data<-data.frame(X=Z.dat[,1],Y=Z.dat[,2],Z=Z.dat[,4])
		spatial.model<-Z.gstat
	}
		
	if(method=='o.krige'){
		browser()
		v <- variogram(Z ~ 1, locations = ~ X + Y, data = dat)
		v.fit <- fit.variogram(v, model = vgm(max(v$gamma), mod.type, median(v$dist), min(v$gamma)))
		Z.krige <- krige(formula = Z ~ 1, locations = ~ X + Y, data = dat, newdata = grid.dat, model=v.fit)
		image.data<-makeTopography(Z.krige[,-4])
		var.data<-makeTopography(Z.krige[,-3])
		if(matrix.dat==F)image.data<-data.frame(X=Z.krige[,1],Y=Z.krige[,2],Z=Z.krige[,3])
		spatial.model<-v.fit
	}
	if(method=='u.krige'){
		v <- variogram(Z ~ CoV, locations = ~ X + Y, data = dat)
		v.fit <- fit.variogram(v, model = vgm(max(v$gamma), mod.type, median(v$dist), min(v$gamma)))
		Z.krige <- krige(formula = Z ~ CoV, locations = ~ X + Y, data = dat, newdata = grid.dat, model=v.fit)
		image.data<-makeTopography(Z.krige[,-4])
		var.data<-makeTopography(Z.krige[,-3])
		if(matrix.dat==F)image.data<-data.frame(X=Z.krige[,1],Y=Z.krige[,2],Z=Z.krige[,3])
		spatial.model<-v.fit
	}
		

	
	if(log.dat)image.data$z<-exp(image.data$z)
	print("image.prep end")
	print(Sys.time())

	return(list(image.data,var.data,spatial.model))
	
}

# grid.data

grid.data <- function(vdata,gdata){
	print("grid.data start")
	print(Sys.time())
	vname<-names(vdata)[3]
	names(vdata)[3]<-"Z"
	Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = vdata)
	Z.pre<- predict(Z.gstat, gdata)
	names(Z.pre)[3]<-vname
	Z.pre[,-4]
	print("grid.data end")
	print(Sys.time())
}
	


# areacal.r	
# calculate strata areas

areacal <- function(strata.dat,strata.def=c(0,5,10,20,40),units='km2'){
	strata.area<-c()
	n<-length(strata.def)-1
	
	for(i in 1:n){
		
		strata.area[i]<-length(strata.dat$z[!is.na(strata.dat$z)&strata.dat$z<strata.def[i+1]&strata.dat$z>=strata.def[i]])
		if(i==n) strata.area[i]<-length(strata.dat$z[!is.na(strata.dat$z)&strata.dat$z<=strata.def[i+1]&strata.dat$z>=strata.def[i]])

	}
	
	km<-(strata.dat$y[2]-strata.dat$y[1])*111.2
	unit.area<-km^2
	atow<-800*2.4384/10^6 # area of standard tow in km2
	if(units=='towable') unit.area<-unit.area/atow
	
	strata.area<-strata.area*unit.area
	names(strata.area)<-1:n	
	
	strata.area
	
}



