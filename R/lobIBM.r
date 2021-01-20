#' @export
lobIBM <- function(p){
	si <- c() #size
	nm <- c() #number of moults
	bs <- c() #berried size
	sx <- c() #sex
	ts <- c() #time step of moulting
	
	niter <- 5000
	with(p,{
		sx = rbinom(1,1,sexratio)+1
		d 	= 365 / aTS #length of time step
		gr = predGrow(p=p,mat=F) #info on the moult increment
		gr = subset(gr,sex==sx)

	si[1] = cl0
	ty = rep(1:aTS,length.out=niter)
	gd = 0 #growing degree days init
for(i in 1:niter) {
		gd = gd + d * temp[ty[i]]
	
		#do maturity before molt, no molt if mature
		LebrisSoM()

		sI = 3
		if(si[i]<60) sI = c(2,3)
		if(ty %in% sI) pm = predMolt(p=p,cl=si[i],d=gd,gdd=T,sex=sx)
		rbinom(1,1,pm)

	}


	})	




}



#p = list()
#p$cl0 		<- 20 							#initiation size must be >20
#p$aTS 		<- 4   							#how many time steps per calendar year
#p$temp 		<- rep(10,p$aTS)				#temperature, can be a vector of length 4 representing season temperatures
#p$sc 		<- 10:180 						#size classes
#p$MaS 		<- function(x) 0.93^x+0.15 		#natural mortaity at size, x, with a asymptote at M of 0.15
#p$Ff 		<- 0 							#fishing morality
#p$minS  	<- 82							#knife edge selectivity
#p$maxS 		<- 300							#maximum size of removals, if no maximum size then set to large number
#p$vnotch 	<- T 							#vnotching program
#p$hM 		<- 0.01							#handling mortality from vnotch program
#p$vlast 	<- 3							#how many years does the vnotch last
#p$area 		<- 'GOM'						#defines maturity ogive and growth matrix from Bergeron 2011
#p$sexratio 	<- .5							#are there initial sex ratio skewness
#p$SoM 		<- function(x) -0.0166*x+91.003 #size at maturity based on degree days
