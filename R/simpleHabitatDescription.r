#' @export
simpleHabitatDescription <- function(){
	#// makes a simple basin model plot to show the filling of habitat space as abundance increases

		x=seq(-100,100,length.out=10000)
		y = dnorm(x,0,75)*-1

	pdf(file='simpleFill.pdf',height=3.5,width=3.5) 	
		plot(x,y, xaxt = 'n', yaxt ='n',xlab='',ylab='',bty='n',type= 'l', lwd = 3)
		fillup(x=x,y=y,increment.length=100)
		dev.off()		



		}


fillup <- function(x,y,increment.length = 100,...) {
	#fill a polgon bit by bit to look like water filling
				i = which.min(y)
		ip = seq(0, 0.5, length.out= increment.length)
			iy = length(y)/2
			for(j in ip) {
				plot(x,y, xaxt = 'n', yaxt ='n',xlab='',ylab='',bty='n',type= 'l', lwd = 3)
				yp = y[i] - y[i]*j
			     o = which.min(abs(yp-y))
		       
		        if(o==iy)  oo = o
		        if(o > iy) oo = iy - (o - iy)
		         if(o < iy) oo = iy + (iy- o) 
			     ooo = o:oo
				polygon(x = c(x[ooo],x[ooo][1]), y=c(y[ooo],y[ooo][1]),border=NULL,col='turquoise2')
				}
		}


fillupTS <- function(x,y,increment.length = 100,...) {
	#fill polygon following TS 
				
		ip = seq(0.25, 0.5, length.out= increment.length)
		
			iy = length(y)/2
			for(j in ip) {
				plot(x,y, xaxt = 'n', yaxt ='n',xlab='',ylab='',bty='n',type= 'l', lwd = 3)
				yp = y[i] - y[i]*j
			     o = which.min(abs(yp-y))
		       
		        if(o==iy)  oo = o
		        if(o > iy) oo = iy - (o - iy)
		         if(o < iy) oo = iy + (iy- o) 
			     ooo = o:oo
				polygon(x = c(x[ooo],x[ooo][1]), y=c(y[ooo],y[ooo][1]),border=NULL,col='turquoise2')
				}
		}
	
