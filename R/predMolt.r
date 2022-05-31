#' @title predMolt
#' @description Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
#' @param \code{p} :parameter list containing (at a minimum) area, doy (day of year), temp (temperature at doy)
#' @param \code{cl} : carapace width 
#' @return The predicted probability of moulting
#' @examples
#' require(devtools)
#' load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
#' nefsc.db(DS = 'odbc.dump.redo')
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export


predMolt <- function(p,cl,doy=d,gdd=FALSE,sex) {
	
	with(p,{
		d = doy
	
		if(gdd) {
				 #getting growing degree days
				
				if(area=='GOM') {
						#this is GOM 20m
					if(cl<=39)       				{ a=-1.835; b=0.005}
					if(cl >39& cl<=65) 				{ a=-2.252; b=0.006}
					if(cl >65& cl<=79 & sex==1) 	{ a=-3.795; b=0.006}
					if(cl >65& cl<=79 & sex==2) 	{ a=-3.750; b=0.006}
					if(cl >79& sex==1) 				{ a=-5.525; b=0.004}
					if(cl >79& sex==2) 				{ a=-5.957; b=0.005}
				}
				if(area == 'BoF') {
					
					if(cl<=39)       				{ a=-1.835; b=0.005} #gom20
					if(cl >39& cl<60) 				{ a=-2.252; b=0.006} #gom20
					if(cl >=60& cl<=99 & sex==1) 	{ a=-10.554; b=0.017}
					if(cl >=60& cl<=99 & sex==2) 	{ a=-9.875; b=0.016}
					if(cl >99 & cl<=129& sex==1)	{ a=-4.758; b=0.006}
					if(cl >99 & cl<=129& sex==2)	{ a=-4.678; b=0.007}
					if(cl >129 & sex==1)			{ a=-3.226; b=0.004}
					if(cl >129 & sex==2)			{ a=-4.426; b=0.005}
					}
				}
		if(!gdd) {

				if(area=='GOM') {
						#this is GOM 20m
					if(cl<=39)       				{ a=-1.687; b=0.0183}
					if(cl >39& cl<=65) 				{ a=-1.951; b=0.023}
					if(cl >65& cl<=79 & sex==1) 	{ a=-3.556; b=0.0274}
					if(cl >65& cl<=79 & sex==2) 	{ a=-3.355; b=0.02}
					if(cl >79& sex==1) 				{ a=-5.316; b=0.0124}
					if(cl >79& sex==2) 				{ a=-5.177; b=0.0137}
				}
				if(area == 'BoF') {	
					if(cl<=39)       				{ a=-1.687; b=0.0183} #gom20
					if(cl >39& cl<60) 				{ a=-1.951; b=0.023} #gom20
					if(cl >=60& cl<=99 & sex==1) 	{ a=-2.56; b=0.0147}
					if(cl >=60& cl<=99 & sex==2) 	{ a=-2.156; b=0.0149}
					if(cl >99 & cl<=129& sex==1)	{ a=-3.654; b=0.0122}
					if(cl >99 & cl<=129& sex==2)	{ a=-3.461; b=0.0107}
					if(cl >129 & sex==1)			{ a=-2.287; b=0.0081}
					if(cl >129 & sex==2)			{ a=-4.392; b=0.0109}
					}
				}

			pPrMolt = 1 / (1+ exp(-(a+b*d)))
			return(pPrMolt)
			})

}