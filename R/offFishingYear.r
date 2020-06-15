#' @export

offFishingYear <- function(date) {
		#following Pezzack 2009 CSAS 
		y = year(date)
		  
		if(y %in% c(1987:2004)) {
			yd = yday(paste(y,10,16,sep="-"))
			ydd = yday(date)
			fy = ifelse(ydd>=yd, y,y-1)
			
		 }
		if(y %in% 1985){
		  yd = yday(paste(y,08,01,sep="-"))
		  ydd = yday(date)
		  fy = ifelse(ydd<yd, y,y+.6)
		  
	 	}
		if(y %in% 1986){
		  yd = yday(paste(y,10,15,sep="-"))
		  ydd = yday(date)
		  fy = ifelse(ydd>yd, y,y-.4)
		  
	 	}
		if(y %in% 2005){
	 	  fy = y-1
			}
		if (y<1985|y>2005){
			fy =  y
			}
return(fy)
		}