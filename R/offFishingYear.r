#' @export

offFishingYear <- function(date) {
		#following Pezzack 2009 CSAS 
		y = year(date)
		if(y %in% c(1986:2004)) {
			yd = yday(paste(y,10,16,sep="-"))
			ydd = yday(date)
			fy = ifelse(ydd>=yd, y,y-1)
			} 
		else {
			fy =  y
			}
return(fy)
}