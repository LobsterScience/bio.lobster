#' @export
getSeason<-function(DATE){

	# seasons 
	ssn<-yday(as.Date(paste("2012-",seq(3,12,3),"-21",sep="")))
	day<-yday(DATE)

	ifelse(day >= ssn[4] | day < ssn[1],"Winter",
		ifelse(day >= ssn[1] & day< ssn[2], "Spring",
			ifelse(day >=ssn[2] & day < ssn[3],"Summer","Fall")))

}
