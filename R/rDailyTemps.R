#' @export

rDailyTemps = function(x=1:365,b=10,m=10,s=50){

	# m = 10 # maginitude of seasonal variation

	# b = 10 # avg annual temp

	# x = 1:365 # days of the year

	# s =  50 # coldest day of the year

	t = -cos(2*pi/365*(x-s))*m+b # daily temperature

	return(t)
}

