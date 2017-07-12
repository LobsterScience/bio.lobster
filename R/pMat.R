#' @export
pMat = function(lfa="LFA27-30",cl){


	pl = data.frame(LFA=c("LFA27-30","LFA29","LFA28,30","LFA32","LFA33"),
		a=c(14.266, 14.173, 16.505, 10.4, 14.23),
		b=c(-0.1959, -0.1727, -0.2132, -0.112, -0.144))

	pMat = with(subset(pl,LFA==lfa),	1/(1+(exp(a+(b*cl)))))


	return(pMat)
}

