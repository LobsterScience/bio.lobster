#' @export
Fecundity = function(lfa="all areas",cl){


	pl = data.frame(LFA=c("LFA27-30","LFA33","all areas"),
		a=c(0.000057, 0.003135, 0.00256),
		b=c(4.28, 3.354	, 3.409))

	pF = with(subset(pl,LFA==lfa),	a * cl^b )


	return(pF)
}

