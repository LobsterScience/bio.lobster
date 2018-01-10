#' @export
pMat = function(p,cl,lfa=NULL){


	if(is.null(lfa)){
		if(p$Area == "27N" || p$Area == "27S" || p$Area == "27")  lfa = "LFA27-30" # 27
		if(p$Area == "29")                                        lfa = "LFA29" # 29
		if(p$Area == "28" || p$Area == "30")                      lfa = "LFA28,30" # 30
		if(p$Area == "31A")                                       lfa = "LFA29" # 31A
		if(p$Area == "31B" || p$Area == "32")                     lfa = "LFA32" # 31B & 32
		if(p$Area == "33E" || p$Area == "33W" || p$Area == "33")  lfa = "LFA33" # 33
	}

	pl = data.frame(LFA=c("LFA27-30","LFA29","LFA28,30","LFA31A","LFA32x","LFA32","LFA33x","LFA33","LFA34"),
		a=c(14.266, 14.173, 16.505, 14.53521, 10.4, 18.99223, 14.23,24.87275,22.37302),
		b=c(-0.1959, -0.1727, -0.2132,-0.20347, -0.112,-0.21128, -0.144,-0.25725,-0.23187))

	pMat = with(subset(pl,LFA==lfa),	1/(1+(exp(a+(b*cl)))))


	return(pMat)
}

