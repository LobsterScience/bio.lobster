#' @export
pMat3 = function(p,cl,lfa=NULL){


	if(is.null(lfa)){
		if(p$LFA == "27N" || p$LFA == "27S" || p$LFA == "27")  lfa = "LFA27-30" # 27
		if(p$LFA == "29")                                        lfa = "LFA29" # 29
		if(p$LFA == "28" || p$LFA == "30")                      lfa = "LFA28,30" # 30
		if(p$LFA == "31A")                                       lfa = "LFA31A" # 31A
		if(p$LFA == "31B" || p$LFA == "32")                     lfa = "LFA32" # 31B & 32
		if(p$LFA == "33E" || p$LFA == "33W" || p$LFA == "33")  lfa = "LFA33" # 33
		if(p$LFA == "34")                                        lfa = "LFA34" # 33
		if(p$LFA == "35")                                        lfa = "LFA35" # 33
		if(p$LFA == "36")                                        lfa = "LFA36" # 33
		if(p$LFA == "38")                                        lfa = "LFA38" # 33



	}

  #updated with howse and armsworthy 2024 new estimates for 36 and 38 (not 33 yet) and the ones the estimate from angelicas 31A and 32
	pl = data.frame(LFA=c("LFA27-30","LFA29","LFA28,30","LFA31A","LFA32","LFA33","LFA34","LFA35","LFA36",'LFA38'),
		a=c(14.266, 14.173, 16.505, 13.12,  12.93, 24.87275,22.37302,22.96,19.94,20.59),
		b=c(-0.1959, -0.1727, -0.2132,-0.17,-0.14, -0.25725,-0.23187,-0.24,-0.22,-0.23))

	pMat = with(subset(pl,LFA==lfa),	1/(1+(exp(a+(b*cl)))))


	return(pMat)
}

l31a = c(13.12,-0.17)
l32 = c(12.93,-0.14)
