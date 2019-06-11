#' @export
lobLW <- function(CL,fsrs=F,sex=2) {


	a=c(0.000608,0.001413,0.00482)
	b=c(3.0583,2.8746,2.638)

				if(fsrs){
							CL[CL==1] <- 5
							CL[CL==2] <- 16
							CL[CL==3] <- 26
							CL[CL==4] <- 36
							CL[CL==5] <- 46
							CL[CL==6] <- 56
							CL[CL==7] <- 66
							CL[CL==8] <- 73
							CL[CL==9] <- 78
							CL[CL==10] <- 86
							CL[CL==10.5] <- 87.5
							CL[CL==11] <- 96
							CL[CL==12] <- 106
							CL[CL==13] <- 116
							CL[CL==14] <- 126
							CL[CL==15] <- 140

						}

	wv = a[sex]*CL^b[sex]

	return(wv)
}
