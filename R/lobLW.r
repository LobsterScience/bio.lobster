#' @export
lobLW <- function(CL,fsrs=F) {
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
	0.001413*CL^2.87465
}
