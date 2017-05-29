#' @title getIncr
#' @description Made up function that gives you probability of moulting based on day since last moult, size and can include temperature through a degree day metric. Needs to be calibrated with real data.
#' @param \code{cw} : carapace width 
#' @param \code{a} : molt probability parameter
#' @param \code{b} : molt probability parameter
#' @param \code{d} : days since last molt
#' @citation  Bergeron 2011 MSc thesis Lobster Age Size Relationships U of Maine
#' @return The predicted probability of moulting
#' @author  Brad Hubley, \email{Brad.Hubley@@dfo-mpo.gc.ca}
#' @export
pPrMolt = function(cw,a,b,d){ 
a = a * log(cw)/log(100)
b = b * 1/(log(cw)	/log(100))
1 / (1+ exp(-(a+b*d)))
}
