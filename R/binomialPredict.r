#' @export

binomialPredict <- function(intercept,term1,x) {
				pr =  1 / (1+exp(-(intercept + term1*x)))
				return(pr)
}