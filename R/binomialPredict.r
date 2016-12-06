#' @export
#' @description using the model output from single variable glm with famnily = 'binomial' and link ='logit'

binomialPredict <- function(intercept,term1,x) {
				pr =  1 / (1+exp(-(intercept + term1*x)))
				return(pr)
}