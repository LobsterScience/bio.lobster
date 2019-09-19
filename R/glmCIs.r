#' glmCIs
#' 
#' This function estimates correct CIs for a glm model by estimating on the link and then backtransforming
#' @param model.output = the glm model output
#' @param newdata = the data on which to predict the glm model
#' @return data frame containing the new data with predictions and CIs
#' @examples None yet
#' @export


glmCIs <- function(model.output,newdata){
	
	ilink <- family(model.output)$linkinv 
		ndata <- as.data.frame(do.call(cbind,predict(model.output, newdata, se.fit = TRUE,type='link')[1:2]))
		ndata <- as.data.frame(cbind(newdata,ndata))
		ndata <- within(ndata, {
							fit_resp = ilink(fit)
							upr = ilink(fit + (2*se.fit))
							 lwr = ilink(fit - (2 *se.fit))
						 })
				return(ndata)
	}