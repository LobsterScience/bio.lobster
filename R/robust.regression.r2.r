robust.regression.r2 <- function(obj,weighted=F) {
	#from Willet and Singer 1989 . The american statistician 42(3).236-238
	if(!any(class(obj) == 'rlm')) stop('needs to be a rlm obj')
		
		obs = obj$resid + obj$fitted
	
if(weighted) {
	#this is generally inflated r2 and represnts the proportion of weighted variation explained....safe to use standard r2 (from willet paper)
	sse = sum((obj$w * obj$resid)^2)
	sst = sum((obj$w * obs - mean(obj$w * obs))^2)
	} else {
	sse = sum(obj$resid^2)
	sst = sum((obs - mean(obs))^2)
	}

	round(1-sse/sst,3)
}