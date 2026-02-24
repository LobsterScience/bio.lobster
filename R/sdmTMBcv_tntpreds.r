#' @export
sdmTMBcv_tntpreds <- function(object){
	#training and test predictions
	x  = object$data
	k = length(object$models)
	ou = list()
	for(i in 1:k){
		g = object$models[[i]]
		d = subset(x,fold_id %ni% i)
		e = subset(x,fold_id %in% i)
		w = predict(g,newdata = d,offset=d$OFFSET)
		d$pred = g$family$linkinv(w$est)
		d$tt = 'train'
		e$pred = e$cv_predicted
		e$tt = 'test'
		d = rbind(d,e)
		ou[[i]] = d
		}
	return(ou)
}