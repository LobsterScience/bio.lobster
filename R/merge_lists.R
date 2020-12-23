#' @export
merge_lists = function(list_obj){

	n = length(list_obj)
	out = list_obj[[1]]
	for(i in 2:n){
		out = merge(out,list_obj[[i]])
		}
	return(out)
	}

