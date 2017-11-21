#' @export
collapse_list<-function(list1) {
	#removes elements from list with no info
	a<-dim_list(list1)
	if(any(a[,1]==0)) {
	list1<-list1[-which(a[,1]==0)]
		}
	return(list1)
	}