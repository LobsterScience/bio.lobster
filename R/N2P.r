# Converts Length Frequency matrix/matrices from numbers to proportions
#' @export

N2P<-function(LF){
	pLF<-list()
	if(!is.list(LF))pLF[[1]]<-LF
	else pLF<-LF
	for(i in 1:length(pLF)){
		pLF[[i]]<-sweep(pLF[[i]],1,FUN="/", rowSums(pLF[[i]]))
	}
	pLF
}
