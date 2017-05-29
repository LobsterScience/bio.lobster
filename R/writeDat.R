#' @export
writeDat<-function(list,file,...){
	
	write(paste("#ADMB input data file:",file),file)
	for(i in 1:length(list)){
		write(paste("#",names(list)[i]),file,append=T)
		write(list[[i]],file,append=T,...)
	}
	write("# eof",file,append=T)
	write(999,file,append=T)
	
	
}
