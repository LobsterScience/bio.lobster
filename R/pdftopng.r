#' @export
pdf2png = function(filename,den=200,multi=F){
	require(animation)
	x=ifelse(multi,"-%04d","")
	im.convert(paste0(filename,".pdf"), output = paste0(filename,x,".png"), extra.opts=paste("-density",den))
}
