ContLegend<-function(...,lvls,Cont.data){
	legend(...,legend=c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=Cont.data$col)
}