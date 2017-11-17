#' @export
assignArea = function(data, coords = c("lon","lat")){
	
		data = makePBS(data,polygon=F)
		LFAs<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","LFAPolys.csv"))
		LFAgrid<-read.csv(file.path( project.datadirectory("bio.lobster"), "data","maps","GridPolys.csv"))
		
        a = which(is.na(data$Y) | is.na(data$X))
        if(length(a)>0) {
                a1 = findPolys(data[-a,],LFAgrid,maxRows = 3e6,includeBdry=1)[,-4]
                }else{
                        a1 = findPolys(data,LFAgrid,maxRows = 3e6,includeBdry=1)[,-4]
                }
        data = merge(data,a1,by='EID')
        data = rename.df(data,n0=c('PID','SID'),n1=c('LFA','LFA_GRID'))

        return(data)
}

