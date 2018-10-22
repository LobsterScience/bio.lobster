#' @export
mls2fsrs <- function(mls=NULL,fsrs=NULL){

	if(is.null(mls)) {
		mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))

		last.year = max(mls$Year)
		current.year = year(Sys.time())
		LFAs = unique(mls$LFA)
		x=list()
		for(i in 1:length(LFAs)){

			x[[i]] = with(subset(mls,Year==last.year&LFA==LFAs[i]),data.frame(Year=(last.year+1):current.year,LFA=LFAs[i],MLS_MM=MLS_MM,SOURCE=SOURCE))

		}
		mls = rbind(mls,do.call("rbind",x))
		mls = mls[order(mls$LFA,mls$Year),]


	}
	if(is.null(fsrs)) fsrs = read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FSRS_SIZE_CODES.csv"))

	mls$fsrs = NA
	for(i in 1:nrow(mls)){
		mls$fsrs[i] = fsrs[which(mls$MLS_MM[i]>=fsrs$MIN_S & mls$MLS_MM[i]<=fsrs$MAX_S),'SIZE_CD']
		}

return(mls)
	

}