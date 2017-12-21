#' @export
mls2fsrs <- function(mls=NULL,fsrs=NULL){

	if(is.null(mls)) mls = read.csv(file=file.path(project.datadirectory('bio.lobster'),'data','inputs','MLS.Changes.all.LFA.csv'))
	if(is.null(fsrs)) fsrs = read.csv(file.path( project.datadirectory('bio.lobster'), "data","inputs","FSRS_SIZE_CODES.csv"))

	mls$fsrs = NA
	for(i in 1:nrow(mls)){
		mls$fsrs[i] = fsrs[which(mls$MLS_MM[i]>=fsrs$MIN_S & mls$MLS_MM[i]<=fsrs$MAX_S),'SIZE_CD']
		}

return(mls)
	

}