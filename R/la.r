#' @title la
#' @description runs the load_all script from devtools to load bio.lobster after function change
#' @param \code{base.loc} :the base directory of bio.lobster
#' @examples
#' la() 
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

la <- function(package='bio.lobster',base.loc=git.repo) {
	load_all(paste(base.loc,package,sep="/"))
}