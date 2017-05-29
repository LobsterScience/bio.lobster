#' @title la
#' @description runs the load_all script from devtools to load bio.lobster after function change
#' @param \code{base.loc} :the base directory of bio.lobster
#' @examples
#' la() 
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

la <- function(base.loc='~/bio') {
	load_all(paste(base.loc,'bio.lobster',sep="/"))
}