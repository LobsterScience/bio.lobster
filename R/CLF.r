#' @title mla
#' @description runs the load_all script from devtools for MCDR to load bio.lobster after function change
#' @param \code{base.loc} :the base directory of bio.lobster
#' @examples
#' la() 
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

mla <- function(base.loc='C:/Users/cassistadarosm/Documents/GitHub/') {
	load_all(paste(base.loc,'bio.lobster',sep="/"))
}