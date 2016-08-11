#' @title gla
#' @description runs install_git from local dir to load funcs
#' @param \code{base.loc} :the base directory of bio.lobster
#' @examples
#' gla() 
#' @author  Adam Cook, \email{Adam.Cook@@dfo-mpo.gc.ca}
#' @export

gla <- function(base.loc=file.path('C:','Manon','git')) {
	install_git(file.path(base.loc,'bio.lobster'))
}