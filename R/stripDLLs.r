# @rdname stripDLLs
# @export

#' Remove other DLL conflicts with TMB and hold on to only the sdmTMB DLL
#'
#'
#' @param tokeep specific dlls you wish to keep loaded (can only be an element)
#' @return
#' Strips all .dlls conflicting with TMB running which keeping just sdmTMB
#'
#' A data frame:
#' * `est`: Estimate in link space (everything is in link space)
#' * `est_non_rf`: Estimate from everything that isn't a random field
#' * `est_rf`: Estimate from all random fields combined
#' * `omega_s`: Spatial (intercept) random field that is constant through time
#' * `zeta_s`: Spatial slope random field
#' * `epsilon_st`: Spatiotemporal (intercept) random fields, could be
#'    off (zero), IID, AR1, or random walk
#'
#' If `return_tmb_object = TRUE` (and `nsim = 0` and `tmbstan_model = NULL`):
#'
#' A list:
#' * `data`: The data frame described above
#' * `report`: The TMB report on parameter values
#' * `obj`: The TMB object returned from the prediction run
#' * `fit_obj`: The original TMB model object
#'
#' In this case, you likely only need the `data` element as an end user.
#' The other elements are included for other functions.
#'
#' If `nsim > 0` or `tmbstan_model` is not `NULL`:
#'

    stripDLLs <- function(tokeep='sdmTMB') {
			    dlls <- getLoadedDLLs()
			    isTMBdll <- function(dll)!is(try(getNativeSymbolInfo("MakeADFunObject",dll),TRUE),"try-error")
			    TMBdll <- sapply(dlls, isTMBdll)
			     kl = dlls[which(TMBdll)]
			     for(i in 1:length(kl)){
			     		if( kl[[i]][1]$name==tokeep) next
			 		     	dyn.unload(kl[[i]][2]$path)
			     }
			     cat('Extra DLLs removed')
			    }