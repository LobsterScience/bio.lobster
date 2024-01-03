#' @export

parse_smoothers <- function(formula, data, newdata = NULL) {
  terms <- all_terms(formula)
  if (!is.null(newdata)) {
    if (any(grepl("t2\\(", terms))) stop("Prediction on newdata with t2() still has issues.", call. = FALSE)
  }
  require(mgcv)
  smooth_i <- get_smooth_terms(terms)
  basis <- list()
  Zs <- list()
  Xs <- list()
  if (length(smooth_i) > 0) {
    has_smooths <- TRUE
    smterms <- terms[smooth_i]
    ns <- 0
    ns_Xf <- 0
    for (i in seq_along(smterms)) {
      obj <- eval(str2expression(smterms[i]))
      basis[[i]] <- mgcv::smoothCon(
        object = obj, data = data,
        knots = NULL, absorb.cons = TRUE,
        diagonal.penalty = TRUE
      )
      for (j in seq_along(basis[[i]])) { # elements > 1 with `by` terms
        ns_Xf <- ns_Xf + 1
        rasm <- mgcv::smooth2random(basis[[i]][[j]], names(data), type = 2)
        if (!is.null(newdata)) {
          rasm <- s2rPred(basis[[i]][[j]], rasm, data = newdata)
        }
        for (k in seq_along(rasm$rand)) { # elements > 1 with if s(x, y) or t2()
          ns <- ns + 1
          Zs[[ns]] <- rasm$rand[[k]]
        }
        Xs[[ns_Xf]] <- rasm$Xf
      }
    }
    sm_dims <- unlist(lapply(Zs, ncol))
    Xs <- do.call(cbind, Xs) # combine 'em all into one design matrix
    b_smooth_start <- c(0, cumsum(sm_dims)[-length(sm_dims)])
  } else {
    has_smooths <- FALSE
    sm_dims <- 0L
    b_smooth_start <- 0L
    Xs <- matrix(nrow = 0L, ncol = 0L)
  }
  list(Xs = Xs, Zs = Zs, has_smooths = has_smooths,
       sm_dims = sm_dims, b_smooth_start = b_smooth_start)
}

