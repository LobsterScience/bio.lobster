#' @export

sdmTMB_cv_nomesh <- function(formula, data, family=NULL,
  k_folds = 8, fold_ids = NULL) {
    data$cv_fold <- NA
      data$cv_fold <- fold_ids


   weights <- ifelse(data$cv_fold == 1L, 0, 1)

  fit_func <- function(k) {
    # data in kth fold get weight of 0:
    weights <- ifelse(data$cv_fold == k, 0, 1)

        dat_fit <- data
      args <- c(list(
        data = dat_fit, formula = formula, family=family,
        weights = weights,spatial='off'))
      object <- do.call(sdmTMB, args)
    
     data$sdmTMB_X_ <- data$sdmTMB_Y_ <- stats::runif(nrow(data))
    # mesh <- make_mesh(data, c("sdmTMB_X_", "sdmTMB_Y_"), cutoff = 1)

      lo = which(data$cv_fold==k)
    cv_data <- data[lo, ]
    cv_data$sdmTMB_X_ = object$mesh$loc_xy[lo,1]
    cv_data$sdmTMB_Y_ = object$mesh$loc_xy[lo,2]
 
    # predict for withheld data:
   # predicted_mod <- predict(object, return_tmb_obj =FALSE)
  #  predicted_mod$preds <- object$family$linkinv(predicted_mod$est)
    predicted_obj <- predict(object, newdata = cv_data, return_tmb_obj = TRUE)
    predicted <- predicted_obj$data
    cv_data$cv_predicted <- object$family$linkinv(predicted$est)
    response <- get_response(object$formula)
    withheld_y <- predicted[[response]]
    withheld_mu <- cv_data$cv_predicted

    cv_data$cv_loglik <- ll_sdmTMB(object, withheld_y, withheld_mu)

    list(
      data = cv_data,
      model = object,
      pdHess = object$sd_report$pdHess,
      max_gradient = max(abs(object$gradients)),
      bad_eig = object$bad_eig#,
   #   model_data = predicted_mod
    )
    #fitfunc ends here
  }

      out <- lapply(seq_len(k_folds), fit_func)
  #  model_datas <- lapply(out, `[[`, "model_data")
    models <- lapply(out, `[[`, "model")
  data <- lapply(out, `[[`, "data")
  fold_cv_ll <- vapply(data, function(.x) sum(.x$cv_loglik), FUN.VALUE = numeric(1L))
  fold_cv_elpd <- vapply(data, function(.x)
    log_sum_exp(.x$cv_loglik) - log(length(.x$cv_loglik)), FUN.VALUE = numeric(1L))
  #fold_cv_ll <- vapply(data, function(.x) .x$cv_loglik[[1L]], FUN.VALUE = numeric(1L))
  # fold_cv_ll_R <- vapply(data, function(.x) .x$cv_loglik_R[[1L]], FUN.VALUE = numeric(1L))
  data <- do.call(rbind, data)
  #data <- data[order(data[["_sdm_order_"]]), , drop = FALSE]
  data[["_sdm_order_"]] <- NULL
  data[["_sdmTMB_time"]] <- NULL
  row.names(data) <- NULL
  # bad_eig <- vapply(out, `[[`, "bad_eig", FUN.VALUE = logical(1L))
  pdHess <- vapply(out, `[[`, "pdHess", FUN.VALUE = logical(1L))
  max_grad <- vapply(out, `[[`, "max_gradient", FUN.VALUE = numeric(1L))
  # converged <- all(!bad_eig) && all(pdHess)
  converged <- all(pdHess)
    list(
      data = data,
      models = models,
 #     model_datas = model_datas,
      fold_loglik = fold_cv_ll,
      fold_elpd = fold_cv_elpd,
      # fold_loglik_R = fold_cv_ll_R,
      sum_loglik = sum(data$cv_loglik),
      elpd = log_sum_exp(data$cv_loglik) - log(length(data$cv_loglik)),
      converged = converged,
      pdHess = pdHess,
      max_gradients = max_grad
    )
}

log_sum_exp <- function(x) {
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x)))
}

get_response <- function(formula) {
  tt <- terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response]
}

ll_gaussian <- function(object, withheld_y, withheld_mu) {
  .sd <- exp(object$model$par[["ln_phi"]])
  stats::dnorm(x = withheld_y, mean = withheld_mu, sd = .sd, log = TRUE)
}

ll_tweedie <- function(object, withheld_y, withheld_mu) {
  p <- stats::plogis(object$model$par[["thetaf"]]) + 1
  phi <- exp(object$model$par[["ln_phi"]])
  fishMod::dTweedie(y = withheld_y, mu = withheld_mu, p = p, phi = phi, LOG = TRUE)
}

ll_binomial <- function(object, withheld_y, withheld_mu) {
  stats::dbinom(x = withheld_y, size = 1, prob = withheld_mu, log = TRUE)
}

ll_gamma <- function(object, withheld_y, withheld_mu) {
  .shape <- exp(object$model$par[["ln_phi"]])
  stats::dgamma(x = withheld_y, shape = .shape, scale = withheld_mu / .shape, log = TRUE)
}

ll_lognormal <- function(object, withheld_y, withheld_mu) {
  .sd <- exp(object$model$par[["ln_phi"]])
  stats::dlnorm(x = withheld_y, meanlog = withheld_mu - 0.5 * (.sd)^2, sdlog = .sd, log = TRUE)
}

dstudent <- function(x, df, mean, sd, ncp, log = FALSE) {
  # from metRology::dt.scaled()
  if (!log) {
    return(stats::dt((x - mean)/sd, df, ncp = ncp, log = FALSE)/sd)
  } else {
    return(stats::dt((x - mean)/sd, df, ncp = ncp, log = TRUE) - log(sd))
  }
}

ll_student <- function(object, withheld_y, withheld_mu) {
  .sd <- exp(object$model$par[["ln_phi"]])
  dstudent(x = withheld_y, df = object$tmb_data$df, mean = withheld_mu, sd = .sd, log = TRUE)
}

ll_nbinom1 <- function(object, withheld_y, withheld_mu) {
  phi <- exp(object$model$par[["ln_phi"]])
  stats::dnbinom(x = withheld_y, size = withheld_mu/phi, mu = withheld_mu, log = TRUE)
}

ll_nbinom2 <- function(object, withheld_y, withheld_mu) {
  phi <- exp(object$model$par[["ln_phi"]])
  stats::dnbinom(x = withheld_y, size = phi, mu = withheld_mu, log = TRUE)
}

ll_sdmTMB <- function(object, withheld_y, withheld_mu) {
  family_func <- switch(object$family$family,
    gaussian = ll_gaussian,
    tweedie = ll_tweedie,
    binomial = ll_binomial,
    lognormal = ll_lognormal,
    student = ll_student,
    Gamma = ll_gamma,
    nbinom1 = ll_nbinom1,
    nbinom2 = ll_nbinom2,
    stop(object$family$family, " not yet implemented. ",
      "Please file an issue on GitHub.",
      call. = FALSE
    )
  )
  family_func(object, withheld_y, withheld_mu)
}