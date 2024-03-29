#' Plot a smooth term from an sdmTMB model
#' @export
#'

plot_smooth_sdmtmb <- function(object, select = 1, n = 100, level = 0.95,
                        ggplot = FALSE, rug = TRUE, return_data = FALSE) {
  se <- TRUE
  require(assertthat)
  assert_that(class(object) == "sdmTMB")
  assert_that(is.logical(ggplot))
  assert_that(is.logical(return_data))
  assert_that(is.logical(se))
  assert_that(is.numeric(n))
  assert_that(is.numeric(level))
  assert_that(length(level) == 1L)
  assert_that(length(select) == 1L)
  assert_that(length(n) == 1L)
  assert_that(is.numeric(select))
  assert_that(level > 0 & level < 1)
  assert_that(n < 500)

  if (ggplot) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("ggplot2 not installed", call. = FALSE)
    }
  }
  sm <- parse_smoothers(object$formula, object$data)
  sm_names <- unlist(lapply(sm$Zs, function(x) attr(x, "s.label")))
  sm_names <- gsub("\\)$", "", gsub("s\\(", "", sm_names))

  fe_names <- colnames(object$tmb_data$X_ij)
  fe_names <- fe_names[!fe_names == "offset"] # FIXME
  fe_names <- fe_names[!fe_names == "(Intercept)"]

  all_names <- c(sm_names, fe_names)
  if (select > length(sm_names)) {
    stop("`select` is greater than the number of smooths", call. = FALSE)
  }
  sel_name <- sm_names[select]
  non_select_names <- all_names[!all_names %in% sel_name]

  x <- object$data[[sel_name]]
  nd <- data.frame(x = seq(min(x), max(x), length.out = n))
  names(nd)[1] <- sel_name
  dat <- object$data
  .t <- terms(object$formula[[1]])
  .t <- labels(.t)
  checks <- c("^as\\.factor\\(", "^factor\\(")
  for (ch in checks) {
    if (any(grepl(ch, .t))) { # any factors from formula? if so, explicitly switch class
      ft <- grep(ch, .t)
      for (i in ft) {
        x <- gsub(ch, "", .t[i])
        x <- gsub("\\)$", "", x)
        dat[[x]] <- as.factor(dat[[x]])
      }
    }
  }
  dat[, object$spde$xy_cols] <- NULL
  dat[[object$time]] <- NULL
  for (i in seq_len(ncol(dat))) {
    if (names(dat)[i] != sel_name) {
      if (is.factor(dat[, i, drop = TRUE])) {
        nd[[names(dat)[[i]]]] <- sort(dat[, i, drop = TRUE])[[1]] # TODO note!
      } else {
        nd[[names(dat)[[i]]]] <- mean(dat[, i, drop = TRUE], na.rm = TRUE) # TODO note!
      }
    }
  }
  nd[object$time] <- min(object$data[[object$time]], na.rm = TRUE) # TODO note!
  nd[[object$spde$xy_cols[1]]] <- mean(object$data[[object$spde$xy_cols[1]]], na.rm = TRUE) # TODO note!
  nd[[object$spde$xy_cols[2]]] <- mean(object$data[[object$spde$xy_cols[2]]], na.rm = TRUE) # TODO note!
  p <- predict(object, newdata = nd, se_fit = se, re_form = NA)
  if (return_data) {
    return(p)
  }
  inv <- object$family$linkinv
  qv <- stats::qnorm(1 - (1 - level) / 2)

  if (!ggplot) {
    if (se) {
      lwr <- inv(p$est - qv * p$est_se)
      upr <- inv(p$est + qv * p$est_se)
      ylim <- range(c(lwr, upr))
    } else {
      ylim <- range(p$est)
    }
    plot(nd[[sel_name]], inv(p$est),
      type = "l", ylim = ylim,
      xlab = sel_name, ylab = paste0("s(", sel_name, ")")
    )
    if (se) {
      graphics::lines(nd[[sel_name]], lwr, lty = 2)
      graphics::lines(nd[[sel_name]], upr, lty = 2)
    }
    if (rug) rug(object$data[[sel_name]])
  } else {
    g <- ggplot2::ggplot(p, ggplot2::aes_string(sel_name, "inv(est)",
      ymin = "inv(est - qv * est_se)", ymax = "inv(est + qv * est_se)"
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(alpha = 0.4) +
      ggplot2::labs(x = sel_name, y = paste0("s(", sel_name, ")"))
    if (rug) {
      g <- g +
        ggplot2::geom_rug(
          data = object$data, mapping = ggplot2::aes_string(x = sel_name),
          sides = "b", inherit.aes = FALSE, alpha = 0.3
        )
    }
    return(g)
  }
}

rm_wsp <- function (x) {
  out <- gsub("[ \t\r\n]+", "", x, perl = TRUE)
  dim(out) <- dim(x)
  out
}
# from brms:::all_terms()
all_terms <- function (x) {
  if (!length(x)) {
    return(character(0))
  }
  if (!inherits(x, "terms")) {
    x <- terms(stats::as.formula(x[[1]]))
  }
  rm_wsp(attr(x, "term.labels"))
}

get_smooth_terms <- function(terms) {
  x1 <- grep("s\\(", terms)
  x2 <- grep("t2\\(", terms)
  if (length(x2) > 0L)
    stop("t2() smoothers are not yet supported due to issues with prediction on newdata.", call. = FALSE)
  x1
}
