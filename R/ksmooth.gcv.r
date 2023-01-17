#' @export

ksmooth.gcv <- function(x, y){
  #uses generalizaed cross validation to find optimum span
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  xdif <- outer(x, x, FUN = "-")
  tune.ksmooth <- function(h){
    xden <- dnorm(xdif / h)
    xden <- xden / rowSums(xden)
    df <- sum(diag(xden))
    fit <- xden %*% y
    mean((fit - y)^2) / (1 - df/nobs)^2
  }
  xrng <- diff(range(x))
  oh <- optimize(tune.ksmooth, interval = c(xrng/nobs, xrng))$minimum
  if(any(oh == c(xrng/nobs, xrng)))
    warning("Minimum found on boundary of search range.\nYou should retune model with expanded range.")
  xden <- dnorm(xdif / oh)
  xden <- xden / rowSums(xden)
  df <- sum(diag(xden))
  fit <- xden %*% y
  list(x = x, y = fit, df = df, h = oh)
}