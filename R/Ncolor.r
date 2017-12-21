#' @export
Ncolor = function(n) sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)],n)
