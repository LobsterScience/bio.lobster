#' @export gam_model_list

gam_model_list<-function (covariates, response){
  logica <- list(c(T,F))
  rep_logical <-  rep(logica, length(covariates))
  regres_logical<-expand.grid(rep_logical)
  colnames(regres_logical)<-covariates
  allModelsList <- apply(regres_logical, 1, function(x) as.formula(paste(c(response, covariates[x]),collapse=" + ")) )
  return(list(allModelsList, regres_logical))
}



