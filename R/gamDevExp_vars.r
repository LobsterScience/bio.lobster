#' @export

gamDevExp_vars<-function(model.list.output, data,offset, select=T, family){

  if(length(model.list.output[[1]])==2){return(summary(gam(model.list.output[[1]][[1]], select=select, data=data, family=family,offset=offset))$dev.expl)}
  else
  {

  allModelsResults<-list()
  fullmodel<-gam(model.list.output[[1]][[1]], select=select, data=data, offset=offset,family=family)

  gam_results<-lapply(model.list.output[[1]], function(model.list.output){
    red_model_nosp<- gam(model.list.output,data=data,select=select,offset=offset,family=family)
    sp_vals<-if(summary(red_model_nosp)$m>0){
      fullmodel$sp[c(pmatch(names(red_model_nosp$sp),names(fullmodel$sp), duplicates.ok = F ))]
    } else  {NULL}
    red_model<- gam(model.list.output,data=data,select=select,family=family, sp=sp_vals)
      })

  dev.expl<-lapply(1:length(gam_results),function(x) summary(gam_results[[x]])$dev.expl)
  dev.expl_frame<-data.frame(unlist(dev.expl))
  dev.expl.var<-bind_cols(data.frame(model.list.output[[2]]), dev.expl_frame)
  reduced_model_diff<-sapply(1:(length(dev.expl.var)-1),function(i) sapply(1:nrow(dev.expl.var), function(index) if(dev.expl.var[index,i]==T){
    dev.expl.var[index, "unlist.dev.expl."] - (dev.expl.var[names(which(apply(dev.expl.var[-index,c(-i,-length(dev.expl.var)),drop=F],1,function(x) identical(unname(unlist(x)),unname(unlist(dev.expl.var[index,c(-i,-length(dev.expl.var))])))))), "unlist.dev.expl."])
  } else {NA}
  ))

  reduced_model_diff<-data.frame(reduced_model_diff)
  colnames(reduced_model_diff)<-colnames(model.list.output[[2]])
  return(colMeans(reduced_model_diff, na.rm=T))
      }
    }
