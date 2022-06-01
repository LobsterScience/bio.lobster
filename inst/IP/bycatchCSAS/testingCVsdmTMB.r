##self CV
#testing
#oi = list()
#for(i in 2:5){
#
# aT$Weight = ifelse(aT$fold_id==i,0,1)
# fit = sdmTMB(CuskWt~
# 				s(lZ,k=5)+DID,
# 				data=aT,
# 				time='WOS', 
# 				mesh=bspde, 
# 				family=tweedie(link='log'),
# 				spatial='on',
# 				spatialtemporal='ar1',
# 				weights = aT$Weight
# 				)
# k = predict(fit,newdata=subset(aT,Weight==1))
# f = predict(fit,newdata=subset(aT,Weight==0))
# k$TT = 'train'
# f$TT = 'test'
# ff = list(k,f)
# oil= dplyr::bind_rows(ff)
# oil$pred = fit$family$linkinv(oil$est)
# oil$FOLD=i
# oi[[i]] = oil
#}
#	 				
#	 ot = dplyr::bind_rows(oi)
#
#bspde2 = make_mesh(aT2,xy_cols=c('X1000','Y1000'),mesh=bspde$mesh)
#
