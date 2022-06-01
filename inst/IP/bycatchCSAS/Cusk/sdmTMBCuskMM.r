#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA33-35.R
rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA33-35.rds')

aT = u$data
bspde = u$grid
be = u$preds

fit =  sdmTMB(CuskWt~
 				s(lZ,k=5) + DID ,
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				spatialtemporal='ar1'
 				)
 

g = predict(fit)


saveRDS(list(fit,g),file='CusksdmTMBFull.rds')


	 fit1 = sdmTMB(CuskWt~
	 				s(lZ,k=5) +DID,
	 				data=aT,
	 				mesh=bspde, 
	 				family=tweedie(link='log'),
	 				spatial='on'
	 				)
	g1 = predict(fit1)



saveRDS(list(fit1,g1),file='CusksdmTMBSpace.rds')



 fit2 = sdmTMB(CuskWt~
 				s(lZ,k=5) +DID,
 				data=aT,
 				mesh=bspde, 
 					family=tweedie(link='log'),
 				spatial='off'
 				)
g2 = predict(fit2)



saveRDS(list(fit2,g2),file='CusksdmTMBDepth.rds')




AIC(fit)
AIC(fit1)
AIC(fit2)

aT$IDS = paste(aT$WOS,aT$GridGroup,sep="-")
aT = cv_SpaceTimeFolds(aT,idCol = 'IDS',nfolds=5)


 fit_cv = sdmTMB_cv(CuskWt~
 				s(lZ,k=5)+DID,
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				fold_ids = 'fold_id',
 				spatialtemporal='ar1',
 				k_folds=5,
 				constant_mesh=F)
 				

fit1_cv1 = sdmTMB_cv(CuskWt~
				s(lZ,k=5)+DID,
				data=aT,
				mesh=bspde, 
 				spatial='on',
 				family=tweedie(link='log'),
				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
				)

fit2_cv = sdmTMB_cv_nomesh(CuskWt~
				s(lZ,k=5)+DID,
				data=aT,
				family=tweedie(link='log'),
				fold_ids = aT$fold_id,
				k_folds=5
				)




mae<- function(x,y){
	sum(abs(x-y))/length(x)
}

rmse = function(x,y){
	sqrt((sum(y-x)^2)/length(x))

}

with(fit_cv$data,mae(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,mae(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fit2_cv$data,mae(as.numeric(CuskWt),as.numeric(fit2$family$linkinv(cv_predicted))))
with(fit_cv$data,rmse(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,rmse(as.numeric(CuskWt),as.numeric(cv_predicted)))
with(fit2_cv$data,rmse(as.numeric(CuskWt),as.numeric(fit2$family$linkinv(cv_predicted))))

#train rmse v test rmse

fit_cvTT = sdmTMBcv_tntpreds(fit_cv)

fitTT = dplyr::bind_rows(fit_cvTT)
fitTT$sqR = fitTT$CuskWt - fitTT$pred
with(subset(fitTT,tt=='train'),mae(as.numeric(CuskWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),mae(as.numeric(CuskWt),as.numeric(pred)))
with(subset(fitTT,tt=='train'),rmse(as.numeric(CuskWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),rmse(as.numeric(CuskWt),as.numeric(pred)))

require(ggplot2)

ggplot(fitTT,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CuskTrainTestDepthSpaceTime.png')

fit_cvTT2 = sdmTMBcv_tntpreds(fit2_cv)
fitTT2 = dplyr::bind_rows(fit_cvTT2)
fitTT2$sqR = fitTT2$CuskWt - fit2$family$linkinv(fitTT2$pred)
with(subset(fitTT2,tt=='train'),mae(as.numeric(CuskWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),mae(as.numeric(CuskWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='train'),rmse(as.numeric(CuskWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),rmse(as.numeric(CuskWt),as.numeric(fit2$family$linkinv(pred))))



require(ggplot2)
ggplot(fitTT2,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CuskTrainTestDepth.png')


fit_cvTT1 = sdmTMBcv_tntpreds(fit1_cv1)
fitTT1 = dplyr::bind_rows(fit_cvTT1)
fitTT1$sqR = fitTT1$CuskWt - fitTT1$pred
with(subset(fitTT1,tt=='train'),mae(as.numeric(CuskWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),mae(as.numeric(CuskWt),as.numeric((pred))))
with(subset(fitTT1,tt=='train'),rmse(as.numeric(CuskWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),rmse(as.numeric(CuskWt),as.numeric((pred))))


require(ggplot2)
ggplot(fitTT1,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CuskTrainTestDepthSpace.png')


######end MM lobsters


##highest elpd has predictions closest to those from true generating process
#
#fit_cv$elpd
#fit1_cv$elpd
#fit2_cv$elpd
#
#	sfit = simulate(fit,nsim=100) 
#	rf = dharma_residuals(sfit,fit)
#
#r <- dharma_residuals(sfit, fit, plot = FALSE)
#
#     plot(r$expected, r$observed)
#     abline(a = 0, b = 1)
#
#
# r1 = fit$family$linkinv(predict(fit))
# r2 = DHARMa::createDHARMa(simulatedResponse=sfit,
# 									observedResponse=fit$data$LegalWt,
# 									fittedPredictedResponse=r1)
#
# plot(r2)
#aT$SRS = r2$scaledResiduals
#	ggplot(data=ns_coast) + geom_sf() +
#			geom_point(data = subset(aT,WOS %in% 1:42),aes(x = X1000*1000, y = Y1000*1000,colour = SRS), shape = 19,size=0.3) +
#			facet_wrap(~WOS) +
#			scale_colour_gradient2(midpoint=.5,low='blue',mid='white',high='red',space='Lab')
#
#
#