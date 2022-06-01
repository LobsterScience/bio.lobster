#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA33-35.R
rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA33-35.rds')

aT = u$data
bspde = u$grid
be = u$preds

fit =  sdmTMB(CodWt~
 				s(lZ,k=5) + DID ,
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				spatialtemporal='ar1'
 				)
 

g = predict(fit)


saveRDS(list(fit,g),file='codsdmTMBFull.rds')


	 fit1 = sdmTMB(CodWt~
	 				s(lZ,k=5) +DID,
	 				data=aT,
	 				mesh=bspde, 
	 				family=tweedie(link='log'),
	 				spatial='on'
	 				)
	g1 = predict(fit1)



saveRDS(list(fit1,g1),file='codsdmTMBSpace.rds')



 fit2 = sdmTMB(CodWt~
 				s(lZ,k=5) +DID,
 				data=aT,
 				mesh=bspde, 
 					family=tweedie(link='log'),
 				spatial='off'
 				)
g2 = predict(fit2)



saveRDS(list(fit2,g2),file='codsdmTMBDepth.rds')




AIC(fit)
AIC(fit1)
AIC(fit2)

aT$IDS = 'I'
aT = cv_SpaceTimeFolds(aT,idCol = 'IDS',nfolds=5)


 fit_cv = sdmTMB_cv(CodWt~
 				s(lZ,k=5)+DID+s(LobsterWt,k=3),
 				data=aT,
 				#time='WOS', 
 				mesh=bspde, 
 				family=tweedie(link='log'),
 				spatial='on',
 				fold_ids = 'fold_id',
 			#	spatialtemporal='ar1',
 				k_folds=5,
 				#constant_mesh=F
 				)
 				

fit1_cv1 = sdmTMB_cv(CodWt~
				s(lZ,k=5)+DID,
				data=aT,
				mesh=bspde, 
 				spatial='on',
 				family=tweedie(link='log'),
				fold_ids = 'fold_id',
				k_folds=5#,
				#constant_mesh=F
				)

fit2_cv = sdmTMB_cv_nomesh(CodWt~
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

with(fit_cv$data,mae(as.numeric(CodWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,mae(as.numeric(CodWt),as.numeric(cv_predicted)))
with(fit2_cv$data,mae(as.numeric(CodWt),as.numeric(fit2$family$linkinv(cv_predicted))))
with(fit_cv$data,rmse(as.numeric(CodWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,rmse(as.numeric(CodWt),as.numeric(cv_predicted)))
with(fit2_cv$data,rmse(as.numeric(CodWt),as.numeric(fit2$family$linkinv(cv_predicted))))

#train rmse v test rmse

fit_cvTT = sdmTMBcv_tntpreds(fit_cv)

fitTT = dplyr::bind_rows(fit_cvTT)
fitTT$sqR = fitTT$CodWt - fitTT$pred
with(subset(fitTT,tt=='train'),mae(as.numeric(CodWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),mae(as.numeric(CodWt),as.numeric(pred)))
with(subset(fitTT,tt=='train'),rmse(as.numeric(CodWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),rmse(as.numeric(CodWt),as.numeric(pred)))

require(ggplot2)

ggplot(fitTT,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CodTrainTestDepthSpaceTime.png')

fit_cvTT2 = sdmTMBcv_tntpreds(fit2_cv)
fitTT2 = dplyr::bind_rows(fit_cvTT2)
fitTT2$sqR = fitTT2$CodWt - fit2$family$linkinv(fitTT2$pred)
with(subset(fitTT2,tt=='train'),mae(as.numeric(CodWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),mae(as.numeric(CodWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='train'),rmse(as.numeric(CodWt),as.numeric(fit2$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),rmse(as.numeric(CodWt),as.numeric(fit2$family$linkinv(pred))))



require(ggplot2)
ggplot(fitTT2,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CodTrainTestDepth.png')


fit_cvTT1 = sdmTMBcv_tntpreds(fit1_cv1)
fitTT1 = dplyr::bind_rows(fit_cvTT1)
fitTT1$sqR = fitTT1$CodWt - fitTT1$pred
with(subset(fitTT1,tt=='train'),mae(as.numeric(CodWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),mae(as.numeric(CodWt),as.numeric((pred))))
with(subset(fitTT1,tt=='train'),rmse(as.numeric(CodWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),rmse(as.numeric(CodWt),as.numeric((pred))))


require(ggplot2)
ggplot(fitTT1,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/CodTrainTestDepthSpace.png')

