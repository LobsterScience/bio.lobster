#Make sure to run 0.RunMeFirst.R and 5.CreateMesh_DataforModellingLFA33-35.R
rm(aT,be,bspde)
u = readRDS(file='results/dataForLFA33-35.rds')

aT = u$data
bspde = u$grid
be = u$preds


aT$PA = ifelse(aT$JonahRtWt>0,1,0)
aT$lZ = log(aT$Depth)
 fitH = sdmTMB(PA~s(lZ,k=3)+DID,
 				data=aT,
 				time='WOS',
 				extra_time=33,
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				spatialtemporal='ar1'
 				)


aT2 = subset(aT,JonahRtWt>0)
bspde2 = make_mesh(aT2,xy_cols=c('X1000','Y1000'),mesh=bspde$mesh)

 fitD = sdmTMB(JonahRtWt~
 				s(lZ,k=5)+DID,
 				data=aT2,
 				#time='WOS', 
 				mesh=bspde2, 
 				#extra_time=33,
 				family=Gamma(link='log'),
 				spatial='on',
 				#spatialtemporal='ar1'
 				)


##Kfold cv

aT$IDS = paste(aT$WOS,aT$GridGroup,sep="-")
aT = cv_SpaceTimeFolds(aT,idCol = 'IDS',nfolds=5)

 fit_cv = sdmTMB_cv(PA~
 				s(Depth,k=5),
 				data=aT,
 				time='WOS', 
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				spatialtemporal='ar1',
 				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
			
 				)


fit1_cv1 = sdmTMB_cv(PA~
 				s(Depth,k=5),
 				data=aT,
 				mesh=bspde, 
 				family=binomial(link='logit'),
 				spatial='on',
 				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
			
 				)

fit2_cv = sdmTMB_cv_nomesh(PA~
 				s(Depth,k=5),
 				data=aT,
 					family=binomial(link='logit'),
 				fold_ids = aT$fold_id,
				k_folds=5		
 				)



mae<- function(x,y){
	sum(abs(x-y))/length(x)
}

rmse = function(x,y){
	sqrt((sum(y-x)^2)/length(x))

}

with(fit_cv$data,mae(as.numeric(CodRtWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,mae(as.numeric(CodRtWt),as.numeric(cv_predicted)))
with(fit2_cv$data,mae(as.numeric(CodRtWt),as.numeric(fit2$family$linkinv(cv_predicted))))
with(fit_cv$data,rmse(as.numeric(CodRtWt),as.numeric(cv_predicted)))
with(fit1_cv1$data,rmse(as.numeric(CodRtWt),as.numeric(cv_predicted)))
with(fit2_cv$data,rmse(as.numeric(CodRtWt),as.numeric(fit2$family$linkinv(cv_predicted))))

require(pROC)

roc(fit_cv$data$PA,fit_cv$data$cv_predicted)
roc(fit1_cv1$data$PA,fit1_cv1$data$cv_predicted)
roc(fit2_cv$data$PA,fit2_cv$data$cv_predicted)



#pa model model 1 best

aT2$IDS = paste(aT2$WOS,aT2$GridGroup,sep="-")
aT2$IDS = 'I'
aT2 = cv_SpaceTimeFolds(aT2,idCol = 'IDS',nfolds=5)

bspde2 = make_mesh(aT2,xy_cols=c('X1000','Y1000'),mesh=bspde$mesh)

 fitD_cv = sdmTMB_cv(JonahRtWt~
 				s(lZ,k=5)+DID,#+s(WOS,k=5),
 				data=aT2,
 				time='WOS', 
 				mesh=bspde2, 
 				family=Gamma(link='log'),
 				spatial='on',
 				spatialtemporal='ar1',
 				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
			
 				)


fitD1_cv1 = sdmTMB_cv(JonahRtWt~
 				s(lZ,k=5)+DID,#+s(WOS,k=5),
 				data=aT2, 
 				mesh=bspde2, 
 				family=Gamma(link='log'),
 				spatial='on',
 				fold_ids = 'fold_id',
				k_folds=5,
				constant_mesh=F
			
 				)

fitD2_cv = sdmTMB_cv_nomesh(
				JonahRtWt~
 				s(lZ,k=5)+DID,#+s(WOS,k=5),
 				data=aT2,
 				family=Gamma(link='log'),
 				fold_ids = aT2$fold_id,
				k_folds=5		
 				)


with(fitD_cv$data,mae(as.numeric(JonahRtWt),as.numeric(cv_predicted)))
with(fitD1_cv1$data,mae(as.numeric(JonahRtWt),as.numeric(cv_predicted)))
with(fitD2_cv$data,mae(as.numeric(JonahRtWt),as.numeric(fitD$family$linkinv(cv_predicted))))
with(fitD_cv$data,rmse(as.numeric(JonahRtWt),as.numeric(cv_predicted)))
with(fitD1_cv1$data,rmse(as.numeric(JonahRtWt),as.numeric(cv_predicted)))
with(fitD2_cv$data,rmse(as.numeric(JonahRtWt),as.numeric(fitD$family$linkinv(cv_predicted))))



fit_cvTT = sdmTMBcv_tntpreds(fitD_cv)

fitTT = dplyr::bind_rows(fit_cvTT)
fitTT$sqR = fitTT$JonahRtWt - fitTT$pred
with(subset(fitTT,tt=='train'),mae(as.numeric(JonahRtWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),mae(as.numeric(JonahRtWt),as.numeric(pred)))
with(subset(fitTT,tt=='train'),rmse(as.numeric(JonahRtWt),as.numeric(pred)))
with(subset(fitTT,tt=='test'),rmse(as.numeric(JonahRtWt),as.numeric(pred)))

require(ggplot2)

ggplot(fitTT,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/JonahGammaTrainTestDepthSpaceTime.png')

fit_cvTT2 = sdmTMBcv_tntpreds(fitD2_cv)
fitTT2 = dplyr::bind_rows(fit_cvTT2)
fitTT2$sqR = fitTT2$JonahRtWt - fitD$family$linkinv(fitTT2$pred)
with(subset(fitTT2,tt=='train'),mae(as.numeric(JonahRtWt),as.numeric(fitD$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),mae(as.numeric(JonahRtWt),as.numeric(fitD$family$linkinv(pred))))
with(subset(fitTT2,tt=='train'),rmse(as.numeric(JonahRtWt),as.numeric(fitD$family$linkinv(pred))))
with(subset(fitTT2,tt=='test'),rmse(as.numeric(JonahRtWt),as.numeric(fitD$family$linkinv(pred))))



require(ggplot2)
ggplot(fitTT2,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/JonahGammaTrainTestDepth.png')


fit_cvTT1 = sdmTMBcv_tntpreds(fitD1_cv1)
fitTT1 = dplyr::bind_rows(fit_cvTT1)
fitTT1$sqR = fitTT1$JonahRtWt - fitTT1$pred
with(subset(fitTT1,tt=='train'),mae(as.numeric(JonahRtWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),mae(as.numeric(JonahRtWt),as.numeric((pred))))
with(subset(fitTT1,tt=='train'),rmse(as.numeric(JonahRtWt),as.numeric((pred))))
with(subset(fitTT1,tt=='test'),rmse(as.numeric(JonahRtWt),as.numeric((pred))))


require(ggplot2)
ggplot(fitTT1,aes(sqR,after_stat(density))) + 
geom_histogram() + facet_wrap(~tt) + xlab('Residuals')
savePlot('Figures/ModelOutput/JonahGammaTrainTestDepthSpace.png')

