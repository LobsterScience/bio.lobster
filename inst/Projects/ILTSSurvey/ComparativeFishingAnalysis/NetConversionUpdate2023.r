require(bio.lobster)
require(bio.utilities)
require(gamlss)
require(tidyr)
require(devtools)
require(geosphere)
la()
options(stringsAsFactors=F)

fpa = file.path(project.datadirectory('bio.lobster'),'analysis','ILTSSurvey')
v = ILTS_ITQ_All_Data(species=2550,redo_base_data = F)
cs = read.csv(file.path(project.datadirectory('bio.lobster'),'data','survey','comparativeStations.csv'))
cs$ID = paste(cs$YEAR,cs$STATION)

v$ID = paste(v$YEAR,v$STATION)
v$rFL = round(v$FISH_LENGTH)
v = subset(v,ID %in% cs$ID)
v$SID = paste(v$TRIP_ID,v$SET_NO)


va = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+SID+SET_DEPTH+SET_LONG+SET_LAT+sweptArea+SET_DATE+SET_TIME,data=v,FUN=sum)
names(va)[12]='N'

#comparing tow tracks

tt = ILTS_ITQ_All_Data(species=2550,redo_base_data = F,return_tow_tracks = T)
tt$SID = paste(tt$TRIP_ID,tt$SET_NO)

tt1 = subset(tt, SID %in% unique(v$SID))

tm = merge(va,tt1,by='SID',all=T)

ut = unique(tm$STATION)
outputs=list()

for(i in 1:length(ut)){
  b = subset(tm,STATION==ut[i])
  ug = unique(b$GEAR)
  uv = unique(b$VESSEL_NAME)
  if(length(uv)==1 & length(ug)>1){
        f1 = subset(b,GEAR=='NEST')
        f2 = subset(b,GEAR=='280 BALLOON')
        ti='Gear Comparison'
  }
  if(length(uv)==2 & length(ug)==1){
    f1 = subset(b,VESSEL_NAME=="JOSIE'S PRIDE")
    f2 = subset(b,VESSEL_NAME=='JOSEPH LORENZO I')
    ti='Vessel Comparison'
  }
  if(nrow(f1)>3 & nrow(f2)>3){
        f1 = f1[order(f1$Time),]
        f2 = f2[order(f2$Time),]
        
      gpl = ggplot(data=f1,aes(X,Y))+
          geom_segment(
            aes(x = f1$X[1], y = f1$Y[1], xend = f1$X[nrow(f1)], yend = f1$Y[nrow(f1)]),
            arrow = arrow(type = "open", length = unit(0.2, "inches")), color='blue'
          ) +
          geom_point(
            aes(x = f1$X[1], y = f1$Y[1]),
             color='blue') +
          geom_segment(data=f2,
            aes(x = f2$X[1], y = f2$Y[1], xend = f2$X[nrow(f2)], yend = f2$Y[nrow(f2)]),
            arrow = arrow(type = "open", length = unit(0.2, "inches")), colour='red'
          ) +
          geom_point(data=f2,
                       aes(x = f2$X[1], y = f2$Y[1]),
                     colour='red')+
          labs(title=paste(ti,'Station ',ut[i]),x='Longitude',y='Latitude')
      nm = paste(ti, ut[i],'- map.png',sep="")
      ggsave(file.path(fpa,nm), plot = gpl, width = 5, height = 4, units = "in") 
        d1 = round(distGeo(f1[nrow(f1)/2, c("X", "Y")], f2[nrow(f2)/2, c("X", "Y")]))
        date_diff = as.numeric(abs(unique(f1$SET_DATE) - unique(f2$SET_DATE)))
        depth_diff = abs(unique(f1$SET_DEPTH) - unique(f2$SET_DEPTH))
        outputs[[i]] = c(ut[i],ti,Geographic_Distance=d1,Days_Diff=date_diff,Depth_diff=depth_diff,meanZ=mean(c(unique(f1$SET_DEPTH), unique(f2$SET_DEPTH))) )
        
        }
}
 outputs = as.data.frame(do.call(rbind,outputs))

#total lobster
va = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=sum)
names(va)[5]='N'

vaw = pivot_wider(va,id_cols=c(STATION, YEAR,VESSEL_NAME),names_from=GEAR,values_from=N)

#by length
vaL = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+rFL,data=v,FUN=sum)
names(vaL)[6]='N'


vaLw = pivot_wider(vaL,id_cols=c(STATION, YEAR,VESSEL_NAME, rFL),names_from=GEAR,values_from=N)
names(vaLw)[5] = 'BALLOON'

#only lengths with data
ex = aggregate(cbind(BALLOON,NEST)~rFL,data=vaLw,FUN=sum)
ex = subset(ex,BALLOON+NEST>0)
ex$C = ex$NEST/ex$BALLOON
valwR = subset(vaLw,rFL %in% ex$rFL)



valwRR = subset(valwR, !is.na(BALLOON))
valwRR$BALLOON = round(valwRR$BALLOON)
valwRR$NEST= round(valwRR$NEST)

dat = subset(valwRR,NEST+BALLOON>0)

exp_balloon =data.frame(len= rep(dat$rFL,times=dat$BALLOON))
exp_nest = data.frame(len = rep(dat$rFL,times=dat$NEST))
xlabs = 'Length'
xlabs = 'Carapace Length'
 ggplot(exp_balloon,aes(x=len))+
      geom_histogram() +
      labs(x=xlabs,y=expression(paste("Balloon: Number per km",.^2)))+
      coord_cartesian(xlim = c(min(dat$rFL),max(dat$rFL))) +
      theme_bw()


ggplot(exp_nest,aes(x=len))+
  geom_histogram() +
  labs(x=xlabs,y=expression(paste("NEST: Number per km",.^2)))+
  coord_cartesian(xlim = c(min(dat$rFL),max(dat$rFL))) +
  theme_bw()

## end data


##begin gamlss

ov = aggregate(cbind(NEST,BALLOON)~rFL,data=dat,FUN=sum)
ov$C = ov$NEST / ov$BALLOON
dat$C = dat$NEST / dat$BALLOON

dat = merge(dat,outputs,by.x='STATION',by.y='V1')
dat$meanZ = round(as.numeric(dat$meanZ),)
fit= out = gamlss(cbind(NEST,BALLOON)~1,data=dat,family=BB())
fit1 = out1 = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3),data=dat,family=BB())
fit2 = out2 = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3),sigma.formula=~cs(rFL,df=3),data=dat,family=BB())
fitz= outz = gamlss(cbind(NEST,BALLOON)~cs(meanZ,df=3),data=dat,family=BB())
fit1z = out1z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+cs(meanZ,df=3),data=dat,family=BB())
fit2z = out2z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+cs(meanZ,df=3),sigma.formula=~cs(rFL,df=3)+cs(meanZ,df=3),data=dat,family=BB())

dat$FVessel = as.factor(dat$VESSEL_NAME)
fit3z = out3z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+cs(meanZ,df=3)+FVessel,sigma.formula=~cs(rFL,df=3)+cs(meanZ,df=3),data=dat,family=BB())
fit4z = out4z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+cs(meanZ,df=3)+FVessel,sigma.formula=~cs(rFL,df=3)+cs(meanZ,df=3)+FVessel,data=dat,family=BB())


i = 0
model.output = data.frame(
    Mod = c('intercept','length.mu','length.mu.sigma','z','length.mu.z','length.mu.sigma.z','length.mu.sigma.z.mu.vess','length.mu.sigma.z.sig.vess'),
    AIC=c(AIC(fit),AIC(fit1),AIC(fit2),AIC(fitz),AIC(fit1z),AIC(fit2z),AIC(fit4z),AIC(fit4z)),
    intercept=c(coef(fit)[1],coef(fit1)[1],coef(fit2)[1],coef(fitz)[1],coef(fit1z)[1],coef(fit2z)[1],coef(fit4z)[1],coef(fit4z)[1]),
    length.coef=c(NA,coef(fit1)[2],coef(fit2)[2],NA,coef(fit1z)[2],coef(fit2z)[2],coef(fit3z)[2],coef(fit4z)[2]),
    depth.coef=c(NA,NA,NA,coef(fit1z)[2],coef(fit1z)[3],coef(fit2z)[3],coef(fit3z)[3],coef(fit4z)[3]))
i = which.min(model.output$AIC)
if(i==2) out = fit1
if(i==3) out = fit2
if(i==4) out = fitz
if(i==5) out = fit1z
if(i==6) out = fit2z
if(i==7) out = fit3z
if(i==8) out = fit4z

#newd = data.frame(rFL=seq(min(dat$rFL),max(dat$rFL),by=1))
#yl=c(0,max(fit$mu.fv/(1-fit$mu.fv))*2)
#if(i>1) yl=c(0,max(fit$mu.fv/(1-fit$mu.fv)+1.96*sqrt(fit$mu.var) * (fit$mu.fv/(1-fit$mu.fv))))
#plot(dat$rFL, out$mu.fv/(1-out$mu.fv),type='l',ylim=yl,xlab='Carapace Length',ylab='Relative Catch Efficiency')
# CI approximated by lognormal given variance of logit(P)
#lines(dat$rFL, fit$mu.fv/(1-fit$mu.fv)-1.96*sqrt(fit$mu.var) * (fit$mu.fv/(1-fit$mu.fv)), col = "blue", lty = "dashed")
#lines(dat$rFL, fit$mu.fv/(1-fit$mu.fv)+1.96*sqrt(fit$mu.var) * (fit$mu.fv/(1-fit$mu.fv)), col = "blue", lty = "dashed")
#abline(h=1,lwd=2,col='red')


d = cbind(dat,out$residuals) #Dunn and Smyth 1996
names(d)[ncol(d)]='Randomized_Quantile_Residuals'

ggplot(data=d,aes(x=STATION,y=Randomized_Quantile_Residuals))+
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
  geom_hline(yintercept = 0,color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=d,aes(x=as.factor(rFL),y=Randomized_Quantile_Residuals))+
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
  geom_hline(yintercept = 0,color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x=xlabs)

ggplot(data=d,aes(x=meanZ,y=Randomized_Quantile_Residuals))+
  geom_point()+
  geom_hline(yintercept = 0,color='red')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Depth")

#bootstrapping across stations- assuming that the sampling unit is the station and that each unit is a sample of the population

st = unique(dat$STATION)
niter=length(st)

newd = expand.grid(rFL=seq(min(dat$rFL),max(dat$rFL),by=1),meanZ=seq(min(dat$meanZ),max(dat$meanZ),by=1),FVessel="Josie's Pride")

ou = matrix(NA,nrow=length(newd[,1]),ncol=niter,byrow=F)
for(i in 1:niter){
		stt = sample(st,length(st),replace=T)
		d1 = list()
		for(j in 1:length(stt)) {
				d1[[j]] = subset(dat,STATION== stt[j])
			}
			d1 = as.data.frame(do.call(rbind,d1))
			out = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+cs(meanZ,df=3),sigma.formula =~cs(rFL,df=3)+cs(meanZ,df=3) ,data=d1,family=BB())
			mu = predict(out,what='mu',type='response',newdata = newd)
			rho = mu / (1-mu)
			ou[,i] = rho
			}


sou = as.data.frame(cbind(newd[,1:2],t(apply(ou,1,quantile,c(0.5,.0275,.975))),(apply(ou,1,mean)),(apply(ou,1,sd))))
names(sou) = c('Length','Depth','Median','L95','U95','Mean','SD')
sou$CV = sou$SD / sou$Mean

souL = aggregate(cbind(Median,L95,U95)~Length,data=sou,FUN=median)
souD = aggregate(cbind(Median,L95,U95)~Depth,data=sou,FUN=median)

ggplot(data=souL, aes(x=Length, y=Median)) + geom_line()+
  geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
  #  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x=xlabs,y='Relative Catch Efficiency [NEST/BALLOON]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()

ggplot(data=souD, aes(x=Depth, y=Median)) + geom_line()+
  geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
  #  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x=xlabs,y='Relative Catch Efficiency [NEST/BALLOON]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()



ggplot(sou,aes(Length,Depth,z=Median))+geom_contour_filled()

ggplot(sou,aes(Length,Depth,z=CV))+geom_contour_filled()

##if Length only
ggplot(data=sou, aes(x=Length, y=Median)) + geom_line()+
geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
#  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x=xlabs,y='Relative Catch Efficiency [NEST/BALLOON]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()



saveRDS(sou,file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_2023.rds'))

png(file=file.path(project.datadirectory('bio.lobster'),'data','survey','ConvNestBall_2023.png'),type='png')
with(sou,{
	plot(Length,Median,xlab='Carapace Length (mm)',ylab=expression(paste('Conversion Coefficient (',rho,')')),type='l',lwd=1.5,ylim=c(0,max(dat$C[is.finite(dat$C)])))
	lines(Length,L95,type='l',lwd=1.5,lty=2)
	lines(Length,U95,type='l',lwd=1.5,lty=2)
	points(ov$rFL,ov$C,pch=16,cex=.75)
	#points(dat$rFL,dat$C,pch=16,col=rgb(0,0,1,alpha=.1),cex=.75)
})
abline(h=1,col='red',lwd=1.5)
dev.off()

png(file=file.path(project.datadirectory('bio.lobster'),'data','survey','CVConvNestBall_2023.png'),type='png')
plot(sou$Length,sou$CV,type='l',xlab = 'Carapace Length',ylab = expression(paste('CV of ',rho)),lwd=1.5)
dev.off()
####end gamlss


vv = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall.rds'))
