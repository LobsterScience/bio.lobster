require(bio.lobster)
require(bio.utilities)
require(gamlss)
require(tidyr)
require(devtools)
require(geosphere)
la()
options(stringsAsFactors=F)

fpa = file.path(project.datadirectory('bio.lobster'),'analysis','ILTSSurvey')
v = ILTS_ITQ_All_Data(species=2550,redo_base_data =F,return_base_data = T)
v = subset(v,SPECCD_ID==2550)
cs = read.csv(file.path(project.datadirectory('bio.lobster'),'data','survey','comparativeStations.csv'))
cs = subset(cs,YEAR %in% c(2016,2019))
cs$ID = paste(cs$YEAR,cs$STATION)

v$ID = paste(v$YEAR,v$STATION)

v$II = paste(v$TRIP_ID,v$SET_NO)
vv = aggregate(TRIP_ID~YEAR+VESSEL_NAME+II,data=v,FUN=length)
vvv = aggregate(II~YEAR+VESSEL_NAME,data=vv,FUN=function(x) length(unique(x)))
vvv[order(vvv$YEAR),]

v$rFL = floor(v$FISH_LENGTH)#/5)*5+2
v = subset(v,FISH_LENGTH>40 & FISH_LENGTH<150)
v = subset(v,ID %in% cs$ID)
v$SID = paste(v$TRIP_ID,v$SET_NO)


va = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+SID+SET_DEPTH+SET_LONG+SET_LAT+sweptArea+spread+SET_DATE+SET_TIME,data=v,FUN=sum)
names(va)[13]='N'

#2022 vessel #### not necessary

# vaL = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+rFL,data=subset(v,YEAR==2022),FUN=sum)
# names(vaL)[6]='N'
# 
# 
# vaLw = pivot_wider(vaL,id_cols=c(STATION, YEAR, rFL),names_from=VESSEL_NAME,values_from=N)
# names(vaLw)[4:5] = c('JL','JP')
# 
# #only lengths with data
# ex = aggregate(cbind(JL,JP)~rFL,data=vaLw,FUN=sum)
# ex = subset(ex,JL+JP>0)
# ex$C = ex$JL/ex$JP
# valwR = subset(vaLw,rFL %in% ex$rFL)
# 
# valwRR = subset(valwR, !is.na(JL))
# valwRR$JL = round(valwRR$JL)
# valwRR$JP= round(valwRR$JP)
# 
# dat = subset(valwRR,JL+JP>0)
# 
# dat = merge(dat,outputs,by.x='STATION',by.y='V1')
# dat$meanZ = round(as.numeric(dat$meanZ),)
# 
# 
# exp_JL =data.frame(len= rep(dat$rFL,times=dat$JL))
# exp_JP = data.frame(len = rep(dat$rFL,times=dat$JP))
# xlabs = 'Length'
# xlabs = 'Carapace Length'
# ggplot(exp_JL,aes(x=len))+
#   geom_histogram() +
#   labs(x=xlabs,y=expression(paste("JL: Number per km",.^2)))+
#   coord_cartesian(xlim = c(min(dat$rFL),max(dat$rFL))) +
#   theme_bw()
# 
# 
# ggplot(exp_JP,aes(x=len))+
#   geom_histogram() +
#   labs(x=xlabs,y=expression(paste("JP: Number per km",.^2)))+
#   coord_cartesian(xlim = c(min(dat$rFL),max(dat$rFL))) +
#   theme_bw()
# 
# 
# fit= out = gamlss(cbind(JL,JP)~1,data=dat,family=BB())
# fit1 = out1 = gamlss(cbind(JL,JP)~cs(rFL,df=3),data=dat,family=BB())
# fit2 = out2 = gamlss(cbind(JL,JP)~cs(rFL,df=3),sigma.formula=~cs(rFL,df=3),data=dat,family=BB())
# fitz= outz = gamlss(cbind(JL,JP)~cs(meanZ,df=3),data=dat,family=BB())
# fit1z = out1z = gamlss(cbind(JL,JP)~cs(rFL,df=3)+cs(meanZ,df=3),data=dat,family=BB())
# fit2z = out2z = gamlss(cbind(JL,JP)~cs(rFL,df=3)+cs(meanZ,df=3),sigma.formula=~cs(rFL,df=3)+cs(meanZ,df=3),data=dat,family=BB())
# 
# i = 0
# model.output = data.frame(
#   Mod = c('intercept','length.mu','length.mu.sigma','z','length.mu.z','length.mu.sigma.z'),
#   AIC=c(AIC(fit),AIC(fit1),AIC(fit2),AIC(fitz),AIC(fit1z),AIC(fit2z)),
#   intercept=c(coef(fit)[1],coef(fit1)[1],coef(fit2)[1],coef(fitz)[1],coef(fit1z)[1],coef(fit2z)[1]),
#   length.coef=c(NA,coef(fit1)[2],coef(fit2)[2],NA,coef(fit1z)[2],coef(fit2z)[2]),
#   depth.coef=c(NA,NA,NA,coef(fit1z)[2],coef(fit1z)[3],coef(fit2z)[3]))
# i = which.min(model.output$AIC)
# if(i==2) out = fit1
# if(i==3) out = fit2
# if(i==4) out = fitz
# if(i==5) out = fit1z
# if(i==6) out = fit2z
# 
# 
# d = cbind(dat,out$residuals) #Dunn and Smyth 1996
# names(d)[ncol(d)]='Randomized_Quantile_Residuals'
# 
# ggplot(data=d,aes(x=STATION,y=Randomized_Quantile_Residuals))+
#   geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
#   geom_hline(yintercept = 0,color='red')+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ggplot(data=d,aes(x=as.factor(rFL),y=Randomized_Quantile_Residuals))+
#   geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
#   geom_hline(yintercept = 0,color='red')+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   labs(x=xlabs)
# 
# ggplot(data=d,aes(x=meanZ,y=Randomized_Quantile_Residuals))+
#   geom_point()+
#   geom_hline(yintercept = 0,color='red')+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   labs(x="Depth")
# #bootstrapping across stations- assuming that the sampling unit is the station and that each unit is a sample of the population
# 
# st = unique(dat$STATION)
# niter=length(st)
# 
# newd = expand.grid(rFL=seq(min(dat$rFL),max(dat$rFL),by=1),meanZ=seq(min(dat$meanZ),max(dat$meanZ),by=1))
# 
# ou = matrix(NA,nrow=length(newd[,1]),ncol=niter,byrow=F)
# for(i in 1:niter){
#   stt = sample(st,length(st),replace=T)
#   d1 = list()
#   for(j in 1:length(stt)) {
#     d1[[j]] = subset(dat,STATION== stt[j])
#   }
#   d1 = as.data.frame(do.call(rbind,d1))
#   out = gamlss(cbind(JL,JP)~cs(rFL,df=3)+cs(meanZ,df=3),sigma.formula =~cs(rFL,df=3)+cs(meanZ,df=3) ,data=d1,family=BB())
#   mu = predict(out,what='mu',type='response',newdata = newd)
#   rho = mu / (1-mu)
#   ou[,i] = rho
# }
# 
# 
# sou = as.data.frame(cbind(newd[,1:2],t(apply(ou,1,quantile,c(0.5,.0275,.975))),(apply(ou,1,mean)),(apply(ou,1,sd))))
# names(sou) = c('Length','Depth','Median','L95','U95','Mean','SD')
# sou$CV = sou$SD / sou$Mean
# 
# souL = aggregate(cbind(Median,L95,U95)~Length,data=sou,FUN=median)
# souD = aggregate(cbind(Median,L95,U95)~Depth,data=sou,FUN=median)
# 
# ggplot(data=souL, aes(x=Length, y=Median)) + geom_line()+
#   geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
#   #  geom_point(data=ov,aes(x=rFL,y=C))+
#   labs(x=xlabs,y='Relative Catch Efficiency [Joseph Lorenzo I/Josies Pride]')+
#   geom_hline(yintercept = 1,colour='red')+
#   theme_bw()
# 
# ggplot(data=souD, aes(x=Depth, y=Median)) + geom_line()+
#   geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
#   #  geom_point(data=ov,aes(x=rFL,y=C))+
#   labs(x='Depth',y='Relative Catch Efficiency [Joseph Lorenzo I/Josies Pride]')+
#   geom_hline(yintercept = 1,colour='red')+
#   theme_bw()
# 
# 
# 
# ggplot(sou,aes(Length,Depth,z=Median))+geom_contour_filled()
# 
# ggplot(sou,aes(Length,Depth,z=CV))+geom_contour_filled()
# 
# 
# 
# ##aggregate
# vaw22 = pivot_wider(subset(va,YEAR==2022),id_cols=c(STATION, YEAR),names_from=VESSEL_NAME,values_from=N)
# vaw22$SampRatio = vaw22$`JOSEPH LORENZO I`/(vaw22$`JOSEPH LORENZO I`+vaw22$`JOSIE'S PRIDE`)
# plot(vaw22$`JOSIE'S PRIDE`,vaw22$`JOSEPH LORENZO I`)
# abline(a=0,b=1)
# 
# vaw22$JP = round(vaw22$`JOSIE'S PRIDE`)
# vaw22$JL = round(vaw22$`JOSEPH LORENZO I`)
# 
# 
# #####################################################################################################################
vg = dplyr::distinct(v,SID,SET_LONG,SET_LAT,YEAR)
vg = st_as_sf(vg,coords=c('SET_LONG','SET_LAT'),crs=4326)
p = ggLobsterMap(area='34-38', addGrids = F,return.object = T)
p+geom_sf(data=subset(vg,YEAR==2016), colour='red')+geom_sf(data=subset(vg,YEAR==2019), colour='blue')
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

 #summary table
 va = aggregate(NUM_CAUGHT~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=unique)
   aggregate(NUM_CAUGHT~GEAR,data=va,FUN=sum)
 
   va = aggregate(distance~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=unique)
   aggregate(distance~GEAR,data=va,FUN=sd)
   
   va = aggregate(spread~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=unique)
   aggregate(spread~GEAR,data=va,FUN=mean)
   
   va = aggregate(sweptArea~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=unique)
   aggregate(sweptArea~GEAR,data=va,FUN=sd)
   
   lobster.db('survey')
    surveyCatch$distSeg = sapply(1:nrow(surveyCatch), function(i) {
       distGeo(surveyCatch[i,c("SET_LONG", "SET_LAT")], surveyCatch[i, c("HAUL_LONG", "HAUL_LAT")])
     })
   
    gg = aggregate(distSeg~YEAR+GEAR+STATION+SET_NO+TRIP_ID,data=surveyCatch,FUN=unique)
    gg$ID = paste(gg$YEAR,gg$STATION)
    aggregate(distSeg~YEAR+GEAR,data=subset(gg,ID %in% cs$ID),FUN=mean)
 
with(subset(v,YEAR==2016 & GEAR=='NEST'),sum(PRORATED_NUM_AT_LENGTH))
 with(subset(v,YEAR==2019 & GEAR=='NEST'),sum(PRORATED_NUM_AT_LENGTH))
 
 with(subset(v,YEAR==2016 & GEAR=='280 BALLOON'),sum(PRORATED_NUM_AT_LENGTH))
 with(subset(v,YEAR==2019 & GEAR=='NEST'),sum(PRORATED_NUM_AT_LENGTH))
 
 
#total lobster
va = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR,data=v,FUN=sum)
names(va)[5]='N'

vaw = pivot_wider(va,id_cols=c('STATION', 'YEAR','VESSEL_NAME'),names_from=GEAR,values_from=N)



#by length
vaL = aggregate(SA_CORRECTED_PRORATED_N~STATION+GEAR+VESSEL_NAME+YEAR+rFL,data=v,FUN=sum)
names(vaL)[6]='N'


vaLw = pivot_wider(vaL,id_cols=c('STATION', 'YEAR','VESSEL_NAME', 'rFL'),names_from=GEAR,values_from=N)
names(vaLw)[6] = 'BALLOON'
vaLw = na.zero(vaLw)

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


ggplot()+
  geom_histogram(data=exp_nest,aes(x=len),fill='blue',alpha=.3) +
  geom_histogram(data=exp_balloon,aes(x=len),fill='red',alpha=.3) +
  labs(x=xlabs,y=expression(paste("Number per km",.^2)))+
  coord_cartesian(xlim = c(min(dat$rFL),max(dat$rFL))) +
  theme_bw()

## end data


##begin gamlss

ov = aggregate(cbind(NEST,BALLOON)~rFL,data=dat,FUN=sum)
ov$C = ov$NEST / ov$BALLOON
dat$C = dat$NEST / dat$BALLOON

vd = aggregate(SET_DEPTH~STATION,data=v,FUN=median)

dat = merge(dat,vd)
dat$meanZ = round(dat$SET_DEPTH)

fit0 = out0z = gamlss(cbind(NEST,BALLOON)~random(FStation),sigma.formula=~random(FStation),data=dat,family=BB(),control=gamlss.control(n.cyc = 200))

fit= out = gamlss(cbind(NEST,BALLOON)~1,data=dat,family=BB())
fit2 = out2 = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=4),sigma.formula=~cs(rFL,df=4),data=dat,family=BB())
fit2z = out2z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=4)+cs(meanZ,df=3),sigma.formula=~cs(rFL,df=4)+cs(meanZ,df=3),data=dat,family=BB())

dat$FVessel = as.factor(dat$VESSEL_NAME)
dat$FStation = as.factor(dat$STATION)

#vessel effecs are nested within both space and time, using a random effect of station instead
#fit3z = out3z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+re(random=~1|FStation),sigma.formula=~cs(rFL,df=3)+re(random=~1|FStation),data=dat,family=BB(),control=gamlss.control(n.cyc = 200))
#fit6z = out6z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+cs(meanZ,df=3)+re(random=~1|FStation),sigma.formula=~cs(rFL,df=3)+cs(meanZ,df=3)+re(random=~1|FStation),data=dat,family=BB(),control=gamlss.control(n.cyc = 200))
fit3z = out3z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+random(FStation),sigma.formula=~cs(rFL,df=3)+random(FStation),data=dat,family=BB(),control=gamlss.control(n.cyc = 200))
fit6z = out6z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=4)+cs(meanZ,df=3)+random(FStation),sigma.formula=~cs(rFL,df=4)+cs(meanZ,df=3)+random(FStation),data=dat,family=BB(),control=gamlss.control(n.cyc = 200))

model.output = data.frame(
    Mod = c('intercept','length.mu.sigma','length.z.mu.sigma','length.re(Station).sigma.mu','length.z.re(Station).sigma.mu'),
    AIC=c(AIC(fit),AIC(fit2),AIC(fit2z),AIC(fit3z),AIC(fit6z)),
    BIC=c(BIC(fit),BIC(fit2),BIC(fit2z),BIC(fit3z),BIC(fit6z)))
out = fit3z
#newd = data.frame(rFL=seq(min(dat$rFL),max(dat$rFL),by=1))
#yl=c(0,max(fit$mu.fv/(1-fit$mu.fv)))
#if(i>1) yl=c(0,max(fit$mu.fv/(1-fit$mu.fv)+1.96*sqrt(fit$mu.var) * (fit$mu.fv/(1-fit$mu.fv))))
#plot(dat$rFL, out$mu.fv/(1-out$mu.fv),type='p',xlab='Carapace Length',ylab='Relative Catch Efficiency')
#points(dat$rFL, fit3za$mu.fv/(1-fit3za$mu.fv),type='p',xlab='Carapace Length',ylab='Relative Catch Efficiency',pch=16,col='blue')
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


equal_breaks <- function(x,n = 25, s = 5,...){
    d <- s * diff(range(x)) / (1+2*s)
    seq = seq(min(x)+d, max(x)-d, length=n)
    round(seq, -floor(log10(abs(seq[2]-seq[1]))))
}

ggplot(data=d,aes(x=as.factor(rFL),y=Randomized_Quantile_Residuals))+
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.75))+
  geom_hline(yintercept = 0,color='red')+
  theme_bw()+
  #theme(axis.text.x = element_blank())+
  labs(x=xlabs)+
  scale_x_discrete(breaks=c(seq(min(d$rFL),max(d$rFL),by=10)),labels=as.character(seq(min(d$rFL),max(d$rFL),by=10)))

#no depth effect
#ggplot(data=d,aes(x=meanZ,y=Randomized_Quantile_Residuals))+
#  geom_point()+
##  geom_hline(yintercept = 0,color='red')+
#  theme_bw()+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#  labs(x="Depth")
#bootstrapping across stations- assuming that the sampling unit is the station and that each unit is a sample of the population

st = unique(dat$STATION)
niter=12000

#newd = expand.grid(rFL=seq(min(dat$rFL),max(dat$rFL),by=1),meanZ=seq(min(dat$meanZ),max(dat$meanZ),by=1),FVessel="Josie's Pride")
for(i in 2:niter){
		stt = sample(st,length(st),replace=T)
		d1 = list()
		for(j in 1:length(stt)) {
				d1[[j]] = subset(dat,STATION== stt[j])
			}
			d1 = as.data.frame(do.call(rbind,d1))
			ee =tryCatch(gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+random(FStation),sigma.formula=~cs(rFL,df=3)+random(FStation),data=d1,family=BB(),control=gamlss.control(n.cyc = 200)),
			 error = function(e) e)
			if(!inherits(ee,"error")){
			  out = ee
			} else {
			  next
			}
			newd = as.data.frame(expand.grid(rFL=seq(min(dat$rFL),max(dat$rFL),by=1),FStation=stt))
			newd$mu = predict(out,what='mu',type='response',newdata = newd)
			newd$rho = newd$mu / (1-newd$mu)
			newd$id = as.numeric(factor(newd$FStation))
if(i==1) ou = tidyr::pivot_wider(subset(newd,select=c(rFL,id,rho)), id_cols = rFL,names_from = id,values_from=rho,values_fn = mean)
      o1 = tidyr::pivot_wider(subset(newd,select=c(rFL,id,rho)), id_cols = rFL,names_from = id,values_from=rho,values_fn = mean)
			ou=		merge(ou,o1,by='rFL')
			}


sou = as.data.frame(cbind(ou[,1],t(apply(ou,1,quantile,c(0.5,.025,.975))),(apply(ou,1,mean)),(apply(ou,1,sd))))
names(sou) = c('Length','Median','L95','U95','Mean','SD')
sou$CV = sou$SD / sou$Mean

#souL = aggregate(cbind(Median,L95,U95)~Length,data=sou,FUN=median)
#souD = aggregate(cbind(Median,L95,U95)~Depth,data=sou,FUN=median)

ggplot(data=subset(sou,Length>30), aes(x=Length, y=Mean)) + geom_line()+
  geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
  #  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x=xlabs,y='Relative Catch Efficiency [NEST/BALLOON]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()

# ggplot(data=souD, aes(x=Depth, y=Median)) + geom_line()+
#   geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
#   #  geom_point(data=ov,aes(x=rFL,y=C))+
#   labs(x='Depth',y='Relative Catch Efficiency [NEST/BALLOON]')+
#   geom_hline(yintercept = 1,colour='red')+
#   theme_bw()
# 
# ggplot(sou,aes(Length,Depth,z=Median))+geom_contour_filled()

#ggplot(sou,aes(Length,Depth,z=CV))+geom_contour_filled()

##if Length only
ggplot(data=sou, aes(x=Length, y=Median)) + geom_line()+
geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
#  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x=xlabs,y='Relative Catch Efficiency [NEST/BALLOON]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()



png(file=file.path(project.datadirectory('bio.lobster'),'data','survey','ConvNestBall_2023.png'))
with(sou,{
	plot(Length,Median,xlab='Carapace Length (mm)',ylab=expression(paste('Conversion Coefficient (',rho,')')),type='l',lwd=1.5,ylim=c(0,22))
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

#flattening ends CV >.3
saveRDS(sou,file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_FINAL.rds'))

sou1 = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_FINAL.rds'))

sou$flat_Median = NA
#if depth included
# ud = unique(sou$Depth)
# for(i in 1:length(ud)){
#  #fill in all cvs>.3
#    ii = which(sou$Depth==ud[i])
#   j = which(sou$CV[ii]<= 0.3)
#   if(length(j)==0) {
#     iii = which(sou$Depth==ud[i-1])
#     sou$flat_Median[ii] = sou$flat_Median[iii]
#     next
#   }
#   sou$flat_Median[ii[j]] = sou$Median[ii[j]]
#   k = which(!is.na(sou$flat_Median[ii]))
#   ik = 1:(k[1]-1)
#   sou$flat_Median[ii[ik]] =sou$flat_Median[ii[k[1]]] 
#   ik = (k[length(k)]+1):length(ii)
#   sou$flat_Median[ii[ik]] =sou$flat_Median[ii[k[length(k)]]] 
# }

#flattening out unmodelled sizes
fi = data.frame(Length=1:220)
mi = min(sou1$Length)
mx = max(sou1$Length)
sou1 = merge(sou1,fi,all=T)
i = which(sou1$Length<mi)
ii = which(sou1$Length==mi)
sou1[i,c('Median','L95','U95','Mean','SD','CV')] = sou1[ii,c('Median','L95','U95','Mean','SD','CV')]

i = which(sou1$Length>mx)
ii = which(sou1$Length==mx)

sou1[i,c('Median','L95','U95','Mean','SD','CV')] = sou1[ii,c('Median','L95','U95','Mean','SD','CV')]

saveRDS(sou1,file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_FINAL.rds'))


vv = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall.rds'))
