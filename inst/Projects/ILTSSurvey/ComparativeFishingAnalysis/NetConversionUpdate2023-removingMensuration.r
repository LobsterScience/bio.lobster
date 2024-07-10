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

v$ss = v$SA_CORRECTED_PRORATED_N*v$sweptArea

va = aggregate(spread~STATION+GEAR+YEAR,data=v,FUN=unique)

l=ggplot(va,aes(x=spread,fill=GEAR))+geom_histogram(aes(y=after_stat(density)))+geom_vline(xintercept=c(12,20),col=c('black','black'),lwd=1.4,linetype=c(1,2))+ylab('Density')+xlab('Wing Spread')



lobster.db('survey')
surveyCatch$distSeg = sapply(1:nrow(surveyCatch), function(i) {
  distGeo(surveyCatch[i,c("SET_LONG", "SET_LAT")], surveyCatch[i, c("HAUL_LONG", "HAUL_LAT")])
})

gg = aggregate(distSeg~YEAR+GEAR+STATION+SET_NO+TRIP_ID,data=surveyCatch,FUN=unique)
gg$ID = paste(gg$YEAR,gg$STATION)
v = merge(v,gg[,c('TRIP_ID','SET_NO','STATION','GEAR','distSeg')],all.x=T)
v$distSeg = v$distSeg/1000
ll=ggplot(v,aes(x=distSeg,y=distance,color=GEAR))+geom_point()+ylab('Mensuration Distance')+xlab('Winch Distance')

v$spread = ifelse(v$GEAR=='NEST',12,20)
v$SA1 = v$spread/1000* v$distSeg
lll=ggplot(v,aes(x=SA1,y=sweptArea,color=GEAR))+geom_point()+ylab('Mensuration Swept Area')+xlab('Winch and Nominal Wing Swept Area')
lll=ggplot(v,aes(x=diff,fill=GEAR))+geom_histogram(aes(y=after_stat(density)))+ylab('Density')+xlab('Difference in lobster density')+geom_vline(xintercept=c(0),col=c('black'),lwd=1.4,linetype=c(1))

v$SA_CORRECTED_PRORATED_N_1 = v$ss/(v$distSeg * (v$spread/1000))
plot(v$SA_CORRECTED_PRORATED_N,v$SA_CORRECTED_PRORATED_N_1)
 abline(a=0,b=1)

 va = aggregate(cbind(SA_CORRECTED_PRORATED_N_1,SA_CORRECTED_PRORATED_N)~STATION+GEAR+VESSEL_NAME+YEAR+SID,data=v,FUN=sum)
 va$diff = va$SA_CORRECTED_PRORATED_N_1 - va$SA_CORRECTED_PRORATED_N #if mensuration is higher then negative
 hist(va$diff)
 llll=ggplot(va,aes(x=diff,fill=GEAR))+geom_histogram(aes(y=after_stat(density)))+ylab('Density')+xlab('Difference in lobster density')+geom_vline(xintercept=c(0),col=c('black'),lwd=1.4,linetype=c(1))
 ggpubr::ggarrange(l,ll,lll,llll,common.legend = T)
 
 plot(log(va$SA_CORRECTED_PRORATED_N),log(va$SA_CORRECTED_PRORATED_N_1))
 abline(a=0,b=1)
 
 names(va)[13]='N'


vaL = aggregate(SA_CORRECTED_PRORATED_N_1~STATION+GEAR+VESSEL_NAME+YEAR+rFL,data=v,FUN=sum)
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



##begin gamlss

ov = aggregate(cbind(NEST,BALLOON)~rFL,data=dat,FUN=sum)
ov$C = ov$NEST / ov$BALLOON
dat$C = dat$NEST / dat$BALLOON

vd = aggregate(SET_DEPTH~STATION,data=v,FUN=median)

dat = merge(dat,vd)
dat$meanZ = round(dat$SET_DEPTH)

dat$FStation = as.factor(dat$STATION)

fit3v = out3z = gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+random(FStation),sigma.formula=~cs(rFL,df=3)+random(FStation),data=dat,family=BB(),control=gamlss.control(n.cyc = 200))
outv = fit3v

st = unique(dat$STATION)
niter=length(st)

#newd = expand.grid(rFL=seq(min(dat$rFL),max(dat$rFL),by=1),meanZ=seq(min(dat$meanZ),max(dat$meanZ),by=1),FVessel="Josie's Pride")
for(i in 1:niter){
		stt = sample(st,length(st),replace=T)
		d1 = list()
		for(j in 1:length(stt)) {
				d1[[j]] = subset(dat,STATION== stt[j])
			}
			d1 = as.data.frame(do.call(rbind,d1))
			ee =tryCatch(gamlss(cbind(NEST,BALLOON)~cs(rFL,df=3)+re(random=~1|FStation),sigma.formula=~cs(rFL,df=3)+re(random=~1|FStation),data=d1,family=BB(),control=gamlss.control(n.cyc = 200)),
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


souv = as.data.frame(cbind(ou[,1],t(apply(ou,1,quantile,c(0.5,.025,.975))),(apply(ou,1,mean)),(apply(ou,1,sd))))
names(souv) = c('Length','Median','L95','U95','Mean','SD')
souv$CV = souv$SD / sou$Mean

#souL = aggregate(cbind(Median,L95,U95)~Length,data=sou,FUN=median)
#souD = aggregate(cbind(Median,L95,U95)~Depth,data=sou,FUN=median)

ggplot(data=subset(souv,Length>30), aes(x=Length, y=Mean)) + geom_line()+
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
ggplot(data=souv, aes(x=Length, y=Median)) + geom_line()+
geom_ribbon(aes(ymin=L95, ymax=U95), linetype=2, alpha=0.1)+
#  geom_point(data=ov,aes(x=rFL,y=C))+
  labs(x=xlabs,y='Relative Catch Efficiency [NEST/BALLOON]')+
  geom_hline(yintercept = 1,colour='red')+
  theme_bw()



png(file=file.path(project.datadirectory('bio.lobster'),'data','survey','ConvNestBall_2023.png'))
with(souv,{
	plot(Length,Median,xlab='Carapace Length (mm)',ylab=expression(paste('Conversion Coefficient (',rho,')')),type='l',lwd=1.5,ylim=c(0,22))
	lines(Length,L95,type='l',lwd=1.5,lty=2)
	lines(Length,U95,type='l',lwd=1.5,lty=2)
	points(ov$rFL,ov$C,pch=16,cex=.75)
	#points(dat$rFL,dat$C,pch=16,col=rgb(0,0,1,alpha=.1),cex=.75)
})
abline(h=1,col='red',lwd=1.5)

with(sou,{
  lines(Length,Median,xlab='Carapace Length (mm)',ylab=expression(paste('Conversion Coefficient (',rho,')')),type='l',lwd=1.5,ylim=c(0,22),col='blue')
  lines(Length,L95,type='l',lwd=1.5,lty=2,col='blue')
  lines(Length,U95,type='l',lwd=1.5,lty=2,col='blue')
  #points(dat$rFL,dat$C,pch=16,col=rgb(0,0,1,alpha=.1),cex=.75)
})


dev.off()

png(file=file.path(project.datadirectory('bio.lobster'),'data','survey','CVConvNestBall_2023.png'),type='png')
plot(sou$Length,sou$CV,type='l',xlab = 'Carapace Length',ylab = expression(paste('CV of ',rho)),lwd=1.5)
dev.off()
####end gamlss

#flattening ends CV >.3
saveRDS(sou,file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_2023.rds'))

sou1 = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_2023.rds'))

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

#flattening out high cvs from length only model
sou$flat_Median = NA
   j = which(sou$CV<= 0.3)
   sou$flat_Median[j] = sou$Median[j]
   k = which(!is.na(sou$flat_Median[ii]))
   ik = 1:(k[1]-1)
  sou$flat_Median[ik] =sou$flat_Median[k[1]]
   ik = (k[length(k)]+1):length(ii)
   sou$flat_Median[ik] =sou$flat_Median[k[length(k)]]
# 
saveRDS(sou,file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall_2023.rds'))


vv = readRDS(file=file.path(project.datadirectory('bio.lobster'),'data','survey','summarybootRhoNestBall.rds'))
