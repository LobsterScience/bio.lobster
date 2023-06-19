#revamped TCAI

require(bio.lobster)
require(bio.utilities)
require(mgcv)
require(devtools)
require(sf)
require(dplyr)

	lobster.db('ccir')
	ccir_data$Ref = ifelse(ccir_data$Size==ccir_data$MLS_FSRS &ccir_data$Short==1 | ccir_data$Size==(ccir_data$MLS_FSRS-1),1,0)
	ccir_data$Legal = ifelse(ccir_data$Size==ccir_data$MLS_FSRS &ccir_data$Short==0 | ccir_data$Size>=(ccir_data$MLS_FSRS),1,0)
	ccir_data$wt = NA
	i=which(ccir_data$YEAR<2019)
	ccir_data$wt[i] = lobLW(ccir_data$Size[i],fsrs.old = T)
	i=which(ccir_data$YEAR>=2019)
	ccir_data$wt[i] = lobLW(ccir_data$Size[i],fsrs.old = F,fsrs = T)
	ccir_data$wt = ccir_data$wt * ccir_data$Legal
	ccir_data$UID = paste(ccir_data$Vessel.Code,ccir_data$DATE,ccir_data$Trap.Number)		

	cd = aggregate(cbind(Ref,Legal,wt)~UID+X+Y+LFA+DATE+Temperature+YEAR,data=ccir_data,FUN=sum)
	cd = subset(cd,Temperature> -1)

	d = cd  %>% st_as_sf(coords=c('X',"Y"),crs=4326) %>% st_transform(32620)
	d$ly = paste(d$LFA,d$YEAR,sep="-")

	u = unique(d$ly)
	junk = list()

#id all traps within an LFA and year that are within 3km
	for(i in 1:length(u)){
				v = subset(d,ly==u[i])
				vv = st_buffer(v,3000)
				vvv = st_intersects(v,vv)
				v$ID = NA
				for(j in 1:length(vvv)){
					n = vvv[[j]]
					k = which(is.na(v$ID))
					n = intersect(n,k)
					if(length(n)>0)	v$ID[n]=j
					if(length(n)==0) stop
				}
				junk[[i]] = v
			}

dc = dplyr::bind_rows(junk)
dc$X = st_coordinates(dc)[,1]
dc$Y = st_coordinates(dc)[,2]

dca = aggregate(cbind(Ref,Legal,wt)~ID+YEAR+DATE+LFA,data=dc,FUN=sum)
dcx = aggregate(cbind(X,Y,Temperature)~ID+YEAR+DATE+LFA,data=dc,FUN=mean)
dcl = aggregate(UID~ID+YEAR+DATE+LFA,data=dc,FUN=function(x) length(unique(x)))

a = list(dca,dcx,dcl) %>% purrr::reduce(inner_join)
a$LFA = as.factor(a$LFA)
a$YEAR = as.factor(a$YEAR)
a=as_tibble(a)
a = subset(a,LFA %ni% c(28,35,34,36) & Ref<40 & Legal<40)
a$wt = a$wt/1000
go = gam(Ref~LFA+YEAR+s(Temperature,k=4),data=a,offset=(log(UID)),family='nb')
go3 = gam(Ref~LFA+YEAR+ti(Temperature)+ti(Legal)+ti(Temperature,Legal),data=a,offset=(log(UID)),family='nb') #best
go3a = gam(Ref~LFA+YEAR+(Temperature)+ti(Legal)+ti(Temperature,Legal),data=a,offset=(log(UID)),family='nb')

go4 = gam(Ref~LFA+YEAR+ti(Temperature)+ti(wt)+ti(Temperature,wt),data=a,offset=(log(UID)),family='nb')
go5 = gam(Ref~LFA+YEAR+(Temperature)+ti(wt)+ti(Temperature,wt),data=a,offset=(log(UID)),family='nb')

gp = predict(go,type='lpmatrix')
nm =  grep('Temp',dimnames(gp)[[2]])

require(ggeffects)

mydf <- ggpredict(go, terms = c('Temperature'))
plot(mydf)

mydf <- ggpredict(go3, terms = c('Temperature'))
plot(mydf)

mydf <- ggpredict(go4, terms = c('wt'))
plot(mydf)

mydf <- ggpredict(go5, terms = c('Temperature'))
plot(mydf)



ggplot(mydf, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  facet_wrap(~group)+
  xlab('Temperature')+
  ylab('Marginal Catchability')

newd = data.frame(Temperature=seq(0,23,.2),Legal=10,LFA=27,YEAR=as.factor(2021))
myf <- ggpredict(go3, terms = c("Temperature"),newdata=newd,full.data=F)
ggplot(myf, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)

newd = data.frame(Temperature=seq(0,23,.1),Legal=10,LFA=33,YEAR=as.factor(2021))
newd$pred = predict(go3,type='response',newdata = newd)
  pre = subset(newd,select=c(Temperature, pred))

saveRDS(pre,file=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','tempCatchability.rds'))
