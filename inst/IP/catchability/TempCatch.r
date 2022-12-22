#revamped TCAI

require(bio.lobster)
require(bio.utilities)
require(mgcv)
require(devtools)
require(sf)
require(dplyr)

	lobster.db('ccir')
	ccir_data$Ref = ifelse(ccir_data$Size==ccir_data$MLS_FSRS &ccir_data$Short==1 | ccir_data$Size==(ccir_data$MLS_FSRS-1),1,0)
	ccir_data$UID = paste(ccir_data$Vessel.Code,ccir_data$DATE,ccir_data$Trap.Number)		

	cd = aggregate(Ref~UID+X+Y+LFA+DATE+Temperature+YEAR,data=ccir_data,FUN=sum)
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

dca = aggregate(Ref~ID+YEAR+DATE+LFA,data=dc,FUN=sum)
dcx = aggregate(cbind(X,Y,Temperature)~ID+YEAR+DATE+LFA,data=dc,FUN=mean)
dcl = aggregate(UID~ID+YEAR+DATE+LFA,data=dc,FUN=function(x) length(unique(x)))

a = list(dca,dcx,dcl) %>% purrr::reduce(inner_join)
a$LFA = as.factor(a$LFA)
a$YEAR = as.factor(a$YEAR)
a=as_tibble(a)
a = subset(a,LFA %ni% c(28,35,34,36))
go = gam(Ref~LFA+YEAR+s(Temperature,k=4),data=a,offset=(log(UID)),family='nb')

require(ggeffects)

mydf <- ggpredict(go, terms = c("Temperature",'LFA','YEAR'))

ggplot(mydf, aes(x = x, y = predicted, colour = facet)) +
  geom_line() +
  facet_wrap(~group)


myf <- ggpredict(go, terms = c("Temperature"))

  pre = data.frame(temp=myf$x,pred = myf$predicted)

saveRDS(pre,file=file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','tempCatchability.rds'))
