

#maturity by temperature
require(bio.lobster)
require(bio.utilities)
options(stringsAsFactors=F)
gsf = readRDS(file.path(project.datadirectory('bio.lobster'),'analysis','ClimateModelling','GlorysClimatologies1993-2022byDOYWithLFA.rds'))
mat = read.csv(file.path(project.datadirectory('bio.lobster'),'data','maturity','haarPaper','Rawdatamaturitypaper.csv'))

mat = subset(mat,LFA %in% c(27,29,'30','31A','31B','32','33','34','35','36','38'))
mat$yr = mat$study.year..mean.
mat$SOM =NA
mat$SOM = ifelse(!is.na(mat$SOM.....CL.mm.) & !is.na(mat$CL50...mm.) & mat$SOM.....CL.mm.> mat$CL50...mm.,mat$CL50...mm.,mat$SOM) 
mat$SOM = ifelse(!is.na(mat$SOM.....CL.mm.) & !is.na(mat$CL50...mm.) & mat$SOM.....CL.mm.< mat$CL50...mm.,mat$SOM.....CL.mm.,mat$SOM)

mat$SOM = ifelse(is.na(mat$SOM.....CL.mm.) & !is.na(mat$CL50...mm.),mat$CL50...mm.,mat$SOM)
mat$SOM = ifelse(!is.na(mat$SOM.....CL.mm.) & is.na(mat$CL50...mm.),mat$SOM.....CL.mm.,mat$SOM)

matr = subset(mat,select=c(yr,SOM,LFA))
matrA = aggregate(SOM~LFA,data=matr,FUN=mean)
boxplot(SOM~LFA,data=matr)


i = which(gsf$z>2 & gsf$z<30)
j = which(gsf$LFA %in% c(27,29,30,311,312,32,33,34,35,36,38))
ij = intersect(i,j)
gsf$i=0
gsf$i[ij] = 1

#j = which(gsf$LFA %in% c(33,34,35,36,38))
#gsf$i[j] = 1

gsfr = subset(gsf,i==1)
gsfr$geometry <- NULL

ii=grep('BT.19',names(gsfr))

xl = split(gsfr,f=gsfr$LFA)
ou = list()

for(i in 1:length(xl)){
	x = xl[[i]]
	xx = dim(x)[1]/365
	ik = as.vector(as.matrix(x[,ii]))
	my = mean(ik)
	da = data.frame(doy = rep(1:365,each=xx),t = ik)
	da = aggregate(t~doy,data=da,FUN=mean)[,2]
	a8 = sum(da[which(da>8)]-8)
	xw = subset(x,doy %in% c(320:365,1:45))
	xsp = subset(x,doy %in% 45:135)
	xsu = subset(x,doy %in% 136:215)
	xfa = subset(x,doy %in% 215:320)
	
	xw = mean(as.vector(as.matrix(xw[,ii])))
	xsp = mean(as.vector(as.matrix(xsp[,ii])))
	xsu = mean(as.vector(as.matrix(xsu[,ii])))
	xfa = mean(as.vector(as.matrix(xfa[,ii])))
	ou[[i]] = c(my,a8,xw,xsp,xsu,xfa,unique(x$LFA))				
}

ou = as.data.frame(do.call(rbind,ou))
ou = toNums(ou,1:7)
names(ou) = c('meanT','ddA8','winterT','springT','summerT','fallT','LFA')

matrA$LFA = ifelse(matrA$LFA=='31A',311, matrA$LFA)
matrA$LFA = ifelse(matrA$LFA=='31B',312, matrA$LFA)

dat = merge(matrA,ou)

m=cor(dat[,2:ncol(dat)])

require(corrplot)

corrplot(m,method='number')

with(dat,plot(springT,SOM))