#Female Proportions at size


require(bio.lobster)
require(bio.utilities)
require(devtools)
require(lubridate)
require(XLConnect)
load_all('~/git/bio.survey/')

la()

wd = file.path(project.datadirectory('bio.lobster'),'PopModelInputs')
setwd(wd)
da = read.csv('femPropAtSize.csv')
Sz = seq(53,223,5)

daA = apply(da,2,mean)
plot(Sz,daA,type='h')