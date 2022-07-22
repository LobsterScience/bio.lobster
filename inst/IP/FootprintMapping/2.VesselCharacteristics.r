#Vessel characterisits

require(bio.lobster)
require(bio.utilities)
require(devtools)
require(ggplot2)
la()


a = lobster.db('vessels.by.port.redo')
a = toNums(a,'YR_FISHED')
a$AGE.of.Boat = a$YR_FISHED - a$YEAR_BUILT
a = subset(a,AGE.of.Boat<100)

#how many ports

aa = aggregate(PORT~VR_NUMBER+LFA+YR_FISHED,data=a,FUN=function(x) length(unique(x)))

aaa = aggregate(PORT~LFA+YR_FISHED,data=aa,FUN=mean)

ggplot(data=subset(aaa,!LFA %in% c(24,28) & YR_FISHED >2006)) + geom_line(aes(x=YR_FISHED,y=PORT)) + facet_wrap(~LFA)
