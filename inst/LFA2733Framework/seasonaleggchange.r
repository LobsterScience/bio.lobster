###eggs

h = lobster.db('atSea.clean')
head(h)
h = subset(h,LFA==27 & SYEAR>2010)
h = subset(h,SEX==3)
h$GR = ifelse(h$WOS<=3,1,ifelse(h$WOS>3 & h$WOS<=6,2,3))

j = aggregate(CULL~GR+EGG,data=h,FUN=length)
	jj = aggregate(CULL~GR,data=j,FUN=sum)

j = merge(j,jj,by='GR',all.x=T)

j$prp = j$CULL.x/j$CULL.y
plot(1,1,xlim=c(1,3),ylim=c(0,1))
with(subset(j,EGG==1),lines(GR,prp))
with(subset(j,EGG==2),lines(GR,prp,col='red'))
with(subset(j,EGG==3),lines(GR,prp,col='green'))
with(subset(j,EGG==4),lines(GR,prp,col='blue'))

