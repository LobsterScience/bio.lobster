
require(bio.survey)
require(bio.lobster)
require(bio.utilities)
fp = file.path("C:/Users/HowseVJ/OneDrive - DFO-MPO/LFA 35-38 Framework Resources/Figures")

p=list()
######### RV Survey ######### 

#Establish Arguments for Stratified Analysis
p$series =c('summer')# p$series =c('georges');p$series =c('fall')
p$define.by.polygons = F
p$lobster.subunits=F
p$area = 'all'
p$years.to.estimate = c(1970:2024) ## Might need to go back to 2023 if survey Data isn't up yet
p$length.based = F
p$by.sex = F
p$bootstrapped.ci=F
p$strata.files.return=F
p$vessel.correction.fixed=1.2
p$strat = NULL
p$clusters = c( rep( "localhost", 7) )
p$strata.efficiencies = F
p$strata.files.return=T
p = make.list(list(yrs=p$years.to.estimate),Y=p)



# DFO survey All stations including adjacent
p$define.by.polygons = F
p$lobster.subunits=F
p$reweight.strata = F #this subsets 
aout= dfo.rv.analysis(DS='stratified.estimates.redo',p=p,save=F)
aa = do.call(rbind,lapply(aout,function(X) X[[2]])) #return just the strata data
aa$yr = substr(aa$mission,4,7)
YG = 5 # year grouping
y = unique(aa$yr)
yL = y[length(y)] #last year
yLL = length(y)-1
yLm = yLL %% YG
yLr = yLL %/% YG
yw = y[which(y %in% y[1:yLm])] #add the early years to the first histogram and keep the rest at 5 years


### Define the group to only run the recent years * Group 10= 2018-2024

yLw = c(rep(1,yLm),rep(1:yLr,each = YG),yLr+1)
grps = data.frame(yr = y,ry = yLw)
defined.groups=T
if(defined.groups){
 grps = data.frame(yr = y)
  grps$ry = ifelse(grps$yr %in% 1969:1980,1,
                   ifelse(grps$yr %in% 1981:1990,2,
                          ifelse(grps$yr %in% 1991:1998,3,
                                 ifelse(grps$yr %in% 1999:2009,4,5))))
}

aa = merge(aa,grps,by='yr',all.x=T)

h = split(aa,f=aa$ry)
hnew <- h[10:11]

for(i in 1:length(hnew)) {
  j = hnew[[i]]
  pdf(file.path(fp,paste('surveyBubblesDFOSummer',min(j$yr),max(j$yr),'pdf',sep=".")))
  LobsterMap('35-38',boundaries='LFAs',addSummerStrata=T,save=F,labcex =0.8,labels=F)
  j = makePBS(j,polygon=F)
  j$Z =j$totno
  addPolys(LFA34,border='red')
  addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
  legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
  dev.off()	
}


############## Just do 2019-2024  #####################
for(i in 1:length(h)) {
  j = h[[i]]
  pdf(file.path(fp,paste('surveyBubblesDFOSummer',min(j$yr),max(j$yr),'pdf',sep=".")))
  ggLobsterMap('35-38',boundaries='LFAs',save=F,labcex =0.8,labels=F)
  j = makePBS(j,polygon=F)
  j$Z =j$totno
  addPolys(LFA34,border='red')
  addBubbles(j,legend.pos='bottomright',legend.type='horiz',legend.cex=0.8,symbol.zero=".",max.size=0.8,z.max=1000,type='surface',symbol.bg = rgb(1,0,0,.6))
  legend('bottomleft',bty='n',pch="", legend=paste(min(j$yr),max(j$yr),sep="-"),cex=1.5)
  dev.off()	
}


#g = ggLobsterMap(return.object=T)
#ggplots g+geom_sf(survey_points)


## Makes Tables 4-6.... Summary of survey information for LFA 35, 36, 38 from RV, Scallop and Lobster
## Then make the plots of survey locations
## Then plot lobster abundance on maps

