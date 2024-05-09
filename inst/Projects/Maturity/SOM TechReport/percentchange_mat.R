options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(dplyr)
require(statmod)
require(reshape2)

p = bio.lobster::load.environment()
la()
setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles")


######### Figure of % Increase ###############



matFun = function(a,b,cl){
  1/(1+exp(a+b*cl))
}

repFun = function(a,b,cl){
  a*exp(b*cl)
}

cl=c(50:150)


#howse and armsworthy 2024 new
#l33 = c(16.78,-0.186)
#l36 = c(19.15,-0.216)
#l38 = c( 20.06,-0.227)

#howse and armsworthy 2024 new
l33 = c(16.78,-0.19)
l36 = c(19.15,-0.22)
l38 = c(20.06,-0.23)


##howse and armsworthy 2024 old
#l31a = c(13.12,-0.17)
#l32 = c(13.77,-0.15)

##howse and armsworthy 2024 old
l31a = c(11.47,-0.15)
l32 = c(13.79,-0.15)



#Size fecundity
sf_gsl =c( 0.000542, 3.77922)# Campbell & Robinson 1983
sf_ens =c( 0.000251, 3.9592) # Campbell & Robinson 1983
sf_bof =c( 0.007236, 3.19005)# Campbell & Robinson 1983

#current MLS
dat = list(
  LFA31A=list(som=l31a,sf = sf_ens,mls=82.5,s1=0.16,s2=.28),
  LFA32=list(som=l32,sf = sf_ens,mls=82.5,s1=0.14,s2=.25),
  LFA33=list(som=l33,sf = sf_bof,mls=82.5,s1=.18,s2=.3),
  LFA36=list(som=l36,sf = sf_bof,mls=82.5,s1=.14,s2=.24),
  LFA38=list(som=l38,sf = sf_bof,mls=82.5,s1=.14,s2=.22)
)

out = data.frame(LFA=NA,som=NA,cPM=NA,onePM=NA,twoPM=NA)
matts = list()
for(i in 1:length(dat)){
  b = dat[[i]]
  out[i,'LFA'] = names(dat)[i]
  mat = matFun(a=b$som[1],b=b$som[2],cl=cl)
  out[i,'som']=findValue(cl,mat,.5)
  if(b$mls<84) out[i,'cPM'] = mean(c(mat[which.min(abs(cl-b$mls))],mat[which.min(abs(cl-b$mls))+1]))
  if(b$mls==84) out[i,'cPM'] = mat[which.min(abs(cl-84))]
  
  out[i,'onePM'] = mat[which.min(abs(cl-84))]
  out[i,'twoPM'] = mat[which.min(abs(cl-86))]
  matts[[i]] = data.frame(cl=cl,mat=mat,lfa=rep(toupper(names(dat)[i]),times=length(cl)))
}

mm = as.data.frame(do.call(rbind,matts))
mm = toNums(mm,c(1:2))



vlines=data.frame(lfa=rep(c('LFA31A','LFA32','LFA33','LFA36','LFA38'),each=3),
                  li = rep(c(82.5,84,86),times=5),line_color = rep(c('#5c53a5','#38b2a3','#ffa600'),times=5), MLS_Steps =c("82.5mm","84mm","86mm"),times=5)



ggplot(subset(mm,cl>60 & cl<110),aes(x=cl,y=mat))+geom_line(linewidth = 0.75)+
  geom_vline(data = vlines, linewidth = 0.75,aes(xintercept = li, colour=MLS_Steps, show.legend =T) )+
  facet_wrap(~lfa)+xlab('Carapace Length (mm)')+ylab('Proportion Mature')+
  scale_color_manual(values=setNames(vlines$line_color,vlines$MLS_Steps))+
  theme_bw()


out$perInc84 = out$perInc86 = NA
for(i in 1:nrow(out)){
  out$perInc84[i] = (out$onePM[i]-out$cPM[i])/out$cPM[i]*100
  out$perInc86[i] = (out$twoPM[i]-out$cPM[i])/out$cPM[i]*100
}

PerInc<-out%>%
  dplyr::select(LFA,som,perInc86, perInc84)

ggplot(PerInc, aes(x = som)) +
  geom_point(aes(y = perInc84, colour = "perInc84", shape = "perInc84"), size = 3) +
  geom_text(aes(y = perInc84, label = LFA), hjust = -0.1, vjust = -0.8, size = 4) +
  geom_point(aes(y = perInc86, colour = "perInc86", shape = "perInc86"), size = 3) +
  geom_text(aes(y = perInc86, label = LFA), hjust = -0.1, vjust = -0.8, size = 4) +
  labs(x = "Size at 50% Maturity (mm)", y = "Percent Increase") +
  ylim(5, 90) + xlim(75, 95) +
  scale_color_manual(name = "Increased MLS",
                     values = c("perInc84" = "#ffa600", "perInc86" = "#38b2a3"),
                     labels = c("perInc84" = "84mm", "perInc86" = "86mm")) +
  scale_shape_manual(name = "Increased MLS",
                     values = c("perInc84" = 17, "perInc86" = 16),
                     labels = c("perInc84" = "84mm", "perInc86" = "86mm")) +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(shape = c(17, 16))))


PerInc %>% mutate(perInc86 = round(perInc86, 2))
