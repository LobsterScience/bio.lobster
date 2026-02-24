require(bio.lobster)
require(devtools)
require(bio.utilities)
la()
#maturity curve

matFun = function(a,b,cl){
  1/(1+exp(a+b*cl))
}

repFun = function(a,b,cl){
  a*exp(b*cl)
}

cl=c(50:150)


#ellerston et al. 2022
offSNE=c(14.216,-0.17885)
eGB=c(32.441,-0.35311)


oSNE = matFun(a=offSNE[1],b=offSNE[2],cl=cl)
#findValue(cl,oSNE,.5) #matches paper 

eGB = matFun(a=eGB[1],b=eGB[2],cl=cl)
#findValue(cl,eGB,.5) #matches paper 


#GOM ASMFC

gom = c(27.243,-.300)
gbk = c(18.256,-.183)
sne = c(14.288,-.188)

#howse and armsworthy 2024 new
l33 = c(16.78,-0.186)
l36 = c(19.15,-0.216)
l38 = c( 20.06,-0.227)

##howse and armsworthy 2024 old
l31a = c(13.12,-0.17)
l31b = c(9.94,-0.121) #modelling 31a and 32 in same dataset
l32 = c(13.77,-0.15)
l34 = l33

#sgls comeau and savoie 2002
sgsl = c(16.94,-.239)
sGSL = matFun(a=sgsl[1],b=sgsl[2],cl=cl)
#findValue(cl,sGSL,.5)


#old ones used in sim models #watson and pezzack and reeves
#l33 =c(24.87275, -0.25725)
#l34 = c(22.37302,-0.23187) #DFO 2013/024 SAR
l29 =c(14.17300, -0.17270)
l30 =c(16.50500, -0.21320)
l27 = c(14.26600, -0.19590)
l35 =c(22.96000, -0.24000)
#lfa 41 cook framework
l41 = c(22.5522,-0.2455)


#Size fecundity
sf_gsl =c( 0.000542, 3.77922)# Campbell & Robinson 1983
sf_ens =c( 0.000251, 3.9592) # Campbell & Robinson 1983
sf_bof =c( 0.007236, 3.19005)# Campbell & Robinson 1983

#current MLS
dat = list(
  l27=list(som=l27,sf = sf_ens,mls=82.5,s1=.24,s2=.41),
  l29=list(som=l29,sf = sf_ens,mls=84,s1=0,s2=.31),
  l30=list(som=l30,sf = sf_ens,mls=82.5,s1=0.15,s2=.29),
  l31A=list(som=l31a,sf = sf_ens,mls=82.5,s1=0.16,s2=.28),
  l31B=list(som=l31b,sf = sf_ens,mls=82.5,s1=0.13,s2=.25),
  l32=list(som=l32,sf = sf_ens,mls=82.5,s1=0.14,s2=.25),
  l33=list(som=l33,sf = sf_bof,mls=82.5,s1=.18,s2=.3),
  l34=list(som=l34,sf = sf_bof,mls=82.5,s1=.18,s2=.3),
  l35=list(som=l35,sf = sf_bof,mls=82.5,s1=.15,s2=.26),
  l36=list(som=l36,sf = sf_bof,mls=82.5,s1=.14,s2=.24),
  l38=list(som=l38,sf = sf_bof,mls=82.5,s1=.14,s2=.22),
  l41=list(som=l41,sf = sf_bof,mls=82.5,s1=.01,s2=.02)
)
#growth matrix
load_all('C:/Users/Cooka/Documents/git/LobsterHCR/')
p=list()
o = moltIncrModel(p=p,redo=F,sex=2)
o = o[[1]][1:length(cl),1:length(cl)]
o = na.zero(o)

out = data.frame(LFA=NA,som=NA,cPM=NA,mulcPM=NA,onePM=NA,twoPM=NA,mult_onePM=NA, mult_twoPM=NA)
matts = list()
for(i in 1:length(dat)){
  b = dat[[i]]
  out[i,'LFA'] = names(dat)[i]
  mat = matFun(a=b$som[1],b=b$som[2],cl=cl)
  #prop multiparous
  p2nd = as.numeric(t(mat) %*% o)
  #p2nd = p2nd/max(p2nd)
  p2nd_given_mature <- p2nd / mat
  #plot(cl,p2nd_given_mature)
  lines(cl,mat)
  out[i,'som']=findValue(cl,mat,.5)
  if(b$mls==82.5) out[i,'cPM'] = mean(c(mat[which.min(abs(cl-b$mls))],mat[which.min(abs(cl-b$mls))+1])) #maturity at 82.5
  if(b$mls==84) out[i,'cPM'] = mat[which.min(abs(cl-84))]
  if(i==6) browser()
  if(b$mls==82.5) out[i,'mulcPM'] = mean(c(p2nd_given_mature[which.min(abs(cl-b$mls))],p2nd_given_mature[which.min(abs(cl-b$mls))+1])) #maturity at 82.5
  if(b$mls==84) out[i,'mulcPM'] = p2nd_given_mature[which.min(abs(cl-84))]
  
  
  out[i,'onePM'] = mat[which.min(abs(cl-84))]
  out[i,'twoPM'] = mat[which.min(abs(cl-86))]
  
  out[i,'mult_onePM'] = p2nd_given_mature[which.min(abs(cl-84))]
  out[i,'mult_twoPM'] = p2nd_given_mature[which.min(abs(cl-86))]
  
  
  matts[[i]] = data.frame(cl=cl,mat=mat,prop_multip=p2nd_given_mature,lfa=rep(toupper(names(dat)[i]),times=length(cl)))
  }

mm = as.data.frame(do.call(rbind,matts))
mm = toNums(mm,c(1:3))

vlines=data.frame(lfa=rep(c('L27','L29','L30','L31A','L31B','L32','L33','L34','L35','L36','L38','L41'),each=3),
                  li = rep(c(82.5,84,86),times=12),cols = rep(c('blue','black','red'),times=12))
vlines[4,'li']=84

require(ggplot2)
ggplot(subset(mm,cl>60 & cl<120 & lfa!='L41'),aes(x=cl,y=mat))+geom_line()+
  geom_vline(data = subset(vlines,lfa %ni% 'L41'), aes(xintercept = li, colour=cols) )+
  facet_wrap(~lfa)+xlab('Carapace Length')+ylab('Proportion Mature')+
  theme_test()

ggplot(subset(mm,cl>60 & cl<120),aes(x=cl,y=prop_multip))+geom_line()+
  geom_vline(data = vlines, aes(xintercept = li, colour=cols) )+
  facet_wrap(~lfa)+xlab('Carapace Length')+ylab('Proportion Multiparous')+
  theme_test()

#just MLS
require(dplyr)
vl <- vlines %>%
  group_by(lfa) %>%
  slice(1) %>%
  ungroup()

ggplot(subset(mm,cl>60 & cl<120 & lfa!='L41'),aes(x=cl,y=mat))+geom_line()+
  geom_vline(data = subset(vl,lfa %ni% 'L41'), aes(xintercept = li, colour=cols) )+
  facet_wrap(~lfa)+xlab('Carapace Length')+ylab('Proportion Mature')+
  theme_test(base_size = 14)+theme(legend.position = "none")



out$multip_perInc84 = out$multip_perInc86 = out$perInc84 = out$perInc86 = NA
for(i in 1:nrow(out)){
out$perInc84[i] = (out$onePM[i]-out$cPM[i])/out$cPM[i]*100
out$perInc86[i] = (out$twoPM[i]-out$cPM[i])/out$cPM[i]*100

out$multip_perInc84[i] = (out$onePM[i]-out$cPM[i])/out$cPM[i]*100
out$perInc86[i] = (out$twoPM[i]-out$cPM[i])/out$cPM[i]*100

}


plot(out$som,out$perInc84,xlab='Size at 50% Maturity',ylab='Percent Increase in Mature to 84mm',pch="")
text(x=out$som,y=out$perInc84,labels=toupper(out$LFA))
plot(out$som,out$perInc86,xlab='Size at 50% Maturity',ylab='Percent Increase in Mature to 86mm',pch="")
text(x=out$som,y=out$perInc86,labels=toupper(out$LFA))

### prob multiparous


