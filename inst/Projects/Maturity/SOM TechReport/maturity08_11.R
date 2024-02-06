#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(tidyr)
require(statmod)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','LobsterMaturityDatabase.csv'))
##########-----------------------------------------------##########

a$Y = a$Lat
a$X = a$Long*-1
a$EID = 1:nrow(a)
a$Date = as.Date(a$Date,"%d/%m/%Y")
a$mon = month(a$Date)
a$year = year(a$Date)

#LobsterMap('all')
#addPoints(na.omit(a[,c('X','Y','EID')]),col='red')


a=subset(a, Sex == 2) #Remove Berried Females
a=subset(a, Cemgland_stage != 'n/a') 
a=subset(a, mon == 5 | mon == 6)#Filter out July and August

#Pleopod Staging
a$Pleopod_mat <-ifelse(a$Cemgland_stage<2,0,1)

## Add LFAs
a$LFA <- ""
a$LFA <-ifelse(a$Location == "Lobster Bay", 34, a$LFA)
a$LFA <-ifelse(a$Location == "Harrigan Cove" | a$Location == "Tangier"| a$Location == "Mushaboom", 32, a$LFA)
a$LFA <-ifelse(a$Location == "Port Mouton", 33,a$LFA)
a$LFA <-ifelse(a$Location == "Canso", 31, a$LFA)
a = subset(a,Carapace_mm <150)


b=a
#b = subset(a,LFA == 33)


b = subset(b,Carapace_mm<120)
g = glm(Pleopod_mat~Carapace_mm,data=b,family=binomial(link='logit'))

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length", ylab = "Maturity")
l = seq(min(b$Carapace_mm),max(b$Carapace_mm),by=.1)
mm =predict(g,list(Carapace_mm=l),type='response')
lines(l,mm)
l[which.min(abs(mm-.5))]



#correct CI's
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Maturity")
lines(ndata$Carapace_mm, ndata$fit_resp)
lines(ndata$Carapace_mm, ndata$upr, lty=2) 
lines(ndata$Carapace_mm, ndata$lwr,lty=2)

with(ndata,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

text(55, 0.8,bquote(paste('L'['50']*' = ','97.6')))



m1 = formula(p~1/(1+(exp(a+(b*cl)))))
nls(m1,data=ndata,start=list(a=10,b=-.1))

########### TO LOOK at Residuals ##################
b$CL_residuals<-qresid(g)

ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(LFA)))+
  geom_point(size=3)

ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(year)))+
  geom_point(size=3)


ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(mon)))+
  geom_point(size=3)

ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(Shell_hardness)))+
  geom_point(size=3)








b31 = subset(b,LFA=='31')
pal =c("#75D7E8","#828289")
LF31a<-ggplot()+
  geom_histogram(data=b31,aes(x=Carapace_mm, fill=as.character(Pleopod_mat), group=Pleopod_mat), alpha=0.6,position = 'identity', bins=50)+
  geom_vline(xintercept=82.5, colour="#F02C2C", lty=2)+
  geom_vline(xintercept=77.5, colour="blue", lty=3,lwd=0.8)+
  xlab("Carapace Length (mm)")+
  ylab("Count")+
  scale_y_continuous(limits=c(0,90), expand = c(0, 0)) +
  #scale_x_continuous(limits=c(40,160), expand = c(0, 0))+
  scale_fill_manual(values=pal, labels = c("Immature", "Mature"), aes(fill ="Maturity Status"))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text=element_text(size=15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
LF31a<-LF31a+  geom_text()+ annotate("text", label="LFA 31A", x=55,y=78,size = 4, col="black")


b32 = subset(b,LFA=='32')
pal =c("#75D7E8","#828289")
LF32<-ggplot()+
  geom_histogram(data=b32,aes(x=Carapace_mm, fill=as.character(Pleopod_mat), group=Pleopod_mat), alpha=0.6,position = 'identity',bins=50)+
  geom_vline(xintercept=82.5, colour="#F02C2C", lty=2)+
  geom_vline(xintercept=92.2, colour="blue", lty=3,lwd=0.8)+
  xlab("Carapace Length (mm)")+
  ylab("Count")+
  scale_y_continuous(limits=c(0,60), expand = c(0, 0)) +
  # scale_x_continuous(limits=c(0,170),expand = c(0, 0))+
  scale_fill_manual(values=pal, labels = c("Immature", "Mature"), aes(fill ="Maturity Status"))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text=element_text(size=15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
LF32<-LF32+  geom_text()+ annotate("text", label="LFA 32", x=55,y=58,size = 4, col="black")

b33= subset(b,LFA=='33')
pal =c("#75D7E8","#828289")
LF33<-ggplot()+
  geom_histogram(data=b33,aes(x=Carapace_mm, fill=as.character(Pleopod_mat), group=Pleopod_mat), alpha=0.6,position = 'identity',bins=50)+
  geom_vline(xintercept=82.5, colour="#F02C2C", lty=2)+
  geom_vline(xintercept=97.6, colour="blue", lty=3,lwd=0.8)+
  xlab("Carapace Length (mm)")+
  ylab("Count")+
  scale_y_continuous(limits=c(0,80), expand = c(0, 0)) +
  # scale_x_continuous(limits=c(0,170),expand = c(0, 0))+
  scale_fill_manual(values=pal, labels = c("Immature", "Mature"), aes(fill ="Maturity Status"))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text=element_text(size=15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
LF33<-LF33+  geom_text()+ annotate("text", label="LFA 33", x=58,y=78,size = 4, col="black")


b34= subset(b,LFA=='34')
pal =c("#75D7E8","#828289")
LF34<-ggplot()+
  geom_histogram(data=b34,aes(x=Carapace_mm, fill=as.character(Pleopod_mat), group=Pleopod_mat), alpha=0.6,position = 'identity',bins=60)+
  geom_vline(xintercept=82.5, colour="#F02C2C", lty=2)+
  geom_vline(xintercept=102, colour="blue", lty=3,lwd=0.8)+
  xlab("Carapace Length (mm)")+
  ylab("Count")+
  scale_y_continuous(limits=c(0,200), expand = c(0, 0)) +
  # scale_x_continuous(limits=c(0,170),expand = c(0, 0))+
  scale_fill_manual(values=pal, labels = c("Immature", "Mature"), aes(fill ="Maturity Status"))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text=element_text(size=15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
LF34<-LF34+  geom_text()+ annotate("text", label="LFA 34", x=58,y=190,size = 4, col="black")

