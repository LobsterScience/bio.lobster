
#pella tomlinson
logistic_growth_pt <- function(t, P0 = 1, r = 0.1, K = 100,p) {
  b = rep(NA,length(t))
  b[1]=P0
  for(i in 2:length(t)){
      b[i] = b[i-1]+r*b[i-1]/p*((1-(b[i-1]/K)^p))
  }
  return(b)
}


# Generate a sequence of time points (t) from 0 to 100
time_points <- seq(0, 50, by = 0.1)

# Calculate the corresponding population values (P(t)) using the logistic growth function
population_values <- logistic_growth_pt(time_points,p=5)
population_values2 <- logistic_growth_pt(time_points,p=.000001)
population_values3 <- logistic_growth_pt(time_points,p=1)

pv <- diff(population_values)
pv2 <- diff(population_values2)
pv3 <- diff(population_values3)


plot(population_values[-1],pv/max(pv))
lines(population_values2[-1],pv2/max(pv2))
lines(population_values3[-1],pv3/max(pv3))

bmsy = population_values[which(pv==max(pv))]
bmsy2 =population_values2[which(pv2==max(pv2))]
bmsy3 =population_values3[which(pv3==max(pv3))]


# Plot the logistic growth curve
pvmax = time_points[which.max(pv)]

xx = data.frame(x=c(0,0),xend=c(pvmax,25),y=c(bmsy,100),yend=c(bmsy,100))
ggplot(data.frame(t = time_points, P = population_values), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Logistic Population Growth", x = "Time", y = "Biomass") +
#  geom_hline(yintercept = c(100),colour='blue',linewidth=1.3)+
  geom_segment(data=xx,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
    annotate('text', y=98,x=10, label='K',size=6)+
  annotate('text', y=65,x=10, label=expression(B[MSY]),size=6)+
  scale_x_continuous(limits=c(0,50))+
  scale_y_continuous(limits=c(0,100))
  
ymax = max(pv)
lrp = bmsy*.4
ylrp = pv[which.min(abs(population_values-lrp))]

xy = data.frame(x=c(bmsy,0,lrp),xend=c(bmsy,bmsy,lrp),y=c(0,ymax,0),yend=c(ymax,ymax,ylrp))
ggplot(data.frame(t = population_values[-1], P = pv), aes(x = t, y = P)) +#
  geom_line(color = "black", size = 1.2) +
  #theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Yield Curve", x = "Biomass", y = "Yield") +
  geom_segment(data=xy,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
  annotate('text', y=.5*xy$yend[1],x=xy$x[1]*.8, label=expression(B[MSY]),size=6)+
  annotate('text', y=xy$yend[2]*.95,x=xy$x[1]*.35, label=expression(MSY),size=6)+
  annotate('text', y=xy$yend[3]*.01,x=xy$x[3]*.45, label=expression(LRP == 0.40*B[MSY]),size=6)+
  
    scale_x_continuous(limits=c(0,100))

#############
# Plot the logistic growth curve
pvmax2 = time_points[which.max(pv2)]

xx = data.frame(x=c(0,0),xend=c(pvmax2,10),y=c(bmsy2,100),yend=c(bmsy2,100))
ggplot(data.frame(t = time_points, P = population_values2), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Logistic Population Growth", x = "Time", y = "Biomass") +
  #  geom_hline(yintercept = c(100),colour='blue',linewidth=1.3)+
  geom_segment(data=xx,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
  annotate('text', y=98,x=4, label='K',size=6)+
  annotate('text', y=30,x=.4, label=expression(B[MSY]),size=6)+
  scale_x_continuous(limits=c(0,20))+
  scale_y_continuous(limits=c(0,100))

ymax = max(pv2)
lrp = bmsy2*.4
ylrp = pv2[which.min(abs(population_values2-lrp))]

xy = data.frame(x=c(bmsy2,0,lrp),xend=c(bmsy2,bmsy2,lrp),y=c(0,ymax,0),yend=c(ymax,ymax,ylrp*.95))
ggplot(data.frame(t = population_values2[-1], P = pv2), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Yield Curve", x = "Biomass", y = "Yield") +
  geom_segment(data=xy,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
  annotate('text', y=.5*xy$yend[1],x=xy$x[1]*.8, label=expression(B[MSY]),size=6)+
  annotate('text', y=xy$yend[2]*.95,x=xy$x[1]*.35, label=expression(MSY),size=6)+
  annotate('text', y=xy$yend[3]*.01,x=xy$x[3]*.45, label=expression(LRP == 0.40*B[MSY]),size=6)+
  
  scale_x_continuous(limits=c(0,100))

