
logistic_growth <- function(t, P0 = 1, r = 0.1, K = 100) {
  return(K / (1 + ((K - P0) / P0) * exp(-r * t)))
}

# Generate a sequence of time points (t) from 0 to 100
time_points <- seq(0, 100, by = 0.1)

# Calculate the corresponding population values (P(t)) using the logistic growth function
population_values <- logistic_growth(time_points)

# Plot the logistic growth curve

xx = data.frame(x=c(0,0),xend=c(time_points[which.min(abs(population_values-50))],80),y=c(50,100),yend=c(50,100))
ggplot(data.frame(t = time_points, P = population_values), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Logistic Population Growth", x = "Time", y = "Biomass") +
#  geom_hline(yintercept = c(100),colour='blue',linewidth=1.3)+
  geom_segment(data=xx,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
    annotate('text', y=98,x=10, label='K',size=6)+
  annotate('text', y=55,x=10, label=expression(B[MSY] == frac(K,2)),size=6)+
  scale_x_continuous(limits=c(0,100))
  

bmsy = population_values[which.min(abs(population_values-50))]
ymax = max(diff(population_values))
xy = data.frame(x=c(bmsy,0),xend=c(bmsy,bmsy),y=c(0,ymax),yend=c(ymax,ymax))
ggplot(data.frame(t = population_values[-1], P = c(diff(population_values))), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Yield Curve", x = "Biomass", y = "Yield") +
#  geom_segment(data=xy,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
#  annotate('text', y=.5*xy$yend[1],x=xy$x[1]*.8, label=expression(B[MSY] == frac(K,2)),size=6)+
#  annotate('text', y=xy$yend[2]*.95,x=xy$x[1]*.35, label=expression(MSY == frac(r*K,4)),size=6)+
  scale_x_continuous(limits=c(0,100))


bmsy = population_values[which.min(abs(population_values-50))]
ydiff = (diff(population_values))
ymax = max(ydiff)
lrp = bmsy*.4
ylrp = ydiff[which.min(abs(population_values-lrp))]
xy = data.frame(x=c(bmsy,0,lrp),xend=c(bmsy,bmsy,lrp),y=c(0,ymax,0),yend=c(ymax,ymax,ylrp))
ggplot(data.frame(t = population_values[-1], P = c(diff(population_values))), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Yield Curve", x = "Biomass", y = "Yield") +
  geom_segment(data=xy,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
  annotate('text', y=.5*xy$yend[1],x=xy$x[1]*.8, label=expression(B[MSY] == frac(K,2)),size=6)+
  annotate('text', y=xy$yend[2]*.95,x=xy$x[1]*.35, label=expression(MSY == frac(r*K,4)),size=6)+
  annotate('text', y=xy$yend[3]*.01,x=xy$x[3]*.45, label=expression(LRP == 0.40*B[MSY]),size=6)+
  
  scale_x_continuous(limits=c(0,100))





# Plot the logistic growth curve

xx = data.frame(x=c(0,0),xend=c(time_points[which.min(abs(population_values-50))],80),y=c(50,100),yend=c(50,100))
pv = population_values
pv[1:550] <- NA
ggplot(data.frame(t = time_points, P = pv), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Logistic Population Growth", x = "Time", y = "Biomass") +
  #  geom_hline(yintercept = c(100),colour='blue',linewidth=1.3)+
  geom_segment(data=xx,aes(x=x,xend=xend[2],y=y[2],yend=yend[2]),arrow=arrow())+
  annotate('text', y=98,x=10, label='K',size=6)+
 # annotate('text', y=55,x=10, label=expression(B[MSY] == frac(K,2)),size=6)+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(1,100))


bmsy = pv[which.min(abs(pv-50))]
ymax = max(diff(pv))
xy = data.frame(x=c(bmsy,0),xend=c(bmsy,bmsy),y=c(0,ymax),yend=c(ymax,ymax))
ggplot(data.frame(t = pv[-1], P = c(diff(pv))), aes(x = t, y = P)) +
  geom_line(color = "black", size = 1.2) +
  theme_test(base_size = 14)+theme(axis.text.x=element_blank(), axis.text.y=element_blank())+
  labs(title = "Yield Curve", x = "Biomass", y = "Yield") +
  geom_segment(data=xy,aes(x=x,xend=xend,y=y,yend=yend),arrow=arrow())+
  annotate('text', y=.5*xy$yend[1],x=xy$x[1]*.8, label=expression(B[MSY] == frac(K,2)),size=6)+
  annotate('text', y=xy$yend[2]*.95,x=xy$x[1]*.35, label=expression(MSY == frac(r*K,4)),size=6)+
  scale_x_continuous(limits=c(0,100))
