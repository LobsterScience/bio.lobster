#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(tidyr)
require(statmod)
require(cowplot)
require(dplyr)

p = bio.lobster::load.environment()
la()
##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','matClean.csv'))
##########-----------------------------------------------##########
b=a

#b = subset(a,year=='2023')
b = subset(b,LFA=='35')
b=subset(b, Sex=2) ##Don't include berried females

b= subset(b, Carapace_mm <120)
g = glm(Pleopod_mat~Carapace_mm,data=b,family=binomial(link='logit')) ##CHECK what Months you're sampling

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length", ylab = "Proportion Mature")
l = seq(min(b$Carapace_mm),max(b$Carapace_mm),by=.1)
mm =predict(g,list(Carapace_mm=l),type='response')
lines(l,mm)
l[which.min(abs(mm-.5))]

#correct CI's
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata$Carapace_mm, ndata$fit_resp)
lines(ndata$Carapace_mm, ndata$upr, lty=2) 
lines(ndata$Carapace_mm, ndata$lwr,lty=2)

with(ndata,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	

with(summary(g), 1 - deviance/null.deviance) #McFaddens R-squared

text(75, 0.8,bquote(paste('L'['50']*' = ','83.2')))
text(75, 0.8,bquote(paste('L'['50']*' = ','90.3')))


### LOOK at Residuals
b$CL_residuals<-qresid(g)

ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(LFA)))+
  geom_point(size=3)

ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(year)))+
  geom_point(size=3)


ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(mon)))+
  geom_point(size=3)

ggplot(b, aes(x=Carapace_mm, y = CL_residuals, color = as.factor(Shell_hardness)))+
  geom_point(size=3)

##FIGURE OUT DISTANCE FROM COAST

## add Column if " Overall " Ovarian Maturity - all 3 criteria must be met.

## Boxplot of Ovary status
#X axis is Ovary maturity stage
#Confusion Matrix with the ovary samples

#################### Together plots ###############
library(ggplot2)
library(patchwork)
library(cowplot)

# Subset data
b33 = subset(b, LFA == '33')
b35 = subset(b, LFA == '35')
b36 = subset(b, LFA == '36')
b38 = subset(b, LFA == '38')

# Color palette
pal = c("#75D7E8", "#828289")

# Create individual plots without legends and y-axis labels
LF33 <- ggplot() +
  geom_histogram(data = b33, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 70) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 90.3, colour = "blue", lty = 3, lwd = 0.8) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  #scale_x_continuous(limits=c(45,175))+
  scale_fill_manual(values = pal, labels = c("Immature", "Mature")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", label="LFA 33", x=50,y=45,size = 4, col="black")

LF35 <- ggplot() +
  geom_histogram(data = b35, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 70) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 88.8, colour = "blue", lty = 3, lwd = 0.8) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  # scale_x_continuous(limits=c(45,175))+
  scale_fill_manual(values = pal, labels = c("Immature", "Mature")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", label="LFA 35", x=40,y=90,size = 4, col="black")

LF36 <- ggplot() +
  geom_histogram(data = b36, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 70) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 88.8, colour = "blue", lty = 3, lwd = 0.8) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  # scale_x_continuous(limits=c(40,170))+
  scale_fill_manual(values = pal, labels = c("Immature", "Mature")) +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", label="LFA 36", x=53,y=90,size = 4, col="black")

LF38 <- ggplot() +
  geom_histogram(data = b38, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 70) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 88.5, colour = "blue", lty = 3, lwd = 0.8) +
  xlab("Carapace Length (mm)") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  #scale_x_continuous(limits=c(40,170))+
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())+
  annotate("text", label="LFA 38", x=52,y=90,size = 4, col="black")

# Combine the plots without individual legends and y-axis labels
combined_plots <- plot_grid(LF33, LF35, LF36, LF38, ncol = 1)

# Create a dummy plot for the y-axis label
y_label <- ggplot() + 
  theme_void() + 
  theme(
    plot.margin = margin(r = 0.5, unit = "cm")
  ) + 
  annotate("text", x = 0.5, y = 0.5, label = "Count", angle = 90, size = 5)

# Combine the y-axis label and the combined plots
final_plot_with_y_label <- plot_grid(y_label, combined_plots, ncol = 2, rel_widths = c(0.05, 1))

# Extract the legend from LF38
legend <- get_legend(
  LF38 + theme(legend.position = "right")
)

# Combine the final plot with the legend
final_plot <- plot_grid(final_plot_with_y_label, legend, ncol = 2, rel_widths = c(1, 0.2))

# Display the final plot
print(final_plot)






###################Confusion Matrix
library(caret)

OvSamp= subset(b,Ovary_sampled =='1')
OvSamp=subset(OvSamp, Ovary_colour != 'n/a') 
OvSamp=subset(OvSamp, Ovary_factor != 'n/a') 


expected_value<-OvSamp$OvaryMaturity
predicted_value<-OvSamp$Pleopod_mat
xtab<- table(predicted_value, expected_value)

testMatrix<-as.matrix(xtab)
conMat<- caret::confusionMatrix(as.factor(predicted_value),as.factor(expected_value))



g = glm(OvaryMaturity~Carapace_mm,data=OvSamp,family=binomial(link='logit')) ##CHECK what Months you're sampling

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length", ylab = "Proportion Mature")
l = seq(min(b$Carapace_mm),max(b$Carapace_mm),by=.1)
mm =predict(g,list(Carapace_mm=l),type='response')
lines(l,mm)
l[which.min(abs(mm-.5))]

#correct CI's
ndata <- list(Carapace_mm=l)
ndata = glmCIs(g,ndata)

plot(b$Carapace_mm, b$Pleopod_mat, pch = 16, xlab = "Carapace Length (mm)", ylab = "Proportion Mature")
lines(ndata$Carapace_mm, ndata$fit_resp)
lines(ndata$Carapace_mm, ndata$upr, lty=2) 
lines(ndata$Carapace_mm, ndata$lwr,lty=2)

with(ndata,{
  print(Carapace_mm[which.min(abs(fit_resp-.5))])
  print(Carapace_mm[which.min(abs(upr-.5))])
  print(Carapace_mm[which.min(abs(lwr-.5))])
})	


text(60, 0.8,bquote(paste('L'['50']*' = ','99.8')))







############ Mapping Samples ############
#### Colour samples by year as a factor #####

require(sf)
#2022 + 33
L33_22<-b[b$LFA== 33 & b$year ==2022, ]
L33_22 = st_as_sf(L33_22,coords=c('X','Y'),crs=4326)
#2023 + 36
L36_23<-b[b$LFA== 36 & b$year ==2023, ]
L36_23 = st_as_sf(L36_23,coords=c('X','Y'),crs=4326)
#2022 + 36
L36_22<-b[b$LFA== 36 & b$year ==2022, ]
L36_22 = st_as_sf(L36_22,coords=c('X','Y'),crs=4326)
#2023 + 38 
L38_23<-b[b$LFA== 38 & b$year ==2023, ]
L38_23 = st_as_sf(L38_23,coords=c('X','Y'),crs=4326)
#2022 + 38
L38_22<-b[b$LFA== 38 & b$year ==2022, ]
L38_22 = st_as_sf(L38_22,coords=c('X','Y'),crs=4326)


b1 = st_as_sf(L33_22,coords=c('X','Y'),crs=4326)
g<-ggLobsterMap( ylim=c(42.5,44.8),xlim=c(-65.8,-62.2))
g<-g+geom_sf(data=b1)
g
#g<-g +geom_sf(data=bsubset of each dataframe) ## add each layer with own aes()





#### Make a convex hull around each subset of data by trip --- Don't know what this step is for?
set.seed(42)
points <- st_sfc(st_point(cbind(runif(10), runif(10))))
point_data <- st_sf(geometry = points)

# Create a convex hull around the point data
convex_hull <- st_convex_hull(st_union(L33_22))

# Plot the points and the convex hull
ggplot() +
  geom_sf(data =L33_22, color = "red") +
  geom_sf(data = convex_hull, fill = "transparent", color = "blue") +
  labs(title = "Convex Hull around Point Data")

### add the buffer ---Buffer doesn't work with lat lOng?
buffer_distance <- 0.1  # Set your desired buffer distance
buffered_convex_hull <- st_buffer(convex_hull, dist = buffer_distance)

# Plot the points, convex hull, and buffered convex hull
ggplot() +
  geom_sf(data = point_data, color = "red") +
  geom_sf(data = convex_hull, fill = "transparent", color = "blue") +
  geom_sf(data = buffered_convex_hull, fill = "transparent", color = "green") +
  labs(title = "Buffered Convex Hull around Point Data")





