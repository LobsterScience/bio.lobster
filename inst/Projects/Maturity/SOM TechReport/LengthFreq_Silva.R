#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(tidyr)
require(statmod)
library(patchwork)
library(cowplot)


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

# Convert Pleopod_mat to character before subsetting
b$Pleopod_mat <- as.character(b$Pleopod_mat)
# Subset data
b31 = subset(b, LFA == '31')
b32 = subset(b, LFA == '32')


# Color palette
pal = c("#75D7E8", "#828289")

# Create individual plots without legends and y-axis labels
LF31 <- ggplot() +
  geom_histogram(data = b31, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 80) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 77.5, colour = "blue", lty = 3, lwd = 0.8) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  #scale_x_continuous(limits=c(45,175))+
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", label="LFA 31A", x=60,y=45,size = 4, col="black")

LF32 <- ggplot() +
  geom_histogram(data = b32, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 80) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 92.2, colour = "blue", lty = 3, lwd = 0.8) +
  xlab("Carapace Length (mm)") +
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  # scale_x_continuous(limits=c(45,175))+
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())+
  annotate("text", label="LFA 32", x=60,y=45,size = 4, col="black")


# Combine the plots without individual legends and y-axis labels
combined_plots <- plot_grid(LF31, LF32, ncol = 1)

# Create a dummy plot for the y-axis label
y_label <- ggplot() + 
  theme_void() + 
  theme(
    plot.margin = margin(r = 0.5, unit = "cm")
  ) + 
  annotate("text", x = 0.5, y = 0.5, label = "Count", angle = 90, size = 5)

# Combine the y-axis label and the combined plots
final_plot_with_y_label <- plot_grid(y_label, combined_plots, ncol = 2, rel_widths = c(0.05, 1))

# Extract the legend from LF31A
legend <- get_legend(
  LF32 + theme(legend.position = "right")
)

# Combine the final plot with the legend
final_plot <- plot_grid(final_plot_with_y_label, legend, ncol = 2, rel_widths = c(1, 0.2))

# Display the final plot
print(final_plot)













# Subset data
b33 = subset(b, LFA == '33')
b34 = subset(b, LFA == '34')


# Color palette
pal = c("#75D7E8", "#828289")

# Create individual plots without legends and y-axis labels
LF33 <- ggplot() +
  geom_histogram(data = b33, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 80) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  annotate("text", label="LFA 33", x=60,y=75,size = 4, col="black")

LF34 <- ggplot() +
  geom_histogram(data = b34, aes(x = Carapace_mm, fill = as.character(Pleopod_mat), group = Pleopod_mat), alpha = 0.6, position = 'identity', bins =100) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  xlab("Carapace Length (mm)") +
  scale_y_continuous(limits = c(0,110), expand = c(0, 0)) +
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank())+
  annotate("text", label="LFA 34", x=60,y=100,size = 4, col="black")


# Combine the plots without individual legends and y-axis labels
combined_plots <- plot_grid(LF33, LF34, ncol = 1)

# Create a dummy plot for the y-axis label
y_label <- ggplot() + 
  theme_void() + 
  theme(
    plot.margin = margin(r = 0.5, unit = "cm")
  ) + 
  annotate("text", x = 0.5, y = 0.5, label = "Count", angle = 90, size = 5)

# Combine the y-axis label and the combined plots
final_plot_with_y_label <- plot_grid(y_label, combined_plots, ncol = 2, rel_widths = c(0.05, 1))

# Extract the legend from LF31A
legend <- get_legend(
  LF34 + theme(legend.position = "right")
)

# Combine the final plot with the legend
final_plot <- plot_grid(final_plot_with_y_label, legend, ncol = 2, rel_widths = c(1, 0.2))

# Display the final plot
print(final_plot)
