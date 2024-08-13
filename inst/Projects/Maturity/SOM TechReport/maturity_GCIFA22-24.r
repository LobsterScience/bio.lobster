#################### Together plots ###############
#maturity modelling
options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)
require(ggplot2)
require(statmod)
library(patchwork)
library(cowplot)
library(dplyr)

##########------------------ Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','Maturity_GCIFA.csv'))
##########-----------------------------------------------##########
b=a

#### Clean and convert ####
b$Date = as.Date(b$Date,"%d-%b-%y")
b$mon = month(b$Date)
b$year = year(b$Date)
b$EID = 1:nrow(b)



convert_to_decimal_degrees <- function(dmm) {
  dmm <- trimws(dmm)  # Trim whitespace
  parts <- strsplit(dmm, " ")[[1]]  # Split by space
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  decimal_degrees <- degrees + (minutes / 60)
  return(decimal_degrees)
}


b$Latitude_DD <- sapply(b$Latitude, convert_to_decimal_degrees)
b$Longitude_DD <- sapply(b$Longitude, convert_to_decimal_degrees)

b <- b %>%
  rename(Y = Latitude_DD, X = Longitude_DD)


b$X[is.na(b$Y)] <- b$Latitude[is.na(b$Y)]
b$Y[is.na(b$X)] <- b$Longitude[is.na(b$X)]
b$X = b$X*-1


##### Map Samples

LobsterMap("27-32.Crop")
addPoints(na.omit(b[,c('X','Y','EID')]),col='red')


# Convert Pleopod_mat to character before subsetting
b$Pleopod_mat <- as.character(b$Pleopod_mat)

# Subset data
b31A <- subset(b, LFA == '31A')
b31B <- subset(b, LFA == '31B')

# Color palette
pal <- c("#75D7E8", "#828289")

# Create individual plots without legends and y-axis labels
LF31A <- ggplot() +
  geom_histogram(data = b31A, aes(x = Carapace_mm, fill = Pleopod_mat, group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 80) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 74.4, colour = "blue", lty = 3, lwd = 0.8) +
  xlab("Carapace Length (mm)") +
   scale_y_continuous(limits = c(0, 70), expand = c(0, 0)) +
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  annotate("text", label = "LFA 31A", x = 56, y = 65, size = 4, col = "black")

LF31B <- ggplot() +
  geom_histogram(data = b31B, aes(x = Carapace_mm, fill = Pleopod_mat, group = Pleopod_mat), alpha = 0.6, position = 'identity', bins = 70) +
  geom_vline(xintercept = 82.5, colour = "#F02C2C", lty = 2, lwd = 0.8) +
  geom_vline(xintercept = 77.6, colour = "blue", lty = 3, lwd = 0.8) +
  xlab("Carapace Length (mm)") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  scale_fill_manual(values = pal, labels = c("Immature", "Mature"), name = "Maturity Status") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank()) +
  annotate("text", label = "LFA 31B", x =50, y = 45, size = 4, col = "black")

# Combine the plots without individual legends and y-axis labels
combined_plots <- plot_grid(LF31A, LF31B, ncol = 1)

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
  LF31A + theme(legend.position = "right")
)

# Combine the final plot with the legend
final_plot <- plot_grid(final_plot_with_y_label, legend, ncol = 2, rel_widths = c(1, 0.2))

# Display the final plot
print(final_plot)


