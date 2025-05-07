require(bio.lobster)
require(lubridate)
require(bio.utilities)
require(ggplot2)
require(dplyr)
require(readxl)


p = bio.lobster::load.environment()

la()

figdir = file.path(project.datadirectory("bio.data", "bio.lobster","requests","season.dates"))
dir.create( figdir, recursive = TRUE, showWarnings = FALSE )

x=read_xlsx(project.datadirectory( "bio.lobster","requests","season.dates", "lru", "lobsters.r.us.master.xlsx"))
t=read_xlsx(project.datadirectory( "bio.lobster","requests","season.dates", "lru", "lru.temps.xlsx"))

clean_numeric <- function(x) {
    as.numeric(gsub("[^0-9.]", "", trimws(x)))
}

# Apply the function to all columns in your data frame
x$dead <- clean_numeric(x$dead)
x$weak <- clean_numeric(x$weak)
x$soft <- clean_numeric(x$soft)
x$cull <- clean_numeric(x$cull)

t$temp.f <- clean_numeric(t$temp.f)
t$temp.c <- clean_numeric(t$temp.c)



# Remove the time part from 'date' (keep only the date)

x$date <- as.Date(x$date)
x$yr=year(x$date)
x$doy=yday(x$date)

t$date <- as.Date(t$date)

test=merge(x, t, all.x=T, all.Y=F)

x=subset(test, doy<205) #remove one Aug 1 entry for 2021 for consistency among years

saveRDS(x, file.path( "C:", "bio.data", "bio.lobster","requests","season.dates", "lru", "lobsters.r.us.master.rds"))

x=readRDS(project.datadirectory( "bio.lobster","requests","season.dates", "lru", "lobsters.r.us.master.rds"))


#Plot as panels for each variable with years as facets within the panels

library(tidyr)
x_long <- x %>%
    pivot_longer(cols = c(dead, weak, soft, cull, temp.c), 
                 names_to = "variable", 
                 values_to = "value")

# Create the trend line plot with facets and updated formatting
ggplot(x_long, aes(x = doy, y = value, color = factor(yr), group = yr)) +
    geom_smooth(method = "loess", se = FALSE) +  # LOESS smooth line, no confidence interval
    labs(x = "Day of Year (DOY)", y = "% of Landings", color = "Year") +  # Change y-axis title
    facet_wrap(~ variable, scales = "free_y") +  # Create a panel for each variable
    theme_minimal() +  # Optional: gives a clean theme
    theme(legend.position = "right",  # Optional: adjust legend position
          strip.text = element_text(face = "bold", size = 12, color = "black", hjust = 0.5))  # Bold and capitalize facet titles

#Updated with only dead and soft, tweaked some parameters
x_long <- x %>%
    pivot_longer(cols = c(dead, soft, temp.c),  # Remove 'weak' variable
                 names_to = "variable", 
                 values_to = "value") %>%
    mutate(
        # Capitalize the first letter of the 'variable' column (facet labels)
        variable = recode(variable, 
                          "dead" = "Dead", 
                          "soft" = "Soft",
                          "temp.c" = "Temp")
    )

# Create the trend line plot with facets stacked vertically, custom x-axis labels, and a title
lruplot= ggplot(x_long, aes(x = doy, y = value, color = factor(yr), group = yr)) +
    geom_smooth(method = "loess", se = FALSE) +  # LOESS smooth line, no confidence interval
    labs(
        y = "% of Landings", 
        color = "Year", 
        title = "LRU Lobster Data"  # Added title here
    ) +
    facet_wrap(~ variable, scales = "free_y", ncol = 1) +  # Stack facets in one column
    scale_x_continuous(
        breaks = c(121, 153, 183),  # Custom breaks for May (121), June (153), July (183)
        labels = c("May", "June", "July")  # Updated to "July" instead of "August"
    ) +
    theme_minimal() +  # Optional: gives a clean theme
    theme(
        legend.position = "right",  # Adjust legend position
        strip.text = element_text(
            face = "bold",  # Bold facet labels
            size = 13, 
            color = "black", 
            hjust = 0.5  # Move facet labels to the left
        ),
        axis.title.x = element_blank(),  # Removes the x-axis title
        plot.title = element_text(
            size = 16,  # Adjust title size
            #face = "bold",  # Bold title
            hjust = 0.1  # Center title
        )
    )

lruplot
