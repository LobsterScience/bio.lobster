library(ggplot2)

# Parameters
time <- seq(0, 30, by = 0.1)
K <- 100

# Logistic growth function
logistic <- function(t, N0, r, K) {
  K / (1 + ((K - N0) / N0) * exp(-r * t))
}

# Species trajectories
df <- rbind(
  data.frame(
    Time = time,
    Biomass = logistic(time, N0 = 2, r = 0.5, K = K),
    Species = "r-selected"
  ),
  data.frame(
    Time = time,
    Biomass = logistic(time, N0 = 2, r = 0.15, K = K),
    Species = "K-selected"
  )
)

ggplot(df, aes(Time, Biomass, colour = Species)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = K, linetype = 2) +
  annotate("text", x = 25, y = K + 3,
           label = "Carrying Capacity (K)") +
  scale_colour_manual(
    values = c(
      "r-selected" = "#1B9E77",
      "K-selected" = "#D95F02"
    )
  ) +
  labs(
    x = "Time",
    y = "Population Size / Biomass",
    colour = NULL
  ) +
  theme_bw(base_size = 14)

