# ---- Packages ----
# install.packages(c("ggplot2", "ggimage", "dplyr"))  # uncomment if needed
library(ggplot2)
library(ggimage)   # lets us place images (incl. GIFs) in ggplot
library(dplyr)
library(tibble)

# ---- Path to your lobster GIF(s) ----
# lob_gif <- "C:/Users/cooka/Pictures/005 Lobster early bottom phase.jpg"   # <- change this to your file
# library(magick)
# img_small <- image_read(lob_gif) |>
#   image_resize("300x")  # shrink the GIF before plotting
# image_write(img_small, "git/bio.lobster/inst/StockRecruit/lobster_Eb__small.jpg")
# lob_gif2 <- "C:/Users/cooka/Pictures/006 Lobster Adult.jpg"   # <- change this to your file
# img_small2 <- image_read(lob_gif2) |>
#   image_resize("300x")  # shrink the GIF before plotting
# image_write(img_small2, "git/bio.lobster/inst/StockRecruit/lobster_ad__small.jpg")

lob_recruit= "git/bio.lobster/inst/StockRecruit/lobster_Eb__small.jpg"
lob_parent  = "git/bio.lobster/inst/StockRecruit/lobster_ad__small.jpg"

# --- Stock–recruit curves on comparable scale ---
a <- 100      # BH asymptote
b <- 150
beta  <- 0.010
alpha <- a * beta * exp(1)   # Ricker peak == a
m_bh = 0.35

SSB_peak <- 1 / beta
m_ricker <- alpha * exp(-beta * SSB_peak)


SSB_eq <- 100
m_bh <- a / (b + SSB_eq)


ssb <- seq(0, 300, length.out = 400)
R_bh     <- a * ssb / (b + ssb)
R_ricker <- alpha * ssb * exp(-beta * ssb)

curve_df <- tibble(
  SSB = ssb,
  `Beverton–Holt` = R_bh,
  `Ricker`        = R_ricker
) |>
  tidyr::pivot_longer(-SSB, names_to = "Curve", values_to = "Recruitment")

R_max <- max(curve_df$Recruitment)

# optional references
B1 <- 50; B2 <- 100
R1 <- a * B1 / (b + B1)
R2 <- a * B2 / (b + B2)

# --- recruits on BOTH sides of the y-axis (left side will appear outside axes) ---
set.seed(42)
n_swarm <- 60
recr_lobsters <- bind_rows(
  tibble(x = runif(n_swarm/2, -60, -10),  # left of y-axis
         y = runif(n_swarm/2, R_max*0.75, R_max*1.02),
         image = lob_recruit),
  tibble(x = runif(n_swarm/2,  10,  60),  # right of y-axis
         y = runif(n_swarm/2, R_max*0.55, R_max*1.02),
         image = lob_recruit)
) |>
  dplyr::mutate(
    angle = runif(dplyr::n(), -35, 35)   # random rotation in degrees
  )


# --- adults BELOW the x-axis (will render outside panel) + SSB label ---
adult_weights <- c(1.2, 1.6)  # kg
SSB_adults    <- sum(adult_weights)

parents <- tibble(
  x     = c(max(ssb)*0.92, max(ssb)*1.10),
  y     = c(-0.04*R_max,   -0.10*R_max),   # negative y -> below axis
  image = c(lob_parent, lob_parent),
  size  = c(0.16, 0.13)
)|>
  dplyr::mutate(
    angle = runif(dplyr::n(), -35, 35)   # random rotation in degrees
  )


annot_x <- mean(parents$x)
annot_y <- min(parents$y) - 0.06*R_max

# --- plot ---
p <- ggplot() +
  # recruits
  geom_image(data = recr_lobsters, aes(x, y, image = image,angle=angle),
             size = 0.05, by = "width", asp = 1, inherit.aes = FALSE) +
  # curves
  geom_line(data = curve_df, aes(SSB, Recruitment, color = Curve), linewidth = 1.2) +
  
  geom_abline(intercept = 0, slope = m_bh,
              linetype = "dashed", color = "grey40", linewidth = 0.9,
              show.legend = FALSE)+

  geom_abline(intercept = 0, slope = m_ricker,
              linetype = "dashed", color = "grey40", linewidth = 0.9,
              show.legend = FALSE)+
  
  # optional guides
  geom_vline(xintercept = c(B1, B2), linetype = "dotted", color = "grey55") +
  geom_hline(yintercept = c(R1, R2), linetype = "dotted", color = "grey55") +
  # axes at origin
  geom_hline(yintercept = 0, linewidth = 1.1, color = "black") +
  geom_vline(xintercept = 0, linewidth = 1.1, color = "black") +
  # adults below x-axis
  geom_image(data = parents, aes(x, y, image = image,angle=angle),
             size = parents$size, asp = 1, inherit.aes = FALSE) +
  # ---- IMPORTANT: axes start at 0, no extension below 0 ----
# Use coord_cartesian to zoom to 0..max while still allowing drawings outside the panel
coord_cartesian(
  xlim = c(0, max(ssb) * 1.30),
  ylim = c(0, R_max * 1.05),
  clip = "off"
) +
  # keep only non-negative tick marks
  scale_x_continuous(
    breaks = function(z) pretty(z[z >= 0]),
    expand = expansion(mult = c(0.00, 0.02))
  ) +
  scale_y_continuous(
    breaks = function(z) pretty(z[z >= 0]),
    expand = expansion(mult = c(0.00, 0.06))
  ) +
  scale_color_manual(
    values = c("Beverton–Holt" = "#1b4f72", "Ricker" = "#d95f02")
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.margin       = margin(20, 40, 25, 40),
    axis.title.x      = element_text(margin = margin(t = 8)),
    axis.title.y      = element_text(margin = margin(r = 8)),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.line         = element_blank(),
    legend.position   = c(0.98, 0.98),        # top-right
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA)
  ) +
  labs(x = "SSB", y = "Recruitment", color = "Stock–Recruit curve")+
  
  annotate("label", x = -00, y = R_max * 1.02,
           label = 'RECRUITMENT\n  Number of "Offspring"',
           fill = "white", color = "grey20", size = 3.7)+
  
  annotate("label", x = 300, y = 10,
           label = 'SPAWNING STOCK\n Weight of "Parents"',
            fill = "white", color = "grey20", size = 3.7)



p


