library(dplyr)
library(ggplot2)
library(ggrepel)

#landings table from git/bio.lobster/inst/FisherySummaryPlots/MaritimesLandingsFullTS.r
dat = daa

early.yrs <- as.data.frame(1860:1891)
colnames(early.yrs) = "YR"
early.yrs$LAND = 0

### early events before landings data up to 1891
early.events <- data.frame(
  YR = c(1867,
         1873,
         1874,
         1876,
         1877),
  event = c("Lobster fishing under federal jurisdiction",
            "First restrictions on soft-shell and berried",
            "First MLS: 9 inches. Soft-shell restriction replaced with closed season July-Aug",
            "Closed season changed to Aug 10 - Sept 20",
            "Sectional closed seasons introduced")
)




vec <- c(
  1889, "MLS increased to 9.5 inches",
  1891, "MLS returned to 9 inches",
  1900, "Prohibition of gear in <2 fa. Gas motors introduced",
  1914, "All non-trap gear prohibited",
  1918, "Licenses introduced",
  #1930, "Department of Fisheries Created",
  1933, "License holders restricted to one district",
  #1940, "Lobster pounds must liberate berried females",
  1945, "Same boat, gear restricted to one district in a year",
  1950, "First record of V-notching for science",
  #1958, "Unemployment insurance expanded to lobster off-season",
  #1966, "Trap limits tested in PEI",
  1967, "Number of licenses capped",
  1968, "Trap limits introduced across Maritimes",
  1969, "Category A and B Licenses introduced",
  1970, "Fishers restricted to one license each",
  1971, "Offshore fishery opens",
  1978, "Licence buy back",
  #1979, "DFO Created",
  1980, "Start of Advisory Committees",
  1981, "Licenses become non-transferable",
  #1984, "Sunday Fishing Allowed",
  1985, "Atlantic Fisheries Regulations",
  1989, "Owner-operator policy",
  1991, "Escape vents",
  1993, "Communal licenses",
  1997, "FRCC Report - Doubling Egg Per Recruit",
  1998, "New HCRs Most LFAs, Inc MLS, Wind., Notch, Max",
  1999, "Marshall Decision",
  2002, "Grey Zone fishing permitted",
  2012, "Reference points established"

)

events <- data.frame(
  YR = as.numeric(vec[seq(1, length(vec), 2)]),
  event = vec[seq(2, length(vec), 2)],
  stringsAsFactors = FALSE
)

early.events <- early.events %>%
  left_join(early.yrs, by = "YR")

events <- events %>%
  left_join(dat, by = "YR")



g1  = ggplot(dat, aes(x = YR, y = LAND)) +
  geom_col(width = 1, fill = "blue") +

  scale_x_continuous(
    limits = c(1865, max(dat$YR) + 1),
    breaks = seq(1865, max(dat$YR), by = 5),
    expand = c(0, 0)
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, 0.20)),
    breaks = seq(0, 60000, by = 10000),
    labels = scales::label_comma()
  ) +

  theme_classic() +
  labs(y = "Maritimes Landings (mt)", x = "") +
  theme(
    axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 13),
    axis.title.y = element_text(size = 17, vjust = 2.5),
    axis.text = element_text(face = "bold")
  )



 g2 = g1 +
  geom_text_repel(
    data = early.events,
    aes(label = event),
    direction = "x",
    nudge_x = 1,
    arrow = arrow(length = unit(0.15, "inches")),
    min.segment.length = 0,
    segment.size = 0.9,
    segment.color = "grey",
    box.padding = 0.4,
    fontface = "bold",
    angle = 90,
    hjust = -0.1
  )

 
 g2 +
   geom_text_repel(
   data = subset(events),
   aes(label = event),
   direction = "y",              # only move vertically
   nudge_y = 0.5 * max(dat$LAND),
   arrow = arrow(length = unit(0.15, "inches")),
   min.segment.length = 0,
   segment.size = 0.9,
   segment.color = "grey",
   box.padding = 0.4,
   fontface = "bold"
 )+
   theme_classic() +
   labs(y = "Maritimes Landings (mt)", x = "") +
   theme(
     axis.text.x = element_text(size = 13, angle = 90, vjust = 0.5, hjust = 1),
     axis.text.y = element_text(size = 13),
     axis.title.y = element_text(size = 17, vjust = 2.5),
     axis.text = element_text(face = "bold")
   )
 
 