#Lobster production units
require(tidyr)
require(dplyr)
require(bio.lobster)
require(devtools)
require(ggplot2)
library(viridis)
la()

###DTW DBA clustering 
#DTW‑DBA clusters time‑series by aligning their patterns in time (DTW)
#and builds a representative pattern (DBA) via their centroids not simply an average,
# this allows LFAs to be grouped based on the shape of their trajectories even if they peak in slightly different years

a = lobster.db('annual.landings')
a = subset(a,!is.na(YR)& YR>1975 & YR<2025)
sa = a %>% gather(key='LFA',value='Landings',-YR)
sa = subset(sa,LFA<'LFA33')
sa = subset(sa,LFA %ni% 'LFA31')
sa = sa[order(sa$LFA,sa$YR),]
gg = ggplot(sa,aes(x=YR,y=Landings/1000))+geom_bar(stat='identity')+
  facet_wrap(~LFA, scales='free_y' )+xlab('Season')+ylab('Landings (kt)')


a = lobster.db('seasonal.landings')
a$SYEAR = as.numeric(substring(a$SYEAR,6,9))
a = subset(a,!is.na(SYEAR)& SYEAR>1975& SYEAR<2025)
sa1 = a %>% pivot_longer(cols=starts_with('LFA'),names_to="LFA",values_to='Landings')
names(sa1)[1] = "YR"

o = bind_rows(sa,sa1)
o1 = subset(o,LFA %ni% c('LFA38B','LFA28'))

library(tidyr)
library(dplyr)
require(tibble)

# df: area, year, value
X <- o1 %>%
  arrange(LFA, YR) %>%
  pivot_wider(names_from = YR, values_from = Landings) %>%
  column_to_rownames("LFA") %>%
  as.matrix()

library(dtwclust)

library(dtwclust)
library(tidyverse)
library(janitor)
library(cluster)
library(scales)

X_scaled <- t(apply(X, 1, zscore))
n_years <- ncol(X_scaled)
w <- max(1, floor(0.10 * n_years))  # 10% window (at least 1)
year_cols <- colnames(X)[grepl("^(19|20)\\d{2}$", colnames(X))]
dist_fun <- function(a, b) dtw_basic(a, b, window.size = w, norm = "L2")

# DTW distance matrix across LFAs
D <- proxy::dist(X_scaled, method = dist_fun)

# Hierarchical clustering (average linkage works well with DTW)
hc <- hclust(D, method = "average")
plot(hc)


k <- 4
clusters <- cutree(hc, k = k)

cluster_assignments <- data.frame(
  lfa = rownames(X_scaled),
  cluster = factor(clusters)
)


pc <- tsclust(
  X_scaled,
  type = "partitional",
  k = 4,
  distance = "dtw_basic",
  centroid = "dba",
  args = tsclust_args(dist = list(window.size = w))
)

cluster_partitional <- data.frame(
  lfa = rownames(X_scaled),
  cluster = factor(pc@cluster)
)
plot(pc)   # shows DBA centroids


library(tidyverse)

df_plot <- as.data.frame(X_scaled) %>%
  mutate(lfa = rownames(.)) %>%
  pivot_longer(-lfa, names_to = "year", values_to = "value") %>%
  left_join(cluster_assignments, by = "lfa") %>%
  mutate(year = as.numeric(year))
df_plot$year = rep(1976:2024,times=length(unique(df_plot$lfa)))

ggplot(df_plot, aes(year, value, group = lfa, color = cluster)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~ cluster, scales = "free_y") +
  theme_minimal() +
  labs(x = "Year", y = "Scaled annual landings",
       title = "DTW Clustered LFAs by Annual Landings Trajectory")


lfa_cluster_table <- data.frame(
  LFA = rownames(X_scaled),
  Cluster = clusters
) %>%
  arrange(Cluster, LFA)

lfa_cluster_table

