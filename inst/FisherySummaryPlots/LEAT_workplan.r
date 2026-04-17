
require(ganttrify)
wp = read.csv(file.path('C:/Users/cooka/Documents/git/bio.lobster.data/misc/workPlanning26_27.csv'))
p = ganttrify(
  project = wp,
  project_start_date = "2024-04",
  font_family = "Roboto Condensed",
  alpha_wp = 0
)



p +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"   # Jan, Feb, Mar, ...
  )
