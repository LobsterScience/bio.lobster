
require(ganttrify)
wp = read.csv('workplan.csv')
ganttrify(
  project = wp,
  project_start_date = "2024-04",
  font_family = "Roboto Condensed",
  alpha_wp = 0
)
