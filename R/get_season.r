#' @export
get_season <- function(date) {
  month_day <- format(date, "%m-%d")
  if (month_day >= "12-21" || month_day <= "03-20") {
    return("Winter")
  } else if (month_day >= "03-21" && month_day <= "06-20") {
    return("Spring")
  } else if (month_day >= "06-21" && month_day <= "09-22") {
    return("Summer")
  } else {
    return("Fall")
  }
}
