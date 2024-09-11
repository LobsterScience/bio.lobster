fillSubstrate_vh <- function(x) {
  x = x[order(x$interval),] # Sort transects by interval
  s = data.frame(NewInt = seq(0, max(x$transect_length), by=5))  ### Should have been the maximum interval
  i = unique(x$diver) # Index by diver
  o = list()
  for(j in 1:length(i)){
    print(paste("Processing diver", i[j]))
    x2 = subset(x, diver == i[j]) 
    x2i = subset(x2, select = c(location, date, transect_id, transect_no, transect_width, transect_length,
                                start_lat, start_long, end_lat, end_long, diver, max_depth, avg_depth, time_in, time_out, bottom_temp,dates, month, year, tid))[1,] # The columns that stay the same throughout transect
    
    x2s = subset(x2, select = c(interval, substrate_1, substrate_2, lobster, carapace, sex, stage, clutch, shell_condition, comment))
    x2s$NewInt = s$NewInt[cut(x2s$interval, breaks = s$NewInt, labels = FALSE, include.lowest = TRUE)]
    s = subset(s, NewInt < unique(x$transect_length))
    x2sp = merge(x2s, s, all.y = TRUE)
    
    # Manually fill NA values for substrate_1
    for (k in 2:nrow(x2sp)) {
      if (is.na(x2sp$substrate_1[k])) {
        x2sp$substrate_1[k] = x2sp$substrate_1[k - 1]
      }
    }
    print(paste("Completed filling substrate_1 for diver", i[j]))
    
    # Manually fill NA values for substrate_2 with transition handling
    last_value <- NA
    for (k in 1:nrow(x2sp)) {
      if (is.na(x2sp$substrate_2[k])) {
        x2sp$substrate_2[k] <- last_value
      } else {
        last_value <- x2sp$substrate_2[k]
      }
    }
    print(paste("Completed filling substrate_2 for diver", i[j]))
    
    inf = as.data.frame(sapply(x2i, rep.int, nrow(x2sp)))
    o[[j]] = as.data.frame(cbind(inf, x2sp))
    print(paste("Completed processing for diver", i[j]))
  }
  result <- data.frame(do.call(rbind, o))
  print("Completed processing all divers")
  return(result)
}
