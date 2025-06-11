
set.seed(123)  # Ensures reproducibility
n=40

random_series <- rnorm(n)  
rr = rmed(1:40,random_series)
plot(random_series, type = "p", xlab = "Time", ylab = "Value")
lines(1:40,rr$x,col='red')

phi <- 0.7  # Autoregressive coefficient
ar_series <- numeric(n)
ar_series[1] <- rnorm(1)  # Initial value

# Generate an autocorrelated time series
for (t in 2:n) {
  ar_series[t] <- phi * ar_series[t - 1] + rnorm(1)
}
ra = runmed(ar_series,k=3)

# Plot the time series
plot(ar_series, type = "p", main = "Autocorrelated Time Series", xlab = "Time", ylab = "Value")
lines(1:40,ra,col='red')


#ra at the end

ra[length(ra)]
median(c(ar_series[length(ar_series)],ra[length(ra)-1 ],3*ra[length(ra)-1 ] - 2*ra[length(ra)-2 ]) )
