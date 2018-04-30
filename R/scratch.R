#add this to the supplement.....

#apply anomaly filter
anom_data <- peat6_all %>%
  arrange(year, time) %>%
  mutate(CH4_anom = anom_filter(wm_gf),
         TA_anom = anom_filter(TA.x),
         GPP_anom = anom_filter(gpp_ANNnight))

anom_data <- arrange(anom_data, datetime)
anom_data$CH4na_anom <- ifelse(is.na(anom_data$wm), NA, anom_data$CH4_anom) # re-insert missing values

#calculate lagged transfer entropy time series (up to 2 day lag at half-hourly timestep)
# for GPP -> CH4
tr_wm_gpp <- transfer_entropy(by_year$GPP_anom, by_year$CH4na_anom, xlag = 1)

run_tests <- rep(seq(10, 100, by = 10), each = 5)
high_runs <- rep(c(500, 1000), each = 5)
run_tests <- c(run_tests, high_runs)
output <- rep(NA, length(run_tests))

for (i in 1:length(run_tests)) {
  output[i] <- tr_confidence(by_year$GPP_anom, by_year$CH4na_anom, xlag = 1, alpha = 0.05,
                             bins = 10, runs = run_tests[i])
}

output_set <- data.frame(output, run_tests)

ggplot(output_set, aes(x = run_tests, y = output)) +
  geom_point() +
  stat_smooth(se = FALSE)

sig_limits <- rep(seq(0.01, .1, by = 0.01), each = 10)
output <- rep(NA, length(sig_limits))

for (i in 1:length(sig_limits)) {
  output[i] <- tr_confidence(by_year$GPP_anom, by_year$CH4na_anom, xlag = 1, alpha = sig_limits[i],
                             bins = 10, runs = 25)
}

output_set <- data.frame(output, sig_limits)

ggplot(output_set, aes(x = sig_limits, y = output)) +
  geom_point() +
  stat_smooth(se = FALSE)
