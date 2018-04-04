anom_filter <- function(var, window = 5) {
  as.numeric(var - ma(var, order = window))
}

test <- subset(peat6_all, year == 2012 & DOY > 100 & DOY < 300)

test <- test %>%
  arrange(time) %>%
  mutate(CH4_anom = anom_filter(wm_gf),
         TA_anom = anom_filter(TA.x),
         GPP_anom = anom_filter(gpp_ANNnight)) %>%
  arrange(datetime)

#re-insert NA values to CH4 anomaly
test$CH4na_anom <- ifelse(is.na(test$wm), NA, test$CH4_anom)

tr_wm_TA <- lag_tr(test$GPP_anom, test$CH4na_anom, 48*2, normalize = T)
conf_wm_TA <- lag_confidence(test$GPP_anom, test$CH4na_anom, 48*2, type = "TR", alpha = 0.05, runs = 20)

Hours <- seq(0.5, 24*2, by = 0.5)

plot(tr_wm_TA ~ Hours, type = "l", xlim = c(0,36), col = "black")
points(conf_wm_TA ~ Hours, type = "l", col = "red")