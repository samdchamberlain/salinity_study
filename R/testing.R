anom_filter <- function(var, window = 7) {
  as.numeric(var - ma(var, order = window))
}

test <- subset(peat6_all, year == 2016 & DOY > 100 & DOY < 300)

anom_data <- test %>%
  arrange(time) %>%
  mutate(CH4_anom = anom_filter(wm_gf),
         TA_anom = anom_filter(TA.x),
         GPP_anom = anom_filter(gpp_ANNnight)) %>%
  arrange(datetime)

tr_wm_TA <- lag_tr(test$gpp_ANNnight, test$wm_gf, 48*2, normalize = T)
conf_wm_TA <- lag_confidence(test$gpp_ANNnight, test$wm_gf, 48*2, type = "TR", runs = 10)

Hours <- seq(0.5, 24*2, by = 0.5)

plot(tr_wm_TA ~ Hours, type = "l", xlim = c(0,36), col = "red")
points(conf_wm_TA*1.66 ~ Hours, type = "l")