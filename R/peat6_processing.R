#' Script to load, merge, and process daily to annual budgets.
#' Code varies for each site as input met variables vary across sites
#' Site: Peat 6 yr old
#'
#' @import dplyr
#' @importFrom lubridate month
#' @importFrom dplyr "%>%"

# load eddy flux and met dataset
load("data/peat6_all.Rdata")
peat6_all$dday <- floor(peat6_all$decday)

#Simple average of daily fluxes for gapfilled values
daily <- peat6_all %>%
  group_by(dday, site) %>%
  summarise(mNEE = mean(wc_gf),         #NEE (umol m-2 s-1)
            vNEE = mean(wc_gf_var),     #NEE variance (umol m-2 s-1)^2
            c_obs = sum(!is.na(wc)),    #num of non-filled observations (applies all _obs)
            mCH4 = mean(wm_gf),         #CH4 flux (nmol m-2 s-1)
            vCH4 = mean(wm_gf_var),     #CH4 variance (nmol m-2 s-1)^2
            m_obs = sum(!is.na(wm)),
            GPP = mean(gpp_ANNnight),   #partitioned GPP (umol m-2 s-1)
            vGPP = mean(wc_gf_var)+mean(er_ANN_var), #error propogation for calculated GPP (NEE-ER)
            ER = mean(er_ANNnight),     #partitioned respiration (umol m-2 s-1)
            vER = mean(er_ANN_var),     #partitioned respiration variance (umol m-2 s-1)^2
            ET = mean(wq_gf),           #evapotranspiration (mmol m-2 s-1)
            q_obs = sum(!is.na(wq)),
            H = mean(H_gf),                    #sensible heat flux (W m-2)
            mGCC = mean(GCC, na.rm=T),   #green index
            PAR = mean(PAR, na.rm=T),          #photosynthetic active radiation (umol m-2 s-1)
            Tair = mean(TA.y, na.rm=T),        #air temp (deg C)
            Tw = mean(TW_WATER_10cm, na.rm=T), #water temp (deg C)
            Ts = mean(TS_TULE_8cm, na.rm=T),   #soil temp (deg C)
            mVPD = mean(VPD, na.rm=T),         #VPD (kPa)
            PA = mean(PA.y, na.rm=T),          #atm pressure (kPa)
            u. = mean(ustar, na.rm=T),         #friction velocity (m/s)
            Rain = sum(PRECIP, na.rm=T),       #rain (mm)
            WTD = mean(WT, na.rm=T),           #water table (m from surface)
            Cond = mean(Cond, na.rm=T),        #conductivity (mS)
            DOrange1 = max(DO_mid, na.rm=T) - min(DO_mid, na.rm=T),
            DOmid = mean(DO_mid, na.rm=T),
            DOrange2 = max(DO_deep, na.rm=T) - min(DO_deep, na.rm=T),
            DOdiff = mean(DO_mid, na.rm=T) - mean(DO_deep, na.rm=T),
            COrangeS = max(wCO2_open_surface_ppm, na.rm=T) - min(wCO2_open_surface_ppm, na.rm=T),
            COrangeM = max(wCO2_open_mid_ppm, na.rm=T) - min(wCO2_open_mid_ppm, na.rm=T),
            COrangeD = max(wCO2_open_deep_ppm, na.rm=T) - min(wCO2_open_deep_ppm, na.rm=T),
            COdiff = mean(wCO2_open_surface_ppm, na.rm=T) - mean(wCO2_open_deep_ppm, na.rm=T),
            t_obs = length(wc_gf),      # total observations in the day
            year = round(median(year))) %>%
  filter(year > 2010 & t_obs == 48) #cut pre-wetland measures, and incomplete days at ends of time series

daily[daily == -Inf] <- NaN

#Time and unit conversions
daily$datetime <- as.POSIXct(daily$dday*86400, origin="2010-01-01")
daily$month <- month(daily$datetime)
daily$mgCH4 <- (daily$mCH4*12.01*3600*24)/1000000 #mg C m-2 d-1
daily$gCO2 <- (daily$mNEE*12.01*3600*24)/1000000  #g C m-2 d-1
daily$gER <- (daily$ER*12.01*3600*24)/1000000     #g C m-2 d-1
daily$gGEP <- (daily$GPP*12.01*3600*24)/1000000   #g C m-2 d-1
daily$mET <- (daily$ET*18.01528*3600*24)/1000000  #mm H2O d-1

#Monthly cumulative fluxes
monthly <- daily %>%
  group_by(year, month) %>%
  summarise(mCH4 = mean(mgCH4),
            mNEE = mean(gCO2),
            mER = mean(gER),
            mGEP = mean(gGEP),
            mCond = mean(Cond, na.rm=T),
            #mSal = mean(Sal, na.rm=T),
            days = sum(!is.na(mgCH4))) %>%
  filter(days > 27) # only complete months

# convert to monthly sums
monthly$gCH4 <- monthly$mCH4*monthly$days/1000 #g C m-2 mnth-1
monthly$gNEE <- monthly$mNEE*monthly$days      #g C m-2 mnth-1
monthly$gER <- monthly$mER*monthly$days        #g C m-2 mnth-1
monthly$gGEP <- monthly$mGEP*monthly$days       #g C m-2 mnth-1

#Annual fluxes from daily fluxes
yearly <- daily %>%
  group_by(year, site) %>%
  summarise(tCH4 = mean(mgCH4)*365/1000, #gC m-2 yr-1 (same for all fluxes below)
            ciCH4 = 1.96*(sqrt(mean(vCH4)*(12.01*3600*24*365/(10^9))^2)/sqrt(20)), #95% confidence interval from 20 ANNs cumulative
            tNEE = mean(gCO2)*365,
            ciNEE = 1.96*(sqrt(mean(vNEE)*(12.01*3600*24*365/(10^6))^2)/sqrt(20)), #95% confidence interval from 20 ANNs cumulative
            tER = mean(gER)*365,
            ciER = 1.96*(sqrt(mean(vER)*(12.01*3600*24*365/(10^6))^2)/sqrt(20)), #95% confidence interval from 20 ANNs cumulative
            tGEP = mean(gGEP)*365,
            ciGEP = 1.96*(sqrt(mean(vGPP)*(12.01*3600*24*365/(10^6))^2)/sqrt(20)), #95% confidence interval from 20 ANN (NEE-ER) differences
            Tair = mean(Tair, na.rm=T),
            GCC = mean(mGCC, na.rm=T),
            Cond = mean(Cond, na.rm=T),
            Days = sum(!is.na(mgCH4))) %>%
  filter(Days >= 365) #only keep annual budgets for full years

#Ecosystem C balance and GHG (CO2eq) balance
yearly$Cbalance <- yearly$tNEE + yearly$tCH4 #C m-2 yr-1
yearly$GHGbalance <- (yearly$tNEE*44/12) + (45*yearly$tCH4*16/12)  #CO2-eq m-2 yr-1 using 45x SGWP (Neubauer and Megonigal 2015)
yearly$ciGHG <- (yearly$ciNEE*44/12) + (45*yearly$ciCH4*16/12) #GHG balance 95% confidence interval

#resave with specific names, and add a site name variable
peat6_yearly <- yearly
peat6_monthly <- monthly
peat6_daily <- daily

#remove other dataframes
rm(yearly); rm(daily); rm(monthly)
