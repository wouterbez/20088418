# Packages
library(pacman)
library(rmsfuns)
library(fmxdat)
p_load("tidyr","tbl2xts", "devtools", "lubridate","readr","PerformanceAnalytics", "tidyverse")
#Data
T40 <- read_rds("data/T40.rds")
RebDays <- read_rds("data/Rebalance_days.rds")

# Compare overall performance of SWIX and ALSI

R_xts <- T40 %>% select(-Short.Name, -Sector, -Index_Name, -J400, -J200) %>%
tbl_xts(., cols_to_xts = Return, spread_by = Tickers)

J200_W_xts <- T40 %>% select(-Short.Name, -Sector, -Index_Name, -J400, -Return) %>%
tbl_xts(., cols_to_xts = J200, spread_by = Tickers)

J400_W_xts <- T40 %>% select(-Short.Name, -Sector, -Index_Name, -J200, -Return) %>%
tbl_xts(., cols_to_xts = J400, spread_by = Tickers)

# Set all NA returns to zero:
R_xts[is.na(R_xts)] <- 0
J400_W_xts[is.na(J400_W_xts)] <- 0
J200_W_xts[is.na(J200_W_xts)] <- 0

J200_Portfolio <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = J200_W_xts, geometric = TRUE)
J400_Portfolio <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = J400_W_xts, geometric = TRUE)

J200_Portf_Rets <- J200_Portfolio$portfolio.returns %>% xts_tbl() %>% mutate(J200_Cum_Ret = cumprod(1+portfolio.returns))

J400_Portf_Rets <- J400_Portfolio$portfolio.returns %>% xts_tbl() %>% mutate(J400_Cum_Ret = cumprod(1+portfolio.returns))

port_compare <- left_join(J200_Portf_Rets, J400_Portf_Rets, by = "date") %>% select(date, J200_Cum_Ret, J400_Cum_Ret) %>%
gather(Index, Return, -date)

plot_port <- port_compare %>%  ggplot() + geom_line(aes(date, Return, color = Index)) + theme_bw() +
fmxdat::fmx_cols() +
fmxdat::theme_fmx(title.size = ggpts(50), CustomCaption = F) +
labs(x = "Date", y = "Cumulative Returns", title = "J200 and J400 Cumulative Returns over time",
subtitle = "A comparison of the cumulative returns of the ALSI Top 40 and the SWIX Top 40 over time")

fmxdat::finplot(port_compare, x.vert = T, x.date.type = "%Y", x.date.dist = "1 years")


# Volatile USDZAR
zar <- read_rds("data/usdzar.rds")

ZARSD <- zar %>% filter(date > ymd(20080102)) %>%
mutate(YearMonth = format(date,"%Y%B")) %>% group_by(YearMonth) %>% summarise(SD = sd(Price)*sqrt(52)) %>%
mutate(TopQtile = quantile(SD, 0.8), BotQtile = quantile(SD,0.2))

Hi_Vol <- ZARSD %>% filter(SD >TopQtile) %>% pull(YearMonth)
Low_Vol <- ZARSD %>% filter(SD < BotQtile) %>% pull (YearMonth)

## Who has better vol?

overall_SD <- port_compare %>% mutate(YearMonth = format(date,"%Y%B")) %>% group_by(Index) %>%
summarise(Full_SD = sd(Return)*sqrt(252))

high_vol_SD <- port_compare %>% mutate(YearMonth = format(date,"%Y%B")) %>%
filter(YearMonth %in% Hi_Vol)%>% group_by(Index) %>%
summarise(Highvol_SD = sd(Return)*sqrt(252))

Low_vol_SD <- port_compare %>% mutate(YearMonth = format(date,"%Y%B")) %>%
filter(YearMonth %in% Low_Vol)%>% group_by(Index) %>%
summarise(Lowvol_SD = sd(Return)*sqrt(252))


vol_compare_demo <- left_join(overall_SD, high_vol_SD, by = "Index")

vol_compare <- left_join(vol_compare_demo, Low_vol_SD, by = "Index") %>%
mutate(high_SD_ratio = Highvol_SD/Full_SD, low_SD_Ratio = Lowvol_SD/Full_SD)


#### Compare overall performance over Large Caps ##########

LC_R_xts <- T40 %>% select(-Short.Name, -Sector, -J400, -J200) %>% filter(Index_Name == "Large_Caps") %>%
tbl_xts(., cols_to_xts = Return, spread_by = Tickers)

LC_J200_W_xts <- T40 %>% select(-Short.Name, -Sector, -J400, -Return) %>% filter(Index_Name == "Large_Caps") %>%
tbl_xts(., cols_to_xts = J200, spread_by = Tickers)

LC_J400_W_xts <- T40 %>% select(-Short.Name, -Sector, -Sector, -J200, -Return) %>% filter(Index_Name == "Large_Caps") %>%
tbl_xts(., cols_to_xts = J400, spread_by = Tickers)

# Set all NA returns to zero:
LC_R_xts[is.na(LC_R_xts)] <- 0
LC_J400_W_xts[is.na(LC_J400_W_xts)] <- 0
LC_J200_W_xts[is.na(LC_J200_W_xts)] <- 0

LC_J200_Portfolio <- rmsfuns::Safe_Return.portfolio(R = LC_R_xts, weights = LC_J200_W_xts, geometric = TRUE)
LC_J400_Portfolio <- rmsfuns::Safe_Return.portfolio(R = LC_R_xts, weights = LC_J400_W_xts, geometric = TRUE)

LC_J200_Portf_Rets <- LC_J200_Portfolio$portfolio.returns %>% xts_tbl() %>% mutate(LC_J200_Cum_Ret = cumprod(1+portfolio.returns))

LC_J400_Portf_Rets <- LC_J400_Portfolio$portfolio.returns %>% xts_tbl() %>% mutate(LC_J400_Cum_Ret = cumprod(1+portfolio.returns))

LC_port_compare <- left_join(LC_J200_Portf_Rets, LC_J400_Portf_Rets, by = "date") %>% select(date, LC_J200_Cum_Ret, LC_J400_Cum_Ret) %>%
    gather(Index, Return, -date)

LC_plot_port <- LC_port_compare %>%  ggplot() + geom_line(aes(date, Return, color = Index)) + theme_bw() +
    fmxdat::fmx_cols() +
    fmxdat::theme_fmx(title.size = ggpts(50), CustomCaption = F) +
    labs(x = "Date", y = "Cumulative Returns", title = "Large Caps J200 and J400 Cumulative Returns over time",
         subtitle = "A comparison of the cumulative returns of the Large Caps in the ALSI Top 40 and the SWIX Top 40 over time")

fmxdat::finplot(LC_port_compare, x.vert = T, x.date.type = "%Y", x.date.dist = "1 years")

##### Compare overall performance over Mid Caps ##########

MC_R_xts <- T40 %>% select(-Short.Name, -Sector, -J400, -J200) %>% filter(Index_Name == "Mid_Caps") %>%
    tbl_xts(., cols_to_xts = Return, spread_by = Tickers)

MC_J200_W_xts <- T40 %>% select(-Short.Name, -Sector, -J400, -Return) %>% filter(Index_Name == "Mid_Caps") %>%
    tbl_xts(., cols_to_xts = J200, spread_by = Tickers)

MC_J400_W_xts <- T40 %>% select(-Short.Name, -Sector, -Sector, -J200, -Return) %>% filter(Index_Name == "Mid_Caps") %>%
    tbl_xts(., cols_to_xts = J400, spread_by = Tickers)

# Set all NA returns to zero:
MC_R_xts[is.na(MC_R_xts)] <- 0
MC_J400_W_xts[is.na(MC_J400_W_xts)] <- 0
MC_J200_W_xts[is.na(MC_J200_W_xts)] <- 0

MC_J200_Portfolio <- rmsfuns::Safe_Return.portfolio(R = MC_R_xts, weights = MC_J200_W_xts, geometric = TRUE)
MC_J400_Portfolio <- rmsfuns::Safe_Return.portfolio(R = MC_R_xts, weights = MC_J400_W_xts, geometric = TRUE)

MC_J200_Portf_Rets <- MC_J200_Portfolio$portfolio.returns %>% xts_tbl() %>% mutate(MC_J200_Cum_Ret = cumprod(1+portfolio.returns))

MC_J400_Portf_Rets <- MC_J400_Portfolio$portfolio.returns %>% xts_tbl() %>% mutate(MC_J400_Cum_Ret = cumprod(1+portfolio.returns))

MC_port_compare <- left_join(MC_J200_Portf_Rets, MC_J400_Portf_Rets, by = "date") %>% select(date, MC_J200_Cum_Ret, MC_J400_Cum_Ret) %>%
gather(Index, Return, -date)

MC_plot_port <- MC_port_compare %>%  ggplot() + geom_line(aes(date, Return, color = Index)) + theme_bw() +
    fmxdat::fmx_cols() +
    fmxdat::theme_fmx(title.size = ggpts(50), CustomCaption = F) +
    labs(x = "Date", y = "Cumulative Returns", title = "Mid Caps J200 and J400 Cumulative Returns over time",
         subtitle = "A comparison of the cumulative returns of the Mid Caps in the ALSI Top 40 and the SWIX Top 40 over time")

fmxdat::finplot(MC_port_compare, x.vert = T, x.date.type = "%Y", x.date.dist = "1 years")


#############




















