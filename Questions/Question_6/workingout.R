library(pacman)
pacman::p_load("tidyverse", "fmxdat", "devtools", "tbl2xts", "lubridate", "readr",
"PerformanceAnalytics", "tidyr", "FactoMineR", "factoextra", "MTS", "robustbase", "rmgarch", "rugarch")
###
MAA <- read_rds("data/MAA.rds")
msci <- read_rds("data/msci.rds") %>% filter(Name %in% c("MSCI_ACWI", "MSCI_USA", "MSCI_RE", "MSCI_Jap"))

## MSCI ##
msci_names <- msci %>% pull(Name) %>% unique
MAA_names <- MAA %>% pull(Ticker) %>% unique


msci_rtn <- msci %>% filter(date > ymd(20100101)) %>% filter(Name %in% msci_names_chosen) %>%
arrange(date) %>% group_by(Name) %>% mutate(dlogret = log(Price) - log(lag(Price))) %>%
mutate(scaled_ret = (dlogret - mean(dlogret, na.rm = T))) %>% slice(-1, -2, -3, -4) %>%
select(-Price, -dlogret) %>% rename(return = scaled_ret)

msci_wide <- msci_rtn %>% pivot_wider(names_from = Name, values_from = return)

############



















