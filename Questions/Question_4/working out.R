cncy <- read_rds("data/currencies.rds")
cncy_Carry <- read_rds("data/cncy_Carry.rds")
cncy_value <- read_rds("data/cncy_value.rds")
cncyIV <- read_rds("data/cncyIV.rds")
bbdxy <- read_rds("data/bbdxy.rds")
library(pacman)
pacman::p_load("tidyverse", "fmxdat", "devtools", "tbl2xts", "lubridate", "readr",
"PerformanceAnalytics", "tidyr", "FactoMineR", "factoextra", "rugarch", "forecast")
pacman::p_load(RcppRoll)

#######

## ZAR is most volatile. I use the cncyIV, implied volatility.
# A higher value indicates the market foresees higher future volatility for a currency.
number_of_cncyIV <- read_rds("data/cncyIV.rds") %>% group_by(date) %>% pull(Name) %>% unique()

cncyIV <- read_rds("data/cncyIV.rds") %>% filter(Name %in% c("Brazil_IV", "EU_IV",
"UK_IV", "Turkey_IV", "Russia_IV", "India_IV", "SouthAfrica_IV")) %>%
filter(date > ymd(20000101)) %>% ggplot() + geom_line(aes(x = date, y = Price, color = Name), size = 1, alpha = 0.8) +
fmxdat::fmx_cols() +
labs(x = "Date", title = "Implied Volatility of a chosen basket of currencies", subtitle = "Higher value indicates greater expected volatility")
cncyIV

# I compare the ZAR to mostly other EMEs in addition to the UK & EU. There is no reason to think the ZAR is more volatile than other EMEs.

cncy_names <- read_rds("data/currencies.rds") %>% group_by(date) %>% pull(Name) %>% unique()

cncy_SD <- read_rds("data/currencies.rds") %>% filter(Name %in% c("SouthAfrica_Cncy")) %>%
mutate(across(.cols = Price, .fns = ~./lag(.)-1, .names = "Return")) %>% select(-Price) %>% arrange(date) %>% filter(date > first(date)) %>%
mutate(across(.cols = Return, .fns = ~sqrt(.^2), .names = "SampleSD"))

plot_cncy_SD <- cncy_SD %>% ggplot() + geom_line(aes(date, SampleSD))
back = 100

roll_plot <- cncy_SD %>% mutate(Constant_var = sd(Return)) %>%
    mutate(Roller = roll_sd(Return, n = back, fill = NA)) %>%
    ggplot() +
    geom_line(aes(date, SampleSD), color = "steelblue") +
    geom_hline(aes(date, yintercept = mean(Constant_var)), color = "red",
               alpha = 0.8, size = 2) +
    geom_line(aes(date, y = Roller), color = "darkgreen", alpha = 0.8,
              size = 2) +
    fmxdat::theme_fmx() +
    labs(title = "Sample Var, Constant Var and Rolling Var of USD/ZAR", caption = "Sample Var in blue, Constant Var in red and Rolling Var in Green",
         x = "Date", y = "Standard Deviation")

################################## CARRY TRADES

cncy_Carry_period_2 <- read_rds("data/cncy_Carry.rds") %>% select(-Name) %>% arrange(date) %>% mutate(day = weekdays(as.Date(date))) %>%
filter(day == "Monday") %>% select(-day) %>% mutate(return = log(Price) - log(lag(Price))) %>% slice(-1) %>%  filter(date > ymd(20160601))

cncy_Carry_Price <- read_rds("data/cncy_Carry.rds") %>% select(-Name) %>% arrange(date) %>%
ggplot() + geom_line(aes(date, Price), color = "steelblue") +
fmxdat::theme_fmx() +
labs(title = "Carry Trade Returns", subtitle = "Carry Trade Returns as shown by the Deutsche Bank G10 Harvest Index from 2000 to 2021",
x = "Date", y = "Carry Trade Index")

######

cncy_Carry_period <- read_rds("data/cncy_Carry.rds") %>% select(-Name) %>% arrange(date) %>% mutate(month = month(as.Date(date))) %>%
filter(month == c("1", "4", "7", "10")) %>% mutate(day = day(as.Date(date))) %>% filter(day < 7) %>% slice(-23, -26, -32, -34) %>%
select(-day, -month) %>% mutate(return_carry = log(Price) - log(lag(Price))) %>% slice(-1) %>% select(-Price)


### USD ZAR
cncy_Carry_period_dates <- read_rds("data/cncy_Carry.rds") %>% select(-Name) %>% arrange(date) %>% mutate(month = month(as.Date(date))) %>%
filter(month == c("1", "4", "7", "10")) %>% mutate(day = day(as.Date(date))) %>% filter(day < 7) %>% slice(-23, -26, -32, -34) %>%
select(-day, -month) %>% pull(date)

cncy_plot <- read_rds("data/currencies.rds") %>% filter(Name == "SouthAfrica_Cncy") %>% select(-Name) %>% filter(date %in% cncy_Carry_period_dates) %>%
mutate(return_zar = log(Price) - log(lag(Price))) %>% slice(-1) %>% select(-Price)

combined_plot_1 <- left_join(cncy_Carry_period, cncy_plot, by = "date")

combined_plot_1_plot <- combined_plot_1 %>%
gather(type, return, -date) %>%
ggplot() + geom_line(aes(date, return, color = type)) +
fmxdat::theme_fmx() +
labs(title = "Carry Trade Returns and USD/ZAR returns", subtitle = "Carry Trade Log Returns and USD/ZAR log returns",
x = "Date", y = "Log Returns")

# we expect them to invert if the hypothesis is true.

######### cheap currency valuations - combined this with above.

cncy_value <- read_rds("data/cncy_value.rds") %>% select(-Name) %>% arrange(date) %>%
filter(date %in% cncy_Carry_period_dates) %>%
mutate(return_value = log(Price) - log(lag(Price))) %>% slice(-1) %>% select(-Price)

three_combo_plot <- left_join(combined_plot_1, cncy_value , by = "date") %>%
gather(type, return, -date) %>%
ggplot() + geom_line(aes(date, return, color = type)) +
fmxdat::theme_fmx() +
labs(title = "Carry Trade Returns, USD/ZAR returns and Undervalued Currency Returns", subtitle = "Carry Trade Log Returns, USD/ZAR log returns and log returns of the value index for currencies",
x = "Date", y = "Log Returns")

####### Rand do well when Dollar is strong

bbdxy <- read_rds("data/bbdxy.rds") %>% select(-Name) %>% arrange(date) %>%
filter(date %in% cncy_Carry_period_dates) %>% # I'm using this to indicate quarterly data.
mutate(return_usd = log(Price) - log(lag(Price))) %>% slice(-1) %>% select(-Price)

usdzar <- read_rds("data/currencies.rds") %>% filter(Name == "SouthAfrica_Cncy") %>% select(-Name) %>% filter(date %in% cncy_Carry_period_dates) %>%
mutate(return_zar = log(Price) - log(lag(Price))) %>% slice(-1) %>% select(-Price)

combo_2 <- left_join(bbdxy, usdzar, by = "date")

plot_combo_2 <- combo_2 %>%
gather(type, return, -date) %>%
ggplot() + geom_line(aes(date, return, color = type)) +
fmxdat::theme_fmx() +
labs(title = "USD/ZAR returns and USD with a basket of currencies", subtitle = "USDZAR relative to the strenght of the USD",
x = "Date", y = "Log Returns")























