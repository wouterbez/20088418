# Historical Context
library(tidyverse)
library(xts)
library(rmsfuns)
library(fmxdat)
library(tbl2xts)
library(lubridate)
## Plot 1 ##
SA_bonds <- read_rds("data/SA_Bonds.rds")

SA_bond_calc <- SA_bonds %>% arrange(date) %>% mutate(YS_10Yr_3M = ZA_10Yr - SA_3M) %>%
mutate(YS_10Yr_2Yr = ZA_10Yr - ZA_2Yr) %>%
mutate(YS_2yr_3M = ZA_2Yr - SA_3M) %>%
select(-SA_3M, -ZA_10Yr, -ZA_2Yr) %>%
gather(rate_type, rate, -date)

g <- SA_bond_calc %>%  ggplot() +
geom_line(aes(date, rate, colour = rate_type), size = 1.1, alpha = 0.8) +
fmxdat::theme_fmx() +
labs(x = "Date", y = "Yield Spreads", title = "Yield Spreads", subtitle = "Historical view of yield spreads in South Africa, 1999 to 2021", caption = "Spreads are calculated as longer period less shorter period.") +
fmx_cols()

finplot(g, x.date.dist = "3 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)

## Plot 2

usdzar <- read_rds("data/usdzar.rds") %>%  filter(date > ymd(20180101))
names(usdzar)[3] <- 'USDZAR'
p <- usdzar %>% select (-Name) %>%
ggplot() +
geom_line(aes(date, USDZAR), color = "steelblue", size = 1.1, alpha = 0.8)
p

# Plot 3 - US v SA

bonds_10yr <- read_rds("data/bonds_10y.rds")
bonds_2yr <- read_rds("data/bonds_2y.rds")

US_10yr <- bonds_10yr %>% group_by(Name) %>% filter(Name == "US_10Yr") %>% ungroup() %>% select(-Name)
names(US_10yr)[2] <- 'US_10Yr'
US_2yr <- bonds_2yr %>% group_by(Name) %>% filter(Name == "US_2yr") %>% ungroup() %>% select(-Name)
names(US_2yr)[2] <- 'US_2Yr'

# I want to compare SA yield to USA yield for maturities available.

US_Yields <- left_join(US_10yr, US_2yr, by = "date")
SA_Yields <- read_rds("data/SA_Bonds.rds") %>% select(-SA_3M)
Combined_Yields <- left_join(US_Yields, SA_Yields, by = "date") %>% filter(date > ymd(20000101))

Combined_Spreads <- Combined_Yields %>% mutate(ZA_US_10Yr = ZA_10Yr - US_10Yr) %>%
mutate(ZA_US_2Yr = ZA_2Yr - US_2Yr) %>%  select(date, ZA_US_10Yr, ZA_US_2Yr) %>%
gather(Spread_Type, Spread, -date) %>%
ggplot() +
geom_line(aes(date, Spread, colour = Spread_Type), size = 1.1, alpha = 0.8) +
fmxdat::theme_fmx() +
labs(x = "date", y = "Yield Spread", title = "Yield Spreads between South African and US Bonds", subtitle = "Historical view of yield spreads between South Africa and the USA, 2000 to 2021", caption = "Spreads are calculated as SA yield less USA yield.") +
fmx_cols()

finplot(Combined_Spreads, x.date.dist = "3 year", x.date.type = "%Y", x.vert = T, y.pct = F, y.pct_acc = 1)

# Plot 4















