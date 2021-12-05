msci <- read_rds("data/msci.rds")
bonds <- read_rds("data/bonds_10y.rds")
comms <- read_rds("data/comms.rds")

library(pacman)
pacman::p_load("tidyverse", "fmxdat", "devtools", "tbl2xts", "lubridate", "readr",
"PerformanceAnalytics", "tidyr", "FactoMineR", "factoextra", "MTS", "robustbase", "rmgarch", "rugarch")

###MSCI Setup
msci_names <- read_rds("data/msci.rds") %>% pull(Name) %>% unique
msci_names_chosen <- c("MSCI_ACWI", "MSCI_RE", "MSCI_USA", "MSCI_USREIT")

msci_rtn <- msci %>% filter(date > ymd(20100101)) %>% filter(Name %in% msci_names_chosen) %>%
arrange(date) %>% group_by(Name) %>% mutate(dlogret = log(Price) - log(lag(Price))) %>%
mutate(scaled_ret = (dlogret - mean(dlogret, na.rm = T))) %>% slice(-1, -2, -3, -4) %>%
select(-Price, -dlogret) %>% rename(return = scaled_ret)

### Commodities Setup
comms <- read_rds("data/comms.rds") %>% filter(date > ymd(20100101)) %>%
arrange(date) %>% group_by(Name) %>% mutate(dlogret = log(Price) - log(lag(Price))) %>%
mutate(scaled_ret = (dlogret - mean(dlogret, na.rm = T))) %>% slice(-1, -2, -3) %>%
select(-Price, -dlogret) %>% rename(return = scaled_ret) %>% filter(Name == "Bcom_Index")

comms_wide <- comms %>% pivot_wider(names_from = Name, values_from = return)
msci_wide <- msci_rtn %>% pivot_wider(names_from = Name, values_from = return)
# I exclude oil and gold.

bonds_names <- read_rds("data/bonds_10y.rds") %>% pull(Name) %>% unique
bonds <- read_rds("data/bonds_10y.rds") %>% filter(Name == "US_10Yr")

# Connect MSCI and Commodities
xts_rtn <- left_join(msci_wide, comms_wide, by = "date") %>%
gather(Name, Return, -date) %>% tbl_xts(.,cols_to_xts = "Return", spread_by = "Name")

######

DCCPre <- dccPre(xts_rtn, include.mean = T, p = 0)

Vol <- DCCPre$marVol
colnames(Vol) <- colnames(xts_rtn)
Vol <-
data.frame( cbind( date = index(xts_rtn), Vol)) %>%
mutate(date = as.Date(date)) %>%  tbl_df()
TidyVol <- Vol %>% gather(Stocks, Sigma, -date)
ggplot(TidyVol) + geom_line(aes(x = date, y = Sigma, colour = Stocks))

# It becomes clear that there is strong correlation in returns.

StdRes <- DCCPre$sresi
detach("package:tidyverse", unload=TRUE)
detach("package:tbl2xts", unload=TRUE)
DCC <- dccFit(StdRes, type="Engle")

pacman::p_load("tidyverse", "tbl2xts", "broom")

###

Rhot <- DCC$rho.t
ReturnSeries = xts_rtn
DCC.TV.Cor = Rhot

renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {
ncolrtn <- ncol(ReturnSeries)
namesrtn <- colnames(ReturnSeries)
paste(namesrtn, collapse = "_")

nam <- c()
xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)
    nam <- c()
    for (j in 1:(ncolrtn)) {
        for (i in 1:(ncolrtn)) {
            nam[(i + (j-1)*(ncolrtn))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")
        }
    }

    colnames(DCC.TV.Cor) <- nam

    # So to plot all the time-varying correlations wrt SBK:
    # First append the date column that has (again) been removed...
    DCC.TV.Cor <-
        data.frame( cbind( date = index(ReturnSeries), DCC.TV.Cor)) %>% # Add date column which dropped away...
        mutate(date = as.Date(date)) %>%  tbl_df()

    DCC.TV.Cor <- DCC.TV.Cor %>% gather(Pairs, Rho, -date)

    DCC.TV.Cor

}


Rhot <- renamingdcc(ReturnSeries = xts_rtn, DCC.TV.Cor = Rhot)

head(Rhot %>% arrange(date))

# Plot 1

g1 <- ggplot(Rhot %>% filter(grepl("MSCI_ACWI_", Pairs ), !grepl("_MSCI_ACWI", Pairs)) ) +
geom_line(aes(x = date, y = Rho, colour = Pairs)) +
ggtitle("Dynamic Conditional Correlations: MSCI_ACWI")
print(g1)

g2 <- ggplot(Rhot %>% filter(grepl("Bcom_Index_", Pairs ), !grepl("_Bcom_Index", Pairs)) ) +
geom_line(aes(x = date, y = Rho, colour = Pairs)) +
ggtitle("Dynamic Conditional Correlations: Bcom_Index")
print(g2)

g3 <- ggplot(Rhot %>% filter(grepl("MSCI_RE_", Pairs ), !grepl("_MSCI_RE", Pairs)) ) +
geom_line(aes(x = date, y = Rho, colour = Pairs)) +
ggtitle("Dynamic Conditional Correlations: MSCI_RE")
print(g3)

g4 <- ggplot(Rhot %>% filter(grepl("MSCI_USREIT_", Pairs ), !grepl("_MSCI_USREIT", Pairs)) ) +
geom_line(aes(x = date, y = Rho, colour = Pairs)) +
ggtitle("Dynamic Conditional Correlations: MSCI_USREIT")
print(g4)

g5 <- ggplot(Rhot %>% filter(grepl("MSCI_USA_", Pairs ), !grepl("_MSCI_USA", Pairs)) ) +
geom_line(aes(x = date, y = Rho, colour = Pairs)) +
ggtitle("Dynamic Conditional Correlations: MSCI_USA")
print(g5)












