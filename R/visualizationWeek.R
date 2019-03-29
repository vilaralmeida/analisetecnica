### Visualization

library(gapminder)
library(ggplot2)
library(gganimate)
library(quantmod)
library(PerformanceAnalytics)

source('~/Projects/rbestcoinstech/R/moreProfitableWeek.R')

bitcoin <- "BTC-USD"
ethereum <- "ETH-USD"
ripple <- "XRP-USD"
tronix <- "TRX-USD"
bitcoincash <- "BCH-USD"
stellar <- "XLM-USD"
tether <- "USDT-USD"
litecoin <- "LTC-USD"

startDate <- "2018-01-01"
endDate <- "2019-01-01"
mbitcoin <- moreProfitableWeek(bitcoin, startDate = startDate,
                                endDate = endDate)
methereum <- moreProfitableWeek(ethereum, startDate = startDate,
                                endDate = endDate)
mripple <- moreProfitableWeek(ripple, startDate = startDate,
                                endDate = endDate)
mtronix <- moreProfitableWeek(tronix, startDate = startDate,
                               endDate = endDate)
mbitcoincash <- moreProfitableWeek(bitcoincash, startDate = startDate,
                               endDate = endDate)
mstellar <- moreProfitableWeek(stellar, startDate = startDate,
                               endDate = endDate)
mtether <- moreProfitableWeek(tether, startDate = startDate,
                               endDate = endDate)
mlitecoin <- moreProfitableWeek(litecoin, startDate = startDate,
                               endDate = endDate)


consolidaWeek <- cbind(mbitcoin,
                   methereum,
                   mripple,
                   mtronix,
                   mbitcoincash,
                   mstellar,
                   mtether,
                   mlitecoin)
colnames(consolidaWeek) <- c("bitcoin",
                         "ethereum",
                         "ripple",
                         "tronix",
                         "bitcoin_cash",
                         "stellar",
                         "tether",
                         "litecoin")
#charts.PerformanceSummary(consolida)
write.table(consolidaWeek, file = "./data/consolidaWeek.csv", sep = ",")



