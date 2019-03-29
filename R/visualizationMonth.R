### Visualization

library(gapminder)
library(ggplot2)
library(gganimate)
library(quantmod)
library(PerformanceAnalytics)

source('~/Projects/rbestcoinstech/R/moreProfitableMonth.R')

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
mbitcoin <- moreProfitableMonth(bitcoin, startDate = startDate,
                                endDate = endDate)
methereum <- moreProfitableMonth(ethereum, startDate = startDate,
                                endDate = endDate)
mripple <- moreProfitableMonth(ripple, startDate = startDate,
                                endDate = endDate)
mtronix <- moreProfitableMonth(tronix, startDate = startDate,
                               endDate = endDate)
mbitcoincash <- moreProfitableMonth(bitcoincash, startDate = startDate,
                               endDate = endDate)
mstellar <- moreProfitableMonth(stellar, startDate = startDate,
                               endDate = endDate)
mtether <- moreProfitableMonth(tether, startDate = startDate,
                               endDate = endDate)
mlitecoin <- moreProfitableMonth(litecoin, startDate = startDate,
                               endDate = endDate)


consolida <- cbind(mbitcoin,
                   methereum,
                   mripple,
                   mtronix,
                   mbitcoincash,
                   mstellar,
                   mtether,
                   mlitecoin)
colnames(consolida) <- c("bitcoin",
                         "ethereum",
                         "ripple",
                         "tronix",
                         "bitcoin_cash",
                         "stellar",
                         "tether",
                         "litecoin")
#charts.PerformanceSummary(consolida)
write.table(consolida, file = "./data/consolida.csv", sep = ",")



