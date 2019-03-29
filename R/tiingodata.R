## Generate Crypto Data tiingo
#' @import riingo

# ver configuracao inicial riingo em caso de novo setup
library(riingo)
library(xts)
library(dplyr)
library(magrittr)
library(quantmod)

user <- "bestcoins"
key <- "b5cf589600af06e30ba0a6c57d0f6b7118d8a81d"
startDate = "2018-01-01"
endDate = Sys.Date() - 1

# Get btcusd data
btcusd <- na.omit(riingo::riingo_crypto_prices(c("btcusd"), start_date = startDate, end_date = endDate))
btcxts <- xts(btcusd, order.by = btcusd$date)
# btczoo <- zoo(btcxts$close, btcxts$date)
weekly.btc.avg <-  apply.weekly(btcxts$close, mean)
monthly.btc.avg <-  apply.monthly(btcxts$close, mean)

# Get ethusd data
ethusd <- na.omit(riingo::riingo_crypto_prices(c("ethusd"), start_date = startDate, end_date = endDate))
ethxts <- xts(ethusd, order.by = ethusd$date)
# ethzoo <- zoo(ethxts$close, ethxts$date)
weekly.eth.avg <- apply.weekly(ethxts$close, mean)
monthly.eth.avg <- apply.monthly(ethxts$close, mean)


xrpusd <- na.omit(riingo::riingo_crypto_prices(c("xrpusd"), start_date = startDate, end_date = endDate))
xrpxts <- xts(xrpusd, order.by = ethusd$date)
# ethzoo <- zoo(ethxts$close, ethxts$date)
weekly.xrp.avg <- apply.weekly(xrpxts$close, mean)
monthly.xrp.avg <- apply.monthly(xrpxts$close, mean)

crypto <- cbind(monthly.btc.avg)
table.AnnualizedReturns(crypto)
charts.PerformanceSummary(crypto)




