getAllCryptoFromTopExchanges <- function(n = 10) {

listaExchanges <- getExchanges()

binanceMarket <- getExchangeMarkets(getExchangeAlias("Binance"))
okexMarket <- getExchangeMarkets(getExchangeAlias("OKEx"))
zbMarket <- getExchangeMarkets(getExchangeAlias("ZB.COM"))
digifinexMarket <- getExchangeMarkets(getExchangeAlias("DigiFinex"))
bitzMarket <- getExchangeMarkets(getExchangeAlias("Bit-Z"))
bitforexMarket <- getExchangeMarkets(getExchangeAlias("BitForex"))
coinbeneMarket <- getExchangeMarkets(getExchangeAlias("CoinBene"))
cryptonexMarket <- getExchangeMarkets(getExchangeAlias("Cryptonex"))
lbankMarket <- getExchangeMarkets(getExchangeAlias("LBank"))
BiboxMarket <- getExchangeMarkets(getExchangeAlias("Bibox"))



listaPairCrypto <- NA

for (j in 1:n) {

  exchangeMarkets <- getExchangeMarkets(
    getExchangeAlias(listaExchanges[j,]$Name)
  )
  if (is.na(listaPairCrypto)) {
    listaPairCrypto <- exchangeMarkets$Currency
  } else {
    listaPairCrypto <- c(listaPairCrypto, exchangeMarkets$Currency)
  }
  # Espera 2 segundos
  Sys.sleep(10)

}

listaPairCrypto

}
