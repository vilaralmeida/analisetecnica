##############################################################
# BestCoins.tech - Solução de Analise Tecnica de CryptoAtivos #
# (c) Rodrigo Almeida - rodrigo.almeida@gmail.com             #
##############################################################


#'
#' Recupera a lista de Cryptoativos com a relação USD avaliada no site Yahoo Finance
#' @return dataframe com as informações dos símbolos dos criptoativos e seu significado
#' @examples
#' getCryptoSymbols()
#' @import rvest, hashmap

getExchanges <- function() {

  set.seed( 123 );
  #' create hash table
  HH <- new.env( hash = TRUE );

  # Mapeando Exchanges para Alias de Consulta
  assign("Binance", "binance", env = HH)
  assign("OKEx", "okex", env = HH)
  assign("DigiFinex", "digifinex", env = HH)
  assign("ZB.COM", "zb-com", env = HH)
  assign("CoinBene", "coinbene", env = HH)
  assign("Huobi Global", "huobi-global", env = HH)
  assign("Bit-Z", "bit-z", env = HH)
  assign("Bibox", "bibox", env = HH)
  assign("LBank", "lbank", env = HH)
  assign("IDAX", "idax", env = HH)
  assign("CHAOEX", "chaoex", env = HH)
  assign("BW", "bw", env = HH)
  assign("DOBI Exchange", "dobi-exchange", env = HH)
  assign("Coineal", "binance", env = HH)
  assign("BCEX", "bcex", env = HH)
  assign("Cryptonex", "cryptonex", env = HH)
  assign("BitForex", "binance", env = HH)
  assign("HitBTC", "hitbtc", env = HH)
  assign("Fatbtc", "fatbtc", env = HH)
  assign("OEX", "oex", env = HH)
  assign("IDCM", "idcm", env = HH)
  assign("RightBTC", "rightbtc", env = HH)
  assign("P2PB2B", "p2bb2b", env = HH)
  assign("Coinsuper", "coinsuper", env = HH)
  assign("LATOKEN", "latoken", env = HH)
  assign("Simex", "simex", env = HH)
  assign("Exrates", "exrates", env = HH)
  assign("DragonEX", "dragonex", env = HH)
  assign("CoinTiger", "cointiger", env = HH)
  assign("Allcoin", "allcoin", env = HH)
  assign("Bitfinex", "bitfinex", env = HH)
  assign("UPbit", "upbit", env = HH)
  assign("CoinMex", "coinmex", env = HH)
  assign("TOPBTC", "topbtc", env = HH)
  assign("TOKOK", "tokok", env = HH)
  assign("Hotbit", "hotbit", env = HH)
  assign("Kraken", "kraken", env = HH)
  assign("CoinEgg", "coinegg", env = HH)
  assign("LocalTrade", "localtrade", env = HH)
  assign("Bitinka", "bitinka", env = HH)
  assign("BiteBTC", "bitebtc", env = HH)
  assign("Coinbase Pro", "coinbase-pro", env = HH)
  assign("55 Global Mar...", "55-global-markets", env = HH)
  assign("Sistemkoin", "sistemkoin", env = HH)
  assign("UEX", "uex", env = HH)
  assign("Bitstamp", "bitstamp", env = HH)
  assign("Kryptono", "kryptono", env = HH)
  assign("B2BX", "b2bx", env = HH)
  assign("Bilaxy", "bilaxy", env = HH)
  assign("Bittrex", "bittrex", env = HH)
  assign("Gate.io", "gate-io", env = HH)
  assign("CoinsBank", "coinsbank", env = HH)
  assign("Paribu", "paribu", env = HH)
  assign("BtcTurk", "btcturk", env = HH)
  assign("BTC-Alpha", "btc-alpha", env = HH)
  assign("C2CX", "c2cx", env = HH)
  assign("Luno", "luno", env = HH)
  assign("Waves Decentr...", "waves-dex", env = HH)
  assign("Indodax", "indodax", env = HH)
  assign("GOPAX", "gopax", env = HH)
  assign("Coinroom", "coinroom", env = HH)
  assign("BtcTrade.im", "bictrade-im", env = HH)
  assign("Korbit", "korbit", env = HH)
  assign("BitBay", "bitbay", env = HH)
  assign("COSS", "coss", env = HH)
  assign("Mercatox", "mercatox", env = HH)
  assign("Liquid", "liquid", env = HH)
  assign("LakeBTC", "lakebtc", env = HH)
  assign("KuCoin", "kucoin", env = HH)
  assign("Coinone", "coinone", env = HH)
  assign("Tidex", "tidex", env = HH)
  assign("MBAex", "mbaex", env = HH)
  assign("Bitsane", "bitsane", env = HH)
  assign("Coindeal", "coindeal", env = HH)
  assign("Coinsquare", "coinsquare", env = HH)
  assign("Iquant", "iquant", env = HH)
  assign("itBit", "itbit", env = HH)
  assign("Livecoin", "livecoin", env = HH)
  assign("Poloniex", "poloniex", env = HH)
  assign("BTCBOX", "btcbox", env = HH)
  assign("BitMax", "bitmax", env = HH)
  assign("YoBit", "yobit", env = HH)
  assign("Coinhub", "coinhub", env = HH)
  assign("Bitrue", "bitrue", env = HH)
  assign("Exmo", "exmo", env = HH)
  assign("OOOBTC", "ooobtc", env = HH)
  assign("BITBOX", "bitbox", env = HH)
  assign("Gemini", "gemini", env = HH)
  assign("bitFlyer", "bitflyer", env = HH)
  assign("Bitlish", "bitlish", env = HH)
  assign("GDAC", "gdac", env = HH)
  assign("CEX.IO", "cex-io", env = HH)
  assign("Ovis", "ovis", env = HH)
  assign("DSX", "dsx", env = HH)
  assign("Ethfinex", "ethfinex", env = HH)
  assign("STEX", "stex", env = HH)
  assign("BX Thailand", "bx-thailand", env = HH)
  assign("Vebitcoin", "vebitcoin", env = HH)




  listExchanges <- c(
    "binance",
    "okex",
    "zb-com",
    "coinbene",
    "bit-z",
    "bibox",
    "digifinex",
    "bw",
    "lbank",
    "oex",
    "coineal",
    "hitbtc",
    "huobi-global",
    "bitforex",
    "idax",
    "bcex",
    "fatbtc",
    "cryptonex",
    "bitmart",
    "p2bb2b",
    "chaoex",
    "coinsuper",
    "simex",
    "exrates",
    "latoken",
    "dragonex",
    "rightbtc",
    "dobi-exchange",
    "allcoin",
    "bitfinex"
  )

  # Endereço com Simbolos Crypto
  url <- "https://coinmarketcap.com/rankings/exchanges/1"
  url2 <- "https://coinmarketcap.com/rankings/exchanges/2"
  url3 <- "https://coinmarketcap.com/rankings/exchanges/3"
  # url_final <- paste(url,exchange, sep = "")
  reader <- read_html(url)
  saida <- reader %>% html_nodes("table") %>% html_table()
  reader2 <- read_html(url2)
  saida2 <- reader2 %>% html_nodes("table") %>% html_table()
  reader3 <- read_html(url3)
  saida3 <- reader3 %>% html_nodes("table") %>% html_table()
  # Ja precisei alterar esse valor de param uma vez. Anteriormente era 2
  df_saida <- as.data.frame(saida[[1]])
  df_saida <- rbind(df_saida, as.data.frame(saida2[[1]]))
  df_saida <- rbind(df_saida, as.data.frame(saida3[[1]]))
  df_saida
}
