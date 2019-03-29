# loadData
#User: neo4j
#Senha: b.VmqGQaL0utX1.ZkVrnLqPRwkSMqW9
#https://hobby-dnfknogldadkgbkefdbanpcl.dbs.graphenedb.com:24780/db/data/
# bolt://hobby-dnfknogldadkgbkefdbanpcl.dbs.graphenedb.com:24787

#Login: diptechbr@gmail.com
#Senha: dipt2233.


'%!in%' <- function(x,y)!('%in%'(x,y))

currency <- c(
  "Bitcoin",
  "Ripple",
  "Ethereum",
  "Bitcoin Cash",
  "Tether",
  "Litecoin",
  "Tronix",
  "Stellar",
  "EOS",
  "Binance Coin"
)

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

ex <- getExchanges()
# Por enquanto somente as 10 primeiras exchanges
ex <- ex[c(1:10),]

ex1 <- cbind(ex,listExchanges[1:nrow(ex)])
exchanges <- ex1[,c(2,7,10,11)]
colnames(exchanges) <- c("name","n_markets","launched","alias")


graph = startGraph("https://hobby-dnfknogldadkgbkefdbanpcl.dbs.graphenedb.com:24780/db/data/",
                   username = "neo4j",
                   password = "b.VmqGQaL0utX1.ZkVrnLqPRwkSMqW9")

# graph = startGraph("http://localhost:7474/db/data/",
#                    username="neo4j", password="323232")


## Criando Exchanges
for(i in 1:nrow(exchanges)) {
  createNode(graph,
             "Exchange",
             name=exchanges[i,]$name,
             n_markets=exchanges[i,]$n_markets,
             launched=exchanges[i,]$launched,
             alias=exchanges[i,]$alias
             )

}

ss <- getCryptoSymbols()

ss <- ss[c(1:10),]
ss <- cbind(ss,currency)
## Criando Crypto
for(m in 1:nrow(ss)) {
  createNode(graph,
             "Crypto",
             symbol=ss[m,]$"Symbol",
             name=ss[m,]$"Name",
             currency=ss[m,]$"currency",
             price=ss[m,]$"Price (Intraday)",
             change=ss[m,]$"Change",
             p_change=ss[m,]$"% Change",
             market_cap=ss[m,]$"Market Cap",
             volume1=ss[m,]$"Volume in Currency (Since 0:00 UTC)",
             volume2=ss[m,]$"Volume in Currency (24Hr)",
             total_volume=ss[m,]$"Total Volume All Currencies (24Hr)",
             circulating_supply=ss[m,]$"Circulating Supply"
  )
}


## Criando Relacoes
for(j in 1:nrow(ex1)) {
  exMarket <- getExchangeMarkets(exchange = ex1[j,]$"listExchanges")
  queryExchange = "MATCH (p:Exchange {name:{name}}) RETURN p"
  exchange <- getSingleNode(graph, queryExchange, name = ex1[j,]$"Name")
  queryCrypto = "MATCH (s:Crypto {currency:{currency}}) RETURN s"
  for (k in 1:nrow(exMarket)) {
    if (exMarket[k,]$Currency %!in%  ss$currency) {
         next
    }
    crypto = getSingleNode(graph, queryCrypto, currency = exMarket[k,]$"Currency")
    print(exMarket[k,]$"Currency")
     createRel(exchange, "HAS_MARKET", crypto,
               pair = exMarket[k,]$"Pair",
               volume24h = exMarket[k,]$"Volume (24h)",
               price = exMarket[k,]$"Price",
               p_volume = exMarket[k,]$"Volume (%)",
               category = exMarket[k,]$"Category",
               fee_type = exMarket[k,]$"Fee Type",
               updated = exMarket[k,]$"Updated"
               )
  }
}


