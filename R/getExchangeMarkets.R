##############################################################
# BestCoins.tech - Solução de Analise Tecnica de CryptoAtivos #
# (c) Rodrigo Almeida - rodrigo.almeida@gmail.com             #
##############################################################


#'
#' Recupera a lista de Cryptoativos com a relação USD avaliada no site Yahoo Finance
#' @return dataframe com as informações dos símbolos dos criptoativos e seu significado
#' @examples
#' getCryptoSymbols()
#' @import rvest

getExchangeMarkets <- function(exchange = NA) {
  # Endereço com Simbolos Crypto
  url <- "https://coinmarketcap.com/exchanges/"
  url_final <- paste(url,exchange, sep = "")
  reader <- read_html(url_final)
  saida <- reader %>% html_nodes("table") %>% html_table()
  # Ja precisei alterar esse valor de param uma vez. Anteriormente era 2
  df_saida <- as.data.frame(saida[[1]])
  df_saida
}
