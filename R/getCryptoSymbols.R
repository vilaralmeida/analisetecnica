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

getCryptoSymbols <- function() {
  # Endereço com Simbolos Crypto
  # url <- "https://finance.yahoo.com/cryptocurrencies/"
  url <- "https://coinmarketcap.com/"
  reader <- read_html(url)
  saida <- reader %>% html_nodes("table") %>% html_table()
  # Ja precisei alterar esse valor de param uma vez. Anteriormente era 2
  df_saida <- as.data.frame(saida[[1]])
  #df_saida <- df_saida[,c(1,2)]
 # colnames(df_saida) <- c("symbol","definition")
  df_saida
}
