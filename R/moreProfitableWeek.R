##############################################################
# Analise Tecnica como Insumo para Previsoes de Stock Market #
# (c) Rodrigo Almeida - rodrigo.almeida@gmail.com            #
#                                                            #
# Esse codigo tem por objetivo experimentar estrategias de   #
# trading com base no historico de um ativo. E necessario    #
# compreender os objetivos de cada estrategia e selecionar   #
# a melhor estrategia para o ativo.                          #
# IMPORTANTE: O foco são em estratégias do tipo LAGGING      #
##############################################################

#' Avalia semana mais rentavel do período para operação de compra/venda. Analise aritmetica do valor.
#' @param asset  Criptoativo que esta sendo avaliado. Ver metodo getCryptoSymbols para lista.
#' @param dataInicio Data Inicio da Janela de Avaliacao no formato YYYY-MM-DD
#' @param dataFim Data Fim da Janela de Avaliacao no formato YYYY-MM-DD
#' @return Desempenho semanal do cripto ativo
#' @examples
#' moreProfitableWeek("BTC-USD","2018-01-01","2019-01-01")
#' @import quantmod


moreProfitableWeek <- function( asset = NA,
                               startDate = NA,
                               endDate = NA) {

  stockData <- na.omit(getSymbols(asset, src = "yahoo",
                                  from = startDate, to = endDate,
                                  auto.assign = FALSE))

  ret <- weeklyReturn(Cl(stockData),type="arithmetic")

  ret

}
