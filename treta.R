library(dplyr)
library(DT)
options(scipen = 999)

setwd("D:/Users/joao.medeiros/Documents/Teste2015/Data")
a <- read.csv("Planilha Subacoes.csv")
str(a)

TESTE_filtro <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida) {
        
        if(dimensao_escolhida == "Financeira") {
        
        resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                  programa_tipo %in% tipo_de_programa)
        
        resultado <- resultado[, -c(6, 7, 8, 9)]
        
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                          programa_tipo %in% tipo_de_programa)
                
                resultado <- resultado[, -c(10, 11, 12, 13)]
                
        }
        
        print(resultado)
        
}

TESTE_MinhasTretas <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida) {
        
        if(dimensao_escolhida == "Financeira") {
        
                resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                           programa_tipo %in% tipo_de_programa)
                
                resultado <- resultado[, -c(6, 7, 8, 9)]
                
                MFtot <- nrow(resultado)
                MF1 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "0.01 a 0.4", ]),4)
                MF2 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "0.4 a 0.7", ]),4)
                MF3 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "0.7 a 1.3", ]),4)
                MF4 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "1.3 a 2.0", ]),4)
                MF5 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "Acima de 2.0", ]),4)
                MF6 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "Não há execução", ]),4)
                MF7 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "Não há meta", ]),4)
                
                q <- HTML(paste("<center>O total de subações de na AR escolhida é", MFtot, ". Deste total:</br>",
                             MF1, "(ou", round(MF1/MFtot, 4), "do total) estão avaliadas entre 0.01 e 0.4; </br>",
                             MF2, "(ou", round(MF2/MFtot, 4), "do total) estão avaliadas entre 0.4 e 0.7; </br>",
                             MF3, "(ou", round(MF3/MFtot, 4), "do total) estão avaliadas entre 0.7 e 1.3; </br>",
                             MF4, "(ou", round(MF4/MFtot, 4), "do total) estão avaliadas entre 1.3 e 2.0; </br>",
                             MF5, "(ou", round(MF5/MFtot, 4), "do total) estão avaliadas acima de 2.0; </br>",
                             MF6, "(ou", round(MF6/MFtot, 4), "do total) não possuem execução; </br>",
                             MF7, "(ou", round(MF7/MFtot, 4), "do total) não possuem metas.</br>",
                             "Ainda, destaca-se que a meta financeira total dos setores selecionados é", sum(resultado$valor_orcado),
                             "e a execução financeira total dos setores selecionados é", sum(resultado$valor_empenhado),
                             ", resultando num indicador financeiro total de", sum(resultado$valor_empenhado)/sum(resultado$valor_orcado), ".</center>"))
        
        }
        
        else if(dimensao_escolhida == "Fisica") {
        
                
                resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                          programa_tipo %in% tipo_de_programa)
                
                resultado <- resultado[, -c(10, 11, 12, 13)]
                
                MFtot <- nrow(resultado)
                MF1 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "0.01 a 0.4", ]),4)
                MF2 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "0.4 a 0.7", ]),4)
                MF3 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "0.7 a 1.3", ]),4)
                MF4 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "1.3 a 2.0", ]),4)
                MF5 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "Acima de 2.0", ]),4)
                MF6 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "Não há execução", ]),4)
                MF7 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "Não há meta", ]),4)
                
                q <- HTML(paste("<center>O total de subações na AR escolhida é", MFtot, ". Deste total:</br>",
                           MF1, "(ou", round(MF1/MFtot, 4), "do total) estão avaliadas entre 0.01 e 0.4; </br>",
                           MF2, "(ou", round(MF2/MFtot, 4), "do total) estão avaliadas entre 0.4 e 0.7; </br>",
                           MF3, "(ou", round(MF3/MFtot, 4), "do total) estão avaliadas entre 0.7 e 1.3; </br>",
                           MF4, "(ou", round(MF4/MFtot, 4), "do total) estão avaliadas entre 1.3 e 2.0; </br>",
                           MF5, "(ou", round(MF5/MFtot, 4), "do total) estão avaliadas acima de 2.0; </br>",
                           MF6, "(ou", round(MF6/MFtot, 4), "do total) não possuem execução; </br>",
                           MF7, "(ou", round(MF7/MFtot, 4), "do total) não possuem metas.</br>",
                           "Ainda, destaca-se que a meta fisica total dos setores selecionados é", sum(resultado$quantificacao_meta),
                           "e a execução financeira total dos setores selecionados é", sum(resultado$quantificacao_executada),
                           ", resultando num indicador financeiro total de", sum(resultado$quantificacao_executada)/sum(resultado$quantificacao_meta), " (destaca-se que tais valores não são interpretáveis).</center>"))
                       
        }
        
        q
}
