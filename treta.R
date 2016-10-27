library(base64enc)
library(grid)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
options(scipen = 999)

#a <- read.csv("./data/Planilha Subacoes.csv")
a <- read.csv("D:/Trabalho/TCE/Teste API/TesteAPI/data/Planilha Subacoes.csv")

levels(a$relacao_fisica_agrupamento) <- c("0.01 a 0.4", "0.4 a 0.7", "0.7 a 1.3", "1.3 a 2.0", "Acima de 2.0", "Nao ha execucao", "Nao ha meta")
levels(a$relacao_financeira_agrupamento) <- c("0.01 a 0.4", "0.4 a 0.7", "0.7 a 1.3", "1.3 a 2.0", "Acima de 2.0", "Nao ha execucao", "Nao ha meta")

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

TESTE_MinhasTretas1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida) {
        
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
                MF6 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "Nao ha execucao", ]),4)
                MF7 <- round(nrow(resultado[resultado$relacao_financeira_agrupamento == "Nao ha meta", ]),4)
                
                q <- HTML(paste("<center>O total de subacoes escolhidas e", MFtot, ". Deste total:</br>",
                                MF1, "(ou", round(MF1/MFtot, 4), "do total) estao avaliadas entre 0.01 e 0.4; </br>",
                                MF2, "(ou", round(MF2/MFtot, 4), "do total) estao avaliadas entre 0.4 e 0.7; </br>",
                                MF3, "(ou", round(MF3/MFtot, 4), "do total) estao avaliadas entre 0.7 e 1.3; </br>",
                                MF4, "(ou", round(MF4/MFtot, 4), "do total) estao avaliadas entre 1.3 e 2.0; </br>",
                                MF5, "(ou", round(MF5/MFtot, 4), "do total) estao avaliadas acima de 2.0; </br>",
                                MF6, "(ou", round(MF6/MFtot, 4), "do total) nao possuem execucao; </br>",
                                MF7, "(ou", round(MF7/MFtot, 4), "do total) nao possuem metas.</br>",
                                "Ainda, destaca-se que a meta financeira total dos setores selecionados e", sum(resultado$valor_orcado),
                                "e a execucao financeira total dos setores selecionados e", sum(resultado$valor_empenhado),
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
                MF6 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "Nao ha execucao", ]),4)
                MF7 <- round(nrow(resultado[resultado$relacao_fisica_agrupamento == "Nao ha meta", ]),4)
                
                q <- HTML(paste("<center>O total de subacoes escolhidas e", MFtot, ". Deste total:</br>",
                                MF1, "(ou", round(MF1/MFtot, 4), "do total) estao avaliadas entre 0.01 e 0.4; </br>",
                                MF2, "(ou", round(MF2/MFtot, 4), "do total) estao avaliadas entre 0.4 e 0.7; </br>",
                                MF3, "(ou", round(MF3/MFtot, 4), "do total) estao avaliadas entre 0.7 e 1.3; </br>",
                                MF4, "(ou", round(MF4/MFtot, 4), "do total) estao avaliadas entre 1.3 e 2.0; </br>",
                                MF5, "(ou", round(MF5/MFtot, 4), "do total) estao avaliadas acima de 2.0; </br>",
                                MF6, "(ou", round(MF6/MFtot, 4), "do total) nao possuem execucao; </br>",
                                MF7, "(ou", round(MF7/MFtot, 4), "do total) nao possuem metas.</br>",
                                "Ainda, destaca-se que a meta fisica total dos setores selecionados e", sum(resultado$quantificacao_meta),
                                "e a execucao financeira total dos setores selecionados e", sum(resultado$quantificacao_executada),
                                ", resultando num indicador financeiro total de", sum(resultado$quantificacao_executada)/sum(resultado$quantificacao_meta), " (destaca-se que tais valores nao sao interpretaveis).</center>"))
                
        }
        
        q
}

TESTE_MinhasTretas2 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida) {
        
        if(dimensao_escolhida == "Financeira") {
                
                resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                          programa_tipo %in% tipo_de_programa)
                
                resultado <- resultado[, -c(6, 7, 8, 9)]
                
                Rtot <- nrow(resultado)
                Atot <- nrow(a)
                METAFINtot <- sum(a$valor_orcado)
                EXECFINtot <- sum(a$valor_empenhado)
                MT2METAFINtot <- sum(resultado$valor_orcado)
                MT2EXECFINtot <- sum(resultado$valor_empenhado)
                
                w <- HTML(paste("<center>O total de subacoes escolhidas e", Rtot, "que corresponde a", Rtot/Atot, "do total de subacoes.</br>",
                                "Destaca-se que a meta financeira total dos setores selecionados e", MT2METAFINtot,
                                "e a execucao financeira total dos setores selecionados e", MT2EXECFINtot,
                                ", resultando num indicador financeiro total de", MT2EXECFINtot/MT2METAFINtot, ".</br>",
                                "Ainda, pode-se ressaltar que a meta fianceira total dos setores selecionados corresponde a", MT2METAFINtot/METAFINtot, "da meta financeira total de", METAFINtot, ". </br>",
                                "Da mesma forma, ressalta-se que a execucao financeira total dos setores selecionados corresponde a", MT2EXECFINtot/EXECFINtot, "da execução financeira total de", EXECFINtot, ". </center>"))
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                
                resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                          programa_tipo %in% tipo_de_programa)
                
                resultado <- resultado[, -c(10, 11, 12, 13)]
                
                Rtot <- nrow(resultado)
                Atot <- nrow(a)
                METAFIStot <- sum(a$quantificacao_meta)
                EXECFIStot <- sum(a$quantificacao_executada)
                MT2METAFIStot <- sum(resultado$quantificacao_meta)
                MT2EXECFIStot <- sum(resultado$quantificacao_executada)
                
                w <- HTML(paste("<center>O total de subacoes escolhidas e", Rtot, "que corresponde a", Rtot/Atot, "do total de subacoes.</br>",
                                "Destaca-se que a meta fisica total dos setores selecionados e", MT2METAFIStot,
                                "e a execucao fisica total dos setores selecionados e", MT2EXECFIStot,
                                ", resultando num indicador fisico total de", MT2EXECFIStot/MT2METAFIStot, ".</br>",
                                "Ainda, pode-se ressaltar que a meta fisica total dos setores selecionados corresponde a", MT2METAFIStot/METAFIStot, "da meta fisica total de", METAFIStot, ". </br>",
                                "Da mesma forma, ressalta-se que a execucao fisica total dos setores selecionados corresponde a", MT2EXECFIStot/EXECFIStot, "da execução fisica total de", EXECFIStot, ". </center>"))
                
        }
        
        w
}

