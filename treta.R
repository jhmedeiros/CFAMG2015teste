ibrary(base64enc)
library(grid)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
library(DT)
options(scipen = 999)

#a <- read.csv("./data/Planilha Subacoes.csv")
a <- read.csv("D:/Trabalho/TCE/Teste API/TesteAPI/data/Planilha Subacoes.csv")

levels(a$relacao_fisica_agrupamento) <- c("0.01 a 0.4", "0.4 a 0.7", "0.7 a 1.3", "1.3 a 2.0", "Acima de 2.0", "Nao ha execucao", "Nao ha meta")
levels(a$relacao_financeira_agrupamento) <- c("0.01 a 0.4", "0.4 a 0.7", "0.7 a 1.3", "1.3 a 2.0", "Acima de 2.0", "Nao ha execucao", "Nao ha meta")


## Funcoes


# Funcoes Sub-Acoes


TESTE_filtro1.1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida) {
        
        resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                          programa_tipo %in% tipo_de_programa)
        
        if(dimensao_escolhida == "Financeira") {
                
                resultado <- resultado[, -c(6, 7, 8, 9)]
                resultado$valor_orcado <- format_real() (resultado$valor_orcado)
                resultado$valor_empenhado <- format_real() (resultado$valor_empenhado)
                resultado$relacao_financeira <- round(resultado$relacao_financeira, 4)
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                resultado <- resultado[, -c(10, 11, 12, 13)]
                resultado$quantificacao_meta <- format_absoluto() (resultado$quantificacao_meta)
                resultado$quantificacao_executada <- format_absoluto() (resultado$quantificacao_executada)
                resultado$relacao_fisica <- round(resultado$relacao_fisica, 4)
                
        }
        
        print(resultado)
        
}


TESTE_filtro1.2 <- function(a, programa_escolhido) {
        
        
        b <- a %>% filter(programa %in% programa_escolhido)
        
        print(b)
        
        
}



TESTE_MinhasTretas1.1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida, programa_escolhido) {

        a <- a %>% filter(area_resultado %in% area_de_resultado,
                          programa_tipo %in% tipo_de_programa,
                          programa %in% programa_escolhido)
        
        if(dimensao_escolhida == "Financeira") {
                
                MFtot <- nrow(a)
                MF1 <- round(nrow(a[a$relacao_financeira_agrupamento == "0.01 a 0.4", ]),4)
                MF2 <- round(nrow(a[a$relacao_financeira_agrupamento == "0.4 a 0.7", ]),4)
                MF3 <- round(nrow(a[a$relacao_financeira_agrupamento == "0.7 a 1.3", ]),4)
                MF4 <- round(nrow(a[a$relacao_financeira_agrupamento == "1.3 a 2.0", ]),4)
                MF5 <- round(nrow(a[a$relacao_financeira_agrupamento == "Acima de 2.0", ]),4)
                MF6 <- round(nrow(a[a$relacao_financeira_agrupamento == "Nao ha execucao", ]),4)
                MF7 <- round(nrow(a[a$relacao_financeira_agrupamento == "Nao ha meta", ]),4)
                
                q <- HTML(paste("<center>O total de subacoes escolhidas e", MFtot, ". Deste total:</br>",
                                MF1, "(ou", round(MF1/MFtot, 4), "do total) estao avaliadas entre 0.01 e 0.4; </br>",
                                MF2, "(ou", round(MF2/MFtot, 4), "do total) estao avaliadas entre 0.4 e 0.7; </br>",
                                MF3, "(ou", round(MF3/MFtot, 4), "do total) estao avaliadas entre 0.7 e 1.3; </br>",
                                MF4, "(ou", round(MF4/MFtot, 4), "do total) estao avaliadas entre 1.3 e 2.0; </br>",
                                MF5, "(ou", round(MF5/MFtot, 4), "do total) estao avaliadas acima de 2.0; </br>",
                                MF6, "(ou", round(MF6/MFtot, 4), "do total) nao possuem execucao; </br>",
                                MF7, "(ou", round(MF7/MFtot, 4), "do total) nao possuem metas.</br>",
                                "Ainda, destaca-se que a meta financeira total dos setores selecionados e", format_real() (sum(a$valor_orcado)),
                                "e a execucao financeira total dos setores selecionados e", format_real() (sum(a$valor_empenhado)),
                                ", resultando num indicador financeiro total de", round(sum(a$valor_empenhado)/sum(a$valor_orcado), 4), ".</center>"))
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                MFtot <- nrow(a)
                MF1 <- round(nrow(a[a$relacao_fisica_agrupamento == "0.01 a 0.4", ]),4)
                MF2 <- round(nrow(a[a$relacao_fisica_agrupamento == "0.4 a 0.7", ]),4)
                MF3 <- round(nrow(a[a$relacao_fisica_agrupamento == "0.7 a 1.3", ]),4)
                MF4 <- round(nrow(a[a$relacao_fisica_agrupamento == "1.3 a 2.0", ]),4)
                MF5 <- round(nrow(a[a$relacao_fisica_agrupamento == "Acima de 2.0", ]),4)
                MF6 <- round(nrow(a[a$relacao_fisica_agrupamento == "Nao ha execucao", ]),4)
                MF7 <- round(nrow(a[a$relacao_fisica_agrupamento == "Nao ha meta", ]),4)
                
                q <- HTML(paste("<center>O total de subacoes escolhidas e", MFtot, ". Deste total:</br>",
                                MF1, "(ou", round(MF1/MFtot, 4), "do total) estao avaliadas entre 0.01 e 0.4; </br>",
                                MF2, "(ou", round(MF2/MFtot, 4), "do total) estao avaliadas entre 0.4 e 0.7; </br>",
                                MF3, "(ou", round(MF3/MFtot, 4), "do total) estao avaliadas entre 0.7 e 1.3; </br>",
                                MF4, "(ou", round(MF4/MFtot, 4), "do total) estao avaliadas entre 1.3 e 2.0; </br>",
                                MF5, "(ou", round(MF5/MFtot, 4), "do total) estao avaliadas acima de 2.0; </br>",
                                MF6, "(ou", round(MF6/MFtot, 4), "do total) nao possuem execucao; </br>",
                                MF7, "(ou", round(MF7/MFtot, 4), "do total) nao possuem metas.</br>",
                                "Ainda, destaca-se que a meta fisica total dos setores selecionados e", format_absoluto() (sum(a$quantificacao_meta)),
                                "e a execucao fisica total dos setores selecionados e", format_absoluto() (sum(a$quantificacao_executada)),
                                ", resultando num indicador fisico total de", round(sum(a$quantificacao_executada)/sum(a$quantificacao_meta), 4), " (destaca-se que tais valores nao sao interpretaveis).</center>"))
                
        }
        
        q
        
}


TESTE_MinhasTretas1.2 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida, programa_escolhido) {
        
        b <- a
        
        a <- a %>% filter(area_resultado %in% area_de_resultado,
                          programa_tipo %in% tipo_de_programa,
                          programa %in% programa_escolhido)

        if(dimensao_escolhida == "Financeira") {
                
                Atot <- nrow(a)
                Btot1114 <- nrow(b)
                METAFINtot1114 <- sum(b$valor_orcado)
                EXECFINtot1114 <- sum(b$valor_empenhado)
                MT2METAFINtot <- sum(a$valor_orcado)
                MT2EXECFINtot <- sum(a$valor_empenhado)
                
                w <- HTML(paste("<center>O total de subacoes escolhidas e", Atot, "que corresponde a", round(Atot/Btot1114, 4), "do total de subacoes.</br>",
                                "Destaca-se que a meta financeira total dos setores selecionados e", format_real() (MT2METAFINtot),
                                "e a execucao financeira total dos setores selecionados e", format_real() (MT2EXECFINtot),
                                ", resultando num indicador financeiro total de", round(MT2EXECFINtot/MT2METAFINtot, 4), ".</br>",
                                "Ainda, pode-se ressaltar que a meta fianceira total dos setores selecionados corresponde a", round(MT2METAFINtot/METAFINtot1114, 4), "da meta financeira total de", format_real() (METAFINtot1114), ". </br>",
                                "Da mesma forma, ressalta-se que a execucao financeira total dos setores selecionados corresponde a", round(MT2EXECFINtot/EXECFINtot1114, 4), "da execucao financeira total de", format_real() (EXECFINtot1114), ". </center>"))
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                
                resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                          programa_tipo %in% tipo_de_programa,
                                          programa %in% programa_escolhido)
                
                resultado <- resultado[, -c(10, 11, 12, 13)]
                
                Atot <- nrow(a)
                Btot1114 <- nrow(b)
                METAFIStot1114 <- sum(b$quantificacao_meta)
                EXECFIStot1114 <- sum(b$quantificacao_executada)
                MT2METAFIStot <- sum(a$quantificacao_meta)
                MT2EXECFIStot <- sum(a$quantificacao_executada)
                
                w <- HTML(paste("<center>O total de subacoes escolhidas e", Atot, "que corresponde a", round(Atot/Btot1114, 4), "do total de subacoes.</br>",
                                "Destaca-se que a meta fisica total dos setores selecionados e", format_absoluto() (MT2METAFIStot),
                                "e a execucao fisica total dos setores selecionados e", format_absoluto() (MT2EXECFIStot),
                                ", resultando num indicador fisico total de", round(MT2EXECFIStot/MT2METAFIStot, 4), ".</br>",
                                "Ainda, pode-se ressaltar que a meta fisica total dos setores selecionados corresponde a", round(MT2METAFIStot/METAFIStot1114, 4), "da meta fisica total de", format_absoluto() (METAFIStot1114), ". </br>",
                                "Da mesma forma, ressalta-se que a execucao fisica total dos setores selecionados corresponde a", round(MT2EXECFIStot/EXECFIStot1114, 4), "da execucao fisica total de", format_absoluto() (EXECFIStot1114), ". </center>"))
                
        }
        
        w
}


# Funcoes Programas


TESTE_filtro2.1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida, programa_escolhido) {
        
        resultado2 <- a %>% filter(area_resultado %in% area_de_resultado,
                                   programa_tipo %in% tipo_de_programa,
                                   programa %in% programa_escolhido)
        
        if(dimensao_escolhida == "Financeira") {
                
                resultado2 <- resultado2[, -c(6, 7, 8, 9)]
                resultado2 <- aggregate(list(resultado2$valor_orcado, resultado2$valor_empenhado), by = list(resultado2$programa), FUN = sum)
                colnames(resultado2) <- c("programa","valor_orcado_total", "valor_empenhado_total")
                resultado2 <- mutate(resultado2, relacao_financeira_total = valor_empenhado_total/valor_orcado_total)
                resultado2$valor_orcado_total <- format_real() (resultado2$valor_orcado_total)
                resultado2$valor_empenhado_total <- format_real() (resultado2$valor_empenhado_total)
                resultado2$relacao_financeira_total <- round(resultado2$relacao_financeira_total, 4)
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                resultado2 <- resultado2[, -c(10, 11, 12, 13)]
                resultado2 <- aggregate(list(resultado2$quantificacao_meta, resultado2$quantificacao_executada), by = list(resultado2$programa), FUN = sum)
                colnames(resultado2) <- c("programa", "meta_fisica_total", "execucao_fisica_total")
                resultado2 <- mutate(resultado2, relacao_fisica_total = execucao_fisica_total/meta_fisica_total)
                resultado2$meta_fisica_total <- format_absoluto() (resultado2$meta_fisica_total)
                resultado2$execucao_fisica_total <- format_absoluto() (resultado2$execucao_fisica_total)
                resultado2$relacao_fisica_total <- round(resultado2$relacao_fisica_total, 4)
                
        }
        
        
print(resultado2)

}
        
       
# Funcoes Geral

 
format_real <- function (prefix = "R$", suffix = "", ..., big.mark = "\\.", negative_parens = FALSE) {
        
        function(x) {
                
                if (length(x) == 0) 
                        
                        return(character())
                
                x <- round_any(x, 0.01)
                
                negative <- !is.na(x) & x < 0
                
                if (negative_parens) {
                        x <- abs(x)
                }
                
                amount <- format(abs(x), nsmall = 2, trim = TRUE, 
                                 big.mark = big.mark, decimal.mark = ",", scientific = FALSE, digits = 1L)
                
                if (negative_parens) {
                        paste0(ifelse(negative, "(", ""), prefix, amount, 
                               suffix, ifelse(negative, ")", ""))
                }
                else {
                        paste0(prefix, ifelse(negative, "-", ""), amount, 
                               suffix)
                }
        }
}


format_absoluto <- function (prefix = "", suffix = "", ..., big.mark = "\\.", negative_parens = FALSE) {
        
        function(x) {
                
                if (length(x) == 0) 
                        
                        return(character())
                
                x <- round_any(x, 0.01)
                
                negative <- !is.na(x) & x < 0
                
                if (negative_parens) {
                        x <- abs(x)
                }
                
                amount <- format(abs(x), nsmall = 0, trim = TRUE, 
                                 big.mark = big.mark, decimal.mark = ",", scientific = FALSE, digits = 1L)
                
                if (negative_parens) {
                        paste0(ifelse(negative, "(", ""), prefix, amount, 
                               suffix, ifelse(negative, ")", ""))
                }
                else {
                        paste0(prefix, ifelse(negative, "-", ""), amount, 
                               suffix)
                }
        }
}
