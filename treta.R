library(base64enc)
library(grid)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
library(DT)
library(gtools)
library(scales)
options(scipen = 999)


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


tretosa <- function(x){
        
        paste(unique(x), collapse = ", ")
        
}


tretosa2 <- function(x){
        
        sum(as.numeric(x))
        
}


## Ler e Preparar Planilhas

# Planilha Bruta


#a0 <- read.csv("D:/Trabalho/TCE/Teste API/TesteAPI/data/Planilha Subacoes2.csv")
#a0 <- read.csv("D:/Users/joao.medeiros/Documents/Teste2015/data/Planilha Subacoes2.csv")
a0 <- read.csv("./data/Planilha Subacoes2.csv")

levels(a0$relacao_fisica_agrupamento) <- c("0.01 a 0.4", "0.4 a 0.7", "0.7 a 1.3", "1.3 a 2.0", "Acima de 2.0", "Nao ha execucao", "Nao ha meta")
levels(a0$relacao_financeira_agrupamento) <- c("0.01 a 0.4", "0.4 a 0.7", "0.7 a 1.3", "1.3 a 2.0", "Acima de 2.0", "Nao ha execucao", "Nao ha meta")

a0$X <- NULL

# Planilha Sub-Acoes

a <- a0[, c(1:12, 14, 20, 15, 16, 17)]


# Planilha Programas

b <- aggregate(list(a0$valor_orcado, 
                    a0$valor_empenhado, 
                    a0$quantificacao_meta, 
                    a0$quantificacao_executada),
               by = list(a0$programa),
               FUN = sum)


colnames(b) <- c("programa", "valor_orcado_total", "valor_empenhado_total","meta_fisica_total", "execucao_fisica_total")

barea_resultado <- aggregate(list(a0$area_resultado, 
                                  a0$programa_tipo,
                                  a0$nome_programa),
                             by = list(a0$programa),
                             FUN = unique)

colnames(barea_resultado) <- c("programa", "area_resultado", "programa_tipo", "nome_programa")

b <- merge(b, barea_resultado)

b <- mutate(b, relacao_financeira_total = valor_empenhado_total/valor_orcado_total, relacao_fisica_total = execucao_fisica_total/meta_fisica_total)

intervals <- c(-1.1,-0.9, 0.0001, 0.4001, 0.7001, 1.30001, 2.00001, 500)

b$relacao_financeira_total_agrupamento <- cut(b$relacao_financeira_total, 
                                              breaks = intervals,
                                              labels = c("Nao ha meta",
                                                         "Nao ha execucao",
                                                         "0.01 a 0.4", 
                                                         "0.4 a 0.7", 
                                                         "0.7 a 1.3", 
                                                         "1.3 a 2.0", 
                                                         "Acima de 2.0"))

b$relacao_fisica_total[is.nan(b$relacao_fisica_total)] <- -1

b$relacao_fisica_total_agrupamento <- cut(b$relacao_fisica_total, 
                                          breaks = intervals,
                                          labels = c("Nao ha meta",
                                                     "Nao ha execucao",
                                                     "0.01 a 0.4", 
                                                     "0.4 a 0.7", 
                                                     "0.7 a 1.3", 
                                                     "1.3 a 2.0", 
                                                     "Acima de 2.0"))

b <- b[, c(6, 1, 7, 2, 3, 9, 11, 4, 5, 10, 12, 8)]


# Planilha Acoes

c.1 <- a0

c <- aggregate(list(c.1$valor_orcado, 
                    c.1$valor_empenhado, 
                    c.1$quantificacao_meta, 
                    c.1$quantificacao_executada),
               by = list(c.1$acao),
               FUN = sum)

colnames(c) <- c("acao", "valor_orcado_total", "valor_empenhado_total","meta_fisica_total", "execucao_fisica_total")

carea_resultado <- aggregate(list(c.1$area_resultado,
                                  c.1$programa,
                                  c.1$programa_tipo,
                                  c.1$nome_acao,
                                  c.1$sub_acao),
                             by = list(c.1$acao),
                             FUN = tretosa)


colnames(carea_resultado) <- c("acao", "area_resultado", "programa", "programa_tipo", "nome_acao", "sub_acoes")

c <- merge(c, carea_resultado)

c <- mutate(c, relacao_financeira_total = valor_empenhado_total/valor_orcado_total, relacao_fisica_total = execucao_fisica_total/meta_fisica_total)

c$relacao_fisica_total[is.nan(c$relacao_fisica_total)] <- -1
c$relacao_financeira_total[is.nan(c$relacao_financeira_total)] <- -1

intervals <- c(-1.1,-0.9, 0.0001, 0.4001, 0.7001, 1.30001, 2.00001, 500)

c$relacao_financeira_total_agrupamento <- cut(c$relacao_financeira_total, 
                                              breaks = intervals,
                                              labels = c("Nao ha meta",
                                                         "Nao ha execucao",
                                                         "0.01 a 0.4", 
                                                         "0.4 a 0.7", 
                                                         "0.7 a 1.3", 
                                                         "1.3 a 2.0", 
                                                         "Acima de 2.0"))

c$relacao_fisica_total_agrupamento <- cut(c$relacao_fisica_total, 
                                          breaks = intervals,
                                          labels = c("Nao ha meta",
                                                     "Nao ha execucao",
                                                     "0.01 a 0.4", 
                                                     "0.4 a 0.7", 
                                                     "0.7 a 1.3", 
                                                     "1.3 a 2.0", 
                                                     "Acima de 2.0"))

c <- c[, c(6, 7, 8, 10, 1, 2, 3, 11, 13, 4, 5, 12, 14, 9)]


# Planilha UO

d.1 <- a0

d <- aggregate(list(d.1$valor_orcado, 
                    d.1$valor_empenhado, 
                    d.1$quantificacao_meta, 
                    d.1$quantificacao_executada),
               by = list(d.1$nome_uo),
               FUN = tretosa2)

colnames(d) <- c("unidade_orcamentaria", "valor_orcado_total", "valor_empenhado_total","meta_fisica_total", "execucao_fisica_total")

darea_resultado <- aggregate(list(d.1$area_resultado,
                                  d.1$programa,
                                  d.1$programa_tipo,
                                  d.1$acao,
                                  d.1$sub_acao),
                             by = list(d.1$nome_uo),
                             FUN = tretosa)


colnames(darea_resultado) <- c("unidade_orcamentaria", "area_resultado", "programa", "programa_tipo", "nome_acao", "sub_acoes")

d <- merge(d, darea_resultado)

d <- mutate(d, relacao_financeira_total = valor_empenhado_total/valor_orcado_total, relacao_fisica_total = execucao_fisica_total/meta_fisica_total)

d$relacao_fisica_total[is.nan(d$relacao_fisica_total)] <- -1
d$relacao_financeira_total[is.nan(d$relacao_financeira_total)] <- -1

intervals <- c(-1.1,-0.9, 0.0001, 0.4001, 0.7001, 1.30001, 2.00001, 500)

d$relacao_financeira_total_agrupamento <- cut(d$relacao_financeira_total, 
                                              breaks = intervals,
                                              labels = c("Nao ha meta",
                                                         "Nao ha execucao",
                                                         "0.01 a 0.4", 
                                                         "0.4 a 0.7", 
                                                         "0.7 a 1.3", 
                                                         "1.3 a 2.0", 
                                                         "Acima de 2.0"))

d$relacao_fisica_total_agrupamento <- cut(d$relacao_fisica_total, 
                                          breaks = intervals,
                                          labels = c("Nao ha meta",
                                                     "Nao ha execucao",
                                                     "0.01 a 0.4", 
                                                     "0.4 a 0.7", 
                                                     "0.7 a 1.3", 
                                                     "1.3 a 2.0", 
                                                     "Acima de 2.0"))

d <- d[, c(1, 6, 7, 8, 9, 10, 2, 3, 11, 13, 4, 5, 12, 14)]


# Planilha Funcoes

e.1 <- a0

e <- aggregate(list(e.1$valor_orcado, 
                    e.1$valor_empenhado, 
                    e.1$quantificacao_meta, 
                    e.1$quantificacao_executada),
               by = list(e.1$funcao),
               FUN = tretosa2)

colnames(e) <- c("funcao", "valor_orcado_total", "valor_empenhado_total","meta_fisica_total", "execucao_fisica_total")

earea_resultado <- aggregate(list(e.1$area_resultado,
                                  e.1$programa,
                                  e.1$programa_tipo,
                                  e.1$acao),
                             by = list(e.1$funcao),
                             FUN = tretosa)


colnames(earea_resultado) <- c("funcao", "area_resultado", "programa", "programa_tipo", "nome_acao")

e <- merge(e, earea_resultado)

e <- mutate(e, relacao_financeira_total = valor_empenhado_total/valor_orcado_total, relacao_fisica_total = execucao_fisica_total/meta_fisica_total)

e$relacao_fisica_total[is.nan(e$relacao_fisica_total)] <- -1
e$relacao_financeira_total[is.nan(e$relacao_financeira_total)] <- -1

intervals <- c(-1.1,-0.9, 0.0001, 0.4001, 0.7001, 1.30001, 2.00001, 500)

e$relacao_financeira_total_agrupamento <- cut(e$relacao_financeira_total, 
                                              breaks = intervals,
                                              labels = c("Nao ha meta",
                                                         "Nao ha execucao",
                                                         "0.01 a 0.4", 
                                                         "0.4 a 0.7", 
                                                         "0.7 a 1.3", 
                                                         "1.3 a 2.0", 
                                                         "Acima de 2.0"))

e$relacao_fisica_total_agrupamento <- cut(e$relacao_fisica_total, 
                                          breaks = intervals,
                                          labels = c("Nao ha meta",
                                                     "Nao ha execucao",
                                                     "0.01 a 0.4", 
                                                     "0.4 a 0.7", 
                                                     "0.7 a 1.3", 
                                                     "1.3 a 2.0", 
                                                     "Acima de 2.0"))

e <- e[, c(1, 6, 7, 8, 9, 2, 3, 10, 12, 4, 5, 11, 13)]


# Planilha Sub-Funcoes

f.1 <- a0

f <- aggregate(list(f.1$valor_orcado, 
                    f.1$valor_empenhado, 
                    f.1$quantificacao_meta, 
                    f.1$quantificacao_executada),
               by = list(f.1$subfuncao),
               FUN = tretosa2)

colnames(f) <- c("sub_funcao", "valor_orcado_total", "valor_empenhado_total","meta_fisica_total", "execucao_fisica_total")

farea_resultado <- aggregate(list(f.1$area_resultado,
                                  f.1$programa,
                                  f.1$programa_tipo,
                                  f.1$acao,
                                  f.1$funcao),
                             by = list(f.1$subfuncao),
                             FUN = tretosa)


colnames(farea_resultado) <- c("sub_funcao", "area_resultado", "programa", "programa_tipo", "acoes", "funcoes")

f <- merge(f, farea_resultado)

f <- mutate(f, relacao_financeira_total = valor_empenhado_total/valor_orcado_total, relacao_fisica_total = execucao_fisica_total/meta_fisica_total)

f$relacao_fisica_total[is.nan(f$relacao_fisica_total)] <- -1
f$relacao_financeira_total[is.nan(f$relacao_financeira_total)] <- -1

intervals <- c(-1.1,-0.9, 0.0001, 0.4001, 0.7001, 1.30001, 2.00001, 500)

f$relacao_financeira_total_agrupamento <- cut(f$relacao_financeira_total, 
                                              breaks = intervals,
                                              labels = c("Nao ha meta",
                                                         "Nao ha execucao",
                                                         "0.01 a 0.4", 
                                                         "0.4 a 0.7", 
                                                         "0.7 a 1.3", 
                                                         "1.3 a 2.0", 
                                                         "Acima de 2.0"))

f$relacao_fisica_total_agrupamento <- cut(f$relacao_fisica_total, 
                                          breaks = intervals,
                                          labels = c("Nao ha meta",
                                                     "Nao ha execucao",
                                                     "0.01 a 0.4", 
                                                     "0.4 a 0.7", 
                                                     "0.7 a 1.3", 
                                                     "1.3 a 2.0", 
                                                     "Acima de 2.0"))

f <- f[, c(1, 10, 6, 7, 8, 9, 2, 3, 11, 13, 4, 5, 12, 14)]

# Planilha Sub Acoes Produto

g <- a0[, c(1:12, 14, 20, 21)]
g <- g[, c(14, 1, 2, 3, 4, 9:12, 5:8, 15)]

g <- mutate(g, preco_previsto = valor_orcado/quantificacao_meta, preco_executado = valor_empenhado/quantificacao_executada)

g$preco_previsto[is.infinite(g$preco_previsto)] <- -2 # ha previsao financeira mas nao tem previsao fisica
g$preco_executado[is.infinite(g$preco_executado)] <- -2 # ha execucao financeira mas nao tem execucao fisica
g$preco_previsto[is.nan(g$preco_previsto)] <- -1
g$preco_executado[is.nan(g$preco_executado)] <- -1

g <- g[, c(1, 2, 3, 4, 5, 14, 6, 10, 15, 7, 11, 16)]

## Funcoes


# Funcoes Sub-Acoes


TESTE_filtro1.1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida) {
        
        resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                                  programa_tipo %in% tipo_de_programa)
        
        if(dimensao_escolhida == "Financeira") {
                
                resultado <- resultado[, -c(5, 6, 7, 8)]
                #ord1 <- resultado$valor_orcado
                #ord2 <- resultado$valor_empenhado
                #resultado$valor_orcado <- factor(resultado$valor_orcado, levels = ord1)
                #resultado$valor_empenhado <- factor(resultado$valor_empenhado, levels = ord2)
                #resultado$valor_orcado <- format_real() (resultado$valor_orcado)
                #resultado$valor_empenhado <- format_real() (resultado$valor_empenhado)
                resultado$relacao_financeira <- round(resultado$relacao_financeira, 4)
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                resultado <- resultado[, -c(9, 10, 11, 12)]
                #ord1 <- mixedsort(format_absoluto() (resultado$quantificacao_meta))
                #ord2 <- mixedsort(format_absoluto() (resultado$quantificacao_executada))
                #resultado$quantificacao_meta <- format_absoluto() (resultado$quantificacao_meta)
                #resultado$quantificacao_executada <- format_absoluto() (resultado$quantificacao_executada)
                #resultado$quantificacao_meta <- factor(resultado$quantificacao_meta, ord1)
                #resultado$quantificacao_executada <- factor(resultado$quantificacao_executada, levels = ord2)
                resultado$relacao_fisica <- round(resultado$relacao_fisica, 4)
                
        }
        
        resultado
        
}


TESTE_filtro1.2 <- function(a, programa_escolhido) {
        
        
        b <- a %>% filter(programa %in% programa_escolhido)
        
        b
        
        
}


TESTE_filtro1.3 <- function(a, acao_escolhida) {
        
        
        b <- a %>% filter(acao %in% acao_escolhida)
        
        b
        
        
}


TESTE_filtro1.4 <- function(a, TRETA_escolhida, UO_escolhida, FUNCAO_escolhida, SUB_FUNCAO_escolhida) {
        
        if(TRETA_escolhida == 0) {
                
                b <- a
                
        }
        
        else if(TRETA_escolhida == 1) {
                
                b <- a %>% filter(nome_uo %in% UO_escolhida)
                
        }
        
      
        
        else if(TRETA_escolhida == 2) {
                
                b <- a %>% filter(funcao %in% FUNCAO_escolhida)
                
        }
        
        else if(TRETA_escolhida == 3) {
                
                b <- a %>% filter(subfuncao %in% SUB_FUNCAO_escolhida)
                
        }
        
        b
}


TESTE_MinhasTretas1.1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida, programa_escolhido, acao_escolhida) {
        
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
                                "Ainda, destaca-se que a meta financeira total dos setores selecionados e", format_real() (sum(as.numeric(a$valor_orcado))),
                                "e a execucao financeira total dos setores selecionados e", format_real() (sum(as.numeric(a$valor_empenhado))),
                                ", resultando num indicador financeiro total de", round(sum(as.numeric(a$valor_empenhado))/sum(as.numeric(a$valor_orcado)), 4), ".</center>"))
                
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


TESTE_MinhasTretas1.2 <- function(a, b, dimensao_escolhida) {
        
        
        if(dimensao_escolhida == "Financeira") {
                
                Atot <- nrow(a)
                Btot1114 <- nrow(b)
                METAFINtot1114 <- sum(as.numeric(b$valor_orcado))
                EXECFINtot1114 <- sum(as.numeric(b$valor_empenhado))
                MT2METAFINtot <- sum(as.numeric(a$valor_orcado))
                MT2EXECFINtot <- sum(as.numeric(a$valor_empenhado))
                
                w <- HTML(paste("<center>O total de subacoes escolhidas e", Atot, "que corresponde a", round(Atot/Btot1114, 4), "do total de subacoes.</br>",
                                "Destaca-se que a meta financeira total dos setores selecionados e", format_real() (MT2METAFINtot),
                                "e a execucao financeira total dos setores selecionados e", format_real() (MT2EXECFINtot),
                                ", resultando num indicador financeiro total de", round(MT2EXECFINtot/MT2METAFINtot, 4), ".</br>",
                                "Ainda, pode-se ressaltar que a meta fianceira total dos setores selecionados corresponde a", round(MT2METAFINtot/METAFINtot1114, 4), "da meta financeira total de", format_real() (METAFINtot1114), ". </br>",
                                "Da mesma forma, ressalta-se que a execucao financeira total dos setores selecionados corresponde a", round(MT2EXECFINtot/EXECFINtot1114, 4), "da execucao financeira total de", format_real() (EXECFINtot1114), ". </center>"))
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                Atot <- nrow(a)
                Btot1114 <- nrow(b)
                METAFIStot1114 <- sum(as.numeric(b$quantificacao_meta))
                EXECFIStot1114 <- sum(as.numeric(b$quantificacao_executada))
                MT2METAFIStot <- sum(as.numeric(a$quantificacao_meta))
                MT2EXECFIStot <- sum(as.numeric(a$quantificacao_executada))
                
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
                
                resultado2 <- resultado2[, -c(8, 9, 10, 11)]
                #resultado2$valor_orcado_total <- format_real() (resultado2$valor_orcado_total)
                #resultado2$valor_empenhado_total <- format_real() (resultado2$valor_empenhado_total)
                resultado2$relacao_financeira_total <- round(resultado2$relacao_financeira_total, 4)
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                resultado2 <- resultado2[, -c(4, 5, 6, 7)]
                #resultado2$meta_fisica_total <- format_absoluto() (resultado2$meta_fisica_total)
                #resultado2$execucao_fisica_total <- format_absoluto() (resultado2$execucao_fisica_total)
                resultado2$relacao_fisica_total <- round(resultado2$relacao_fisica_total, 4)
                
        }
        
        
        print(resultado2)
        
        
}


TESTE_MinhasTretas2.1 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida, programa_escolhido) {
        
        a <- a %>% filter(area_resultado %in% area_de_resultado,
                          programa_tipo %in% tipo_de_programa,
                          programa %in% programa_escolhido)
        
        if(dimensao_escolhida == "Financeira") {
                
                MFtot <- nrow(a)
                MF1 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "0.01 a 0.4", ]),4)
                MF2 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "0.4 a 0.7", ]),4)
                MF3 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "0.7 a 1.3", ]),4)
                MF4 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "1.3 a 2.0", ]),4)
                MF5 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "Acima de 2.0", ]),4)
                MF6 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "Nao ha execucao", ]),4)
                MF7 <- round(nrow(a[a$relacao_financeira_total_agrupamento == "Nao ha meta", ]),4)
                
                q <- HTML(paste("<center>O total de programas escolhidos e", MFtot, ". Deste total:</br>",
                                MF1, "(ou", round(MF1/MFtot, 4), "do total) estao avaliados entre 0.01 e 0.4; </br>",
                                MF2, "(ou", round(MF2/MFtot, 4), "do total) estao avaliados entre 0.4 e 0.7; </br>",
                                MF3, "(ou", round(MF3/MFtot, 4), "do total) estao avaliados entre 0.7 e 1.3; </br>",
                                MF4, "(ou", round(MF4/MFtot, 4), "do total) estao avaliados entre 1.3 e 2.0; </br>",
                                MF5, "(ou", round(MF5/MFtot, 4), "do total) estao avaliados acima de 2.0; </br>",
                                MF6, "(ou", round(MF6/MFtot, 4), "do total) nao possuem execucao; </br>",
                                MF7, "(ou", round(MF7/MFtot, 4), "do total) nao possuem metas.</br>",
                                "Ainda, destaca-se que a meta financeira total dos setores selecionados e", format_real() (sum(a$valor_orcado_total)),
                                "e a execucao financeira total dos setores selecionados e", format_real() (sum(a$valor_empenhado_total)),
                                ", resultando num indicador financeiro total de", round(sum(a$valor_empenhado_total)/sum(a$valor_orcado_total), 4), ".</center>"))
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                MFtot <- nrow(a)
                MF1 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "0.01 a 0.4", ]),4)
                MF2 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "0.4 a 0.7", ]),4)
                MF3 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "0.7 a 1.3", ]),4)
                MF4 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "1.3 a 2.0", ]),4)
                MF5 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "Acima de 2.0", ]),4)
                MF6 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "Nao ha execucao", ]),4)
                MF7 <- round(nrow(a[a$relacao_fisica_total_agrupamento == "Nao ha meta", ]),4)
                
                q <- HTML(paste("<center>O total de programas escolhidos e", MFtot, ". Deste total:</br>",
                                MF1, "(ou", round(MF1/MFtot, 4), "do total) estao avaliados entre 0.01 e 0.4; </br>",
                                MF2, "(ou", round(MF2/MFtot, 4), "do total) estao avaliados entre 0.4 e 0.7; </br>",
                                MF3, "(ou", round(MF3/MFtot, 4), "do total) estao avaliados entre 0.7 e 1.3; </br>",
                                MF4, "(ou", round(MF4/MFtot, 4), "do total) estao avaliados entre 1.3 e 2.0; </br>",
                                MF5, "(ou", round(MF5/MFtot, 4), "do total) estao avaliados acima de 2.0; </br>",
                                MF6, "(ou", round(MF6/MFtot, 4), "do total) nao possuem execucao; </br>",
                                MF7, "(ou", round(MF7/MFtot, 4), "do total) nao possuem metas.</br>",
                                "Ainda, destaca-se que a meta fisica total dos setores selecionados e", format_absoluto() (sum(a$meta_fisica_total)),
                                "e a execucao fisica total dos setores selecionados e", format_absoluto() (sum(a$execucao_fisica_total)),
                                ", resultando num indicador fisico total de", round(sum(a$execucao_fisica_total)/sum(a$meta_fisica_total), 4), " (destaca-se que tais valores nao sao interpretaveis).</center>"))
                
        }
        
        q
        
}


TESTE_MinhasTretas2.2 <- function(a, area_de_resultado, tipo_de_programa, dimensao_escolhida, programa_escolhido) {
        
        b <- a
        
        a <- a %>% filter(area_resultado %in% area_de_resultado,
                          programa_tipo %in% tipo_de_programa,
                          programa %in% programa_escolhido)
        
        if(dimensao_escolhida == "Financeira") {
                
                Atot <- nrow(a)
                Btot1114 <- nrow(b)
                METAFINtot1114 <- sum(as.numeric(b$valor_orcado_total))
                EXECFINtot1114 <- sum(as.numeric(b$valor_empenhado_total))
                MT2METAFINtot <- sum(as.numeric(a$valor_orcado_total))
                MT2EXECFINtot <- sum(as.numeric(a$valor_empenhado_total))
                
                w <- HTML(paste("<center>O total de programas escolhidos e", Atot, "que corresponde a", round(Atot/Btot1114, 4), "do total de programas.</br>",
                                "Destaca-se que a meta financeira total dos setores selecionados e", format_real() (MT2METAFINtot),
                                "e a execucao financeira total dos setores selecionados e", format_real() (MT2EXECFINtot),
                                ", resultando num indicador financeiro total de", round(MT2EXECFINtot/MT2METAFINtot, 4), ".</br>",
                                "Ainda, pode-se ressaltar que a meta fianceira total dos setores selecionados corresponde a", round(MT2METAFINtot/METAFINtot1114, 4), "da meta financeira total de", format_real() (METAFINtot1114), ". </br>",
                                "Da mesma forma, ressalta-se que a execucao financeira total dos setores selecionados corresponde a", round(MT2EXECFINtot/EXECFINtot1114, 4), "da execucao financeira total de", format_real() (EXECFINtot1114), ". </center>"))
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                Atot <- nrow(a)
                Btot1114 <- nrow(b)
                METAFIStot1114 <- sum(as.numeric(b$meta_fisica_total))
                EXECFIStot1114 <- sum(as.numeric(b$execucao_fisica_total))
                MT2METAFIStot <- sum(as.numeric(a$meta_fisica_total))
                MT2EXECFIStot <- sum(as.numeric(a$execucao_fisica_total))
                
                w <- HTML(paste("<center>O total de programas escolhidos e", Atot, "que corresponde a", round(Atot/Btot1114, 4), "do total de programas.</br>",
                                "Destaca-se que a meta fisica total dos setores selecionados e", format_absoluto() (MT2METAFIStot),
                                "e a execucao fisica total dos setores selecionados e", format_absoluto() (MT2EXECFIStot),
                                ", resultando num indicador fisico total de", round(MT2EXECFIStot/MT2METAFIStot, 4), ".</br>",
                                "Ainda, pode-se ressaltar que a meta fisica total dos setores selecionados corresponde a", round(MT2METAFIStot/METAFIStot1114, 4), "da meta fisica total de", format_absoluto() (METAFIStot1114), ". </br>",
                                "Da mesma forma, ressalta-se que a execucao fisica total dos setores selecionados corresponde a", round(MT2EXECFIStot/EXECFIStot1114, 4), "da execucao fisica total de", format_absoluto() (EXECFIStot1114), ". </center>"))
                
        }
        
        w
}


# Funcoes Acoes


TESTE_filtro3.1 <- function(a, acao_escolhida, dimensao_escolhida) {
        
        resultado3 <- a %>% filter(acao %in% acao_escolhida)
        
        #resultado3 <- a
        
        if(dimensao_escolhida == "Financeira") {
                
                resultado3 <- resultado3[, -c(10, 11, 12, 13)]
                #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                resultado3$relacao_financeira_total <- round(resultado3$relacao_financeira_total, 4)
                
        }
        
        else if(dimensao_escolhida == "Fisica") {
                
                resultado3 <- resultado3[, -c(6, 7, 8, 9)]
                #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                resultado3$relacao_fisica_total <- round(resultado3$relacao_fisica_total, 4)
                
        }
        
        
        print(resultado3)
        
        
}


# Funcoes UO, funcoes, sub_funcoes


TESTE_filtro4.1 <- function(a, dimensao_escolhida, TRETA_escolhida, UO_escolhida) {

        if(TRETA_escolhida == 1) {

                resultado4 <- a %>% filter(unidade_orcamentaria %in% UO_escolhida)                
                
                if(dimensao_escolhida == "Financeira") {
                        
                        resultado4 <- resultado4[, -c(11, 12, 13, 14)]
                        #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                        #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                        resultado4$relacao_financeira_total <- round(resultado4$relacao_financeira_total, 4)
                        
                }
                
                else if(dimensao_escolhida == "Fisica") {
                        
                        resultado4 <- resultado4[, -c(7, 8, 9, 10)]
                        #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                        #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                        resultado4$relacao_fisica_total <- round(resultado4$relacao_fisica_total, 4)
                        
                }
                
                print(resultado4)
                
        }
        
        else {
                
                resultado4 <- a
                
                if(dimensao_escolhida == "Financeira") {
                        
                        resultado4 <- resultado4[, -c(11, 12, 13, 14)]
                        #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                        #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                        resultado4$relacao_financeira_total <- round(resultado4$relacao_financeira_total, 4)
                        
                }
                
                else if(dimensao_escolhida == "Fisica") {
                        
                        resultado4 <- resultado4[, -c(7, 8, 9, 10)]
                        #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                        #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                        resultado4$relacao_fisica_total <- round(resultado4$relacao_fisica_total, 4)
                        
                }
                
                print(resultado4)
                
        }
        
        
}


TESTE_filtro5.1 <- function(a, dimensao_escolhida, TRETA_escolhida, FUNCAO_escolhida) {
        
        if(TRETA_escolhida == 2) {
                
                resultado4 <- a %>% filter(funcao %in% FUNCAO_escolhida)
                
                if(dimensao_escolhida == "Financeira") {
                        
                        resultado4 <- resultado4[, -c(10, 11, 12, 13)]
                        #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                        #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                        resultado4$relacao_financeira_total <- round(resultado4$relacao_financeira_total, 4)
                        
                }
                
                else if(dimensao_escolhida == "Fisica") {
                        
                        resultado4 <- resultado4[, -c(6, 7, 8, 9)]
                        #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                        #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                        resultado4$relacao_fisica_total <- round(resultado4$relacao_fisica_total, 4)
                        
                }
                
                print(resultado4)
                
        }
        
        else {
                
                resultado4 <- a
                
                if(dimensao_escolhida == "Financeira") {
                        
                        resultado4 <- resultado4[, -c(10, 11, 12, 13)]
                        #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                        #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                        resultado4$relacao_financeira_total <- round(resultado4$relacao_financeira_total, 4)
                        
                }
                
                else if(dimensao_escolhida == "Fisica") {
                        
                        resultado4 <- resultado4[, -c(6, 7, 8, 9)]
                        #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                        #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                        resultado4$relacao_fisica_total <- round(resultado4$relacao_fisica_total, 4)
                        
                }
                
                print(resultado4)
                
        }
        
}


TESTE_filtro6.1 <- function(a, dimensao_escolhida, TRETA_escolhida, SUB_FUNCAO_escolhida) {
        
        if(TRETA_escolhida == 3) {
                
                resultado4 <- a %>% filter(sub_funcao %in% SUB_FUNCAO_escolhida)
                
                if(dimensao_escolhida == "Financeira") {
                        
                        resultado4 <- resultado4[, -c(11, 12, 13, 14)]
                        #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                        #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                        resultado4$relacao_financeira_total <- round(resultado4$relacao_financeira_total, 4)
                        
                }
                
                else if(dimensao_escolhida == "Fisica") {
                        
                        resultado4 <- resultado4[, -c(7, 8, 9, 10)]
                        #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                        #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                        resultado4$relacao_fisica_total <- round(resultado4$relacao_fisica_total, 4)
                        
                }
                
                print(resultado4)
                
        }
        
        else {
                
                resultado4 <- a
                
                if(dimensao_escolhida == "Financeira") {
                        
                        resultado4 <- resultado4[, -c(11, 12, 13, 14)]
                        #resultado3$valor_orcado_total <- format_real() (resultado3$valor_orcado_total)
                        #resultado3$valor_empenhado_total <- format_real() (resultado3$valor_empenhado_total)
                        resultado4$relacao_financeira_total <- round(resultado4$relacao_financeira_total, 4)
                        
                }
                
                else if(dimensao_escolhida == "Fisica") {
                        
                        resultado4 <- resultado4[, -c(7, 8, 9, 10)]
                        #resultado3$meta_fisica_total <- format_absoluto() (resultado3$meta_fisica_total)
                        #resultado3$execucao_fisica_total <- format_absoluto() (resultado3$execucao_fisica_total)
                        resultado4$relacao_fisica_total <- round(resultado4$relacao_fisica_total, 4)
                        
                }
                
                print(resultado4)
                
        }
        
}


TESTE_filtro7.1 <- function(a, b, area_de_resultado, acao_escolhida, programa_escolhido) {
        
        resultado <- a %>% filter(nome_sub_acao %in% b$nome_sub_acao,
                                  area_resultado %in% area_de_resultado,
                                  acao %in% acao_escolhida,
                                  programa %in% programa_escolhido)
        
        print(resultado)
        
}
