library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
library(dplyr)
options(scipen = 999)

setwd("D:/Trabalho/TCE/Teste API/TesteAPI/data")
a <- read.csv("planilha_2015.csv", sep = ";")
str(a)

TESTE_filtro <- function(a, area_de_resultado, tipo_de_programa) {
    
    resultado <- a %>% filter(area_resultado %in% area_de_resultado,
                              tipo_programa %in% tipo_de_programa)
    
    print(resultado)

}
