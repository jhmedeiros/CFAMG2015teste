library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid)
options(scipen = 999)

source("treta.R")

ui <- shinyUI(dashboardPage(
    
        dashboardHeader(title = "Teste CFAMGBH", 
                    titleWidth = 550),
    
        dashboardSidebar(
        
                selectInput("ano_dos_dados", 
                    "Dados do ano:", 
                    c("2015" = "2015", 
                      "2016" = "2016", 
                      "2017" = "2017")),
        
                checkboxGroupInput("tipo_de_programa",
                    "Selecionar o(s) tipo(s) de programas:",
                    c("Sustentador" = "Sustentador",
                    "Associado" = "Associado",
                    "Apoio Administrativo" = "Apoio Administrativo")),
        
                checkboxGroupInput("area_de_resultado",
                           "Selecionar a(s) area(s) de resultado:",
                           c("1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5",
                           "6" = "6",
                           "7" = "7",
                           "8" = "8",
                           "9" = "9",
                           "10" = "10",
                           "11" = "11",
                           "12" = "12")),
                
                radioButtons("dimensao_escolhida",
                             "Selecionar a dimensao a ser analisada:",
                             c("Financeira" = "indicadores_agrupamento_Financeira",
                               "Fisica" = "indicadores_agrupamento_Fisica"))
        
        ),
    
        dashboardBody(
                        
                fluidRow(
            
                        box(title = "Distribution Preview", 
                            status = "primary",
                            width = 4,
                            height = 300,
                            solidHeader = TRUE,
                            plotOutput("distPlot")
                        ),
            
                        tabBox(title = NULL,
                               side = "left", 
                               width = 8,
                               selected = "Theorem",
                               tabPanel("I.Financeiro", 
                                        HTML("<center><p>Em primeiro lugar, foi calculada a relacao entre: <br> 
                                             INDICADOR FINANCEIRO = <sup>EXECUCAO FINANCEIRA</sup> &frasl; <sub>PREVISAO FINANCEIRA</sub> <br>
                                             de cada sub-acao, sendo o resultado um INDICADOR FINANCEIRO.
                                             Por exemplo, se uma sub-acao apresenta um INDICADOR FINANCEIRO de 0.5, isso significa que a EXECUCAO FINANCEIRA da mesma representou a metade de sua respectiva PREVISAO FINANCEIRA, ou seja, executou-se apenas a metade do que foi previsto <br>
                                             O ideal, portanto, e um INDICADOR FINANCEIRO de 1, que representa um equilibrio entre o valor previsto e o valor executado.</p></center>")),
                               tabPanel("I.Fisico", 
                                        HTML("<center><p>Em primeiro lugar, foi calculada a relacao entre: <br> 
                                             INDICADOR FISICO = <sup>EXECUCAO FISICA</sup> &frasl; <sub>PREVISAO FISICA</sub> <br>
                                             de cada sub-acao, sendo o resultado um INDICADOR FISICO.
                                             Por exemplo, se uma sub-acao apresenta um INDICADOR FISICO de 2.0, isso significa que a EXECUCAO FISICA da mesma representou o dobro de sua respectiva PREVISAO FISICA, ou seja, executou-se duas vezes o que foi previsto <br>
                                             O ideal, portanto, e um INDICADOR FISICO de 1, que representa um equilibrio entre a meta fisica previsto e a executado.</p></center>"),
                                        textOutput("originalmean")),
                               tabPanel("Escalonamento",
                                        HTML("<center><p>As sub-acoes foram escalonadas em faixas de resultado ilustrativas, a fim de facilitar o entendimento das informacoes. <br>
                                             As faixas mais importantes sao aquelas em que as metas e execucoes estao zeradas. Em seguida, destaca-se a faixa de equilibrio, entre 0.7 e 1.3. As demais faixas apresentam situacoes de desequilibrio, em que a subestimacao ou superestimacao de metas. </p></center>")),
                               tabPanel("Grafico", 
                                        HTML("<center><p>Enfim, foi plotado um grafico que ilustra o numero de sub-acoes em cada faixa de resultado, de acordo com os criterios selecionados pelo usuario (tipo de programa, area de resultado e dimensao analisada).</p></center>"))
                         )
                ),
        
                fluidRow(
            
                        box(title = "Frequencia das Sub-Acoes por Classificacao do Indicador", 
                            status = "primary",
                            width = 12,
                            height = 350,
                            solidHeader = TRUE,
                            plotOutput("Plot"))
            
                ),
                
                fluidRow(
                        
                        box(title = "Visualizar Sub-Acoes:",
                            status = "primary",
                            width = 12,
                            solidHeader = TRUE,
                            dataTableOutput(outputId="dTable"))
                        
                )
                        
        )
))


server <- shinyServer(function(input, output) {
        
        dataTable <- reactive({
        
                
                TESTE_filtro(a, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida)
        
                
        })
    
        
        output$dTable <- renderDataTable({
        
                dataTable()
        
        })
    
        
        output$Plot <- renderPlot({
                
                if(input$dimensao_escolhida == "indicadores_agrupamento_Financeira"){
        
                        ggplot(dataTable(), 
                               aes(x = indicadores_agrupamento_Financeira)) + 
                        geom_bar(colour = "black", 
                                 fill = "white", 
                                 aes(y = ..count..)) +
                        theme_bw() +
                        ylab(NULL) + 
                        xlab(NULL) + 
                        ggtitle(NULL) +
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y  = element_blank())
                        
                }
                
                
                else if(input$dimensao_escolhida == "indicadores_agrupamento_Fisica"){
                        
                        ggplot(dataTable(), 
                               aes(x = indicadores_agrupamento_Fisica)) + 
                                geom_bar(colour = "black", 
                                         fill = "white", 
                                         aes(y = ..count..)) +
                                theme_bw() +
                                ylab(NULL) + 
                                xlab(NULL) + 
                                ggtitle(NULL) +
                                theme(axis.text.y = element_blank(),
                                      axis.ticks.y  = element_blank())
                        
                }
        
        }, height = 290)      
    
})


shinyApp(ui = ui, server = server)

## https://github.com/Xiaodan/Coursera-Developing-Data-Products/tree/master/project
