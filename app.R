source("Treta.R")

ui <- shinyUI(dashboardPage(
        
        dashboardHeader(title = "Teste CFAMGBH - 2015", 
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
                                   c("Cidade Saudavel" = "1",
                                     "Educacao" = "2",
                                     "Cidade com Mobilidade" = "3",
                                     "Cidade Segura" = "4",
                                     "Prosperidade" = "5",
                                     "Modernidade" = "6",
                                     "Todas as Vilas Vivas" = "7",
                                     "Cidade Compartilhada" = "8",
                                     "Cidade Sustentavel" = "9",
                                     "Cidade de Todos" = "10",
                                     "Cultura" = "11",
                                     "Integracao Metropolitana" = "12")),
                radioButtons("dimensao_escolhida",
                             "Selecionar a dimensao a ser analisada:",
                             c("Financeira" = "Financeira",
                               "Fisica" = "Fisica"))
                
        ),
        
        dashboardBody(
                                
                fluidRow(
        
                        box(title = "Estatisticas", 
                        status = "primary",
                        width = 4,
                        height = 300,
                        solidHeader = TRUE,
                        htmlOutput(outputId = "Estatisticas")
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
        
                        box(title = "Escalonamento das Sub-Acoes em Faixas de Avaliacao", 
                        status = "primary",
                        width = 12,
                        height = 350,
                        solidHeader = TRUE,
                        plotOutput("Plot")
                        )
        
                ),

                fluidRow(
        
                        box(title = "Visualizar Sub-Acoes:",
                        status = "primary",
                        width = 12,
                        solidHeader = TRUE,
                        dataTableOutput(outputId="dTable")
                        )
        
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
        
        
        TextoEstatisticas <- reactive({
                
                TESTE_MinhasTretas(a, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida)

        })
        
        
        output$Estatisticas <- renderUI({
                
                TextoEstatisticas()
                
        })
        
        
        output$Plot<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                
                        w <- ggplot(dataTable(), aes(x = relacao_financeira_agrupamento)) + 
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
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        w <- ggplot(dataTable(), aes(x = relacao_fisica_agrupamento)) + 
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
                
                w
                
        }, height = 290)   
        
})


shinyApp(ui = ui, server = server)

## https://github.com/Xiaodan/Coursera-Developing-Data-Products/tree/master/project
