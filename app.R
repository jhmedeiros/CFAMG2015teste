source("treta.R")

ui <- shinyUI(dashboardPage(
        
        dashboardHeader(title = "CFAMGBH2015_v01", 
                        titleWidth = 230),
        
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
                               "Fisica" = "Fisica")),
                uiOutput("programa_ui")
                
        ),
        
        dashboardBody(tags$style(HTML(".box.box-solid.box-success>.box-header {color:#fff; background:#015d7f}
                                      .box.box-solid.box-success{border-bottom-color:#015d7f; border-left-color:#015d7f; border-right-color:#015d7f; border-top-color:#015d7f}
                                      .nav-tabs-custom .nav-tabs li.active {border-top-color: #015d7f;}")),
                      
                      box(title = "Sub-Acoes",
                          collapsible = TRUE,
                          solidHeader = TRUE,
                          collapsed = TRUE,
                          width = 13,
                          status = "primary",
                          
                          fluidRow(
                                  
                                  box(title = "Estatisticas",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      width = 12,
                                      status = "success",
                                      solidHeader = TRUE,
                                      
                                      fluidRow(
                                              
                                              tabBox(title = NULL, 
                                                     width = 4,
                                                     selected = "Estatisticas1",
                                                     tabPanel("Estatisticas1", 
                                                              htmlOutput(outputId = "Estatisticas1.1")),
                                                     tabPanel("Estatisticas2",
                                                              htmlOutput(outputId = "Estatisticas1.2"))
                                                     
                                              ),
                                              
                                              tabBox(title = NULL,
                                                     side = "left", 
                                                     width = 8,
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
                                                     )
                                                              )
                                  
                                      ),
                          
                          fluidRow(
                                  
                                  box(tags$head(tags$style(".shiny-plot-output{height:25vh !important;}")),
                                      title = "Graficos", 
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      status = "success",
                                      width = 12,
                                      solidHeader = TRUE,
                                      
                                      fluidRow(
                                              
                                              tabBox(title = NULL,
                                                     width = 12,
                                                     side = "left",
                                                     tabPanel("Grafico1",
                                                              plotOutput(outputId = "Plot1.1")),
                                                     tabPanel("Grafico2")
                                              )
                                              
                                      )    
                                      
                                  )
                                  
                          ),
                          
                          fluidRow(
                                  
                                  box(title = "Visualizar:",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      status = "success",
                                      width = 12,
                                      solidHeader = TRUE,
                                      dataTableOutput(outputId="dTable1")
                                  )
                                  
                          )
                          
                          ),
                      
                      box(title = "Programas",
                          collapsible = TRUE,
                          collapsed = TRUE,
                          solidHeader = TRUE,
                          width = 13,
                          status = "primary",
                          
                          fluidRow(
                                  
                                  box(title = "Estatisticas",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      width = 12,
                                      status = "success",
                                      solidHeader = TRUE,
                                      
                                      fluidRow(
                                              
                                              tabBox(title = NULL, 
                                                     width = 4,
                                                     selected = "Estatisticas1",
                                                     tabPanel("Estatisticas1",
                                                              htmlOutput(outputId = "Estatisticas2.1")),
                                                     tabPanel("Estatisticas2",
                                                              htmlOutput(outputId = "Estatisticas2.2"))
                                                     
                                              ),
                                              
                                              tabBox(title = NULL,
                                                     side = "left", 
                                                     width = 8,
                                                     tabPanel("I.Financeiro"),
                                                     tabPanel("I.Fisico"),
                                                     tabPanel("Escalonamento")
                                                     
                                              )
                                      )
                                      
                                  )),
                          
                          fluidRow(
                                  
                                  box(tags$head(tags$style(".shiny-plot-output{height:25vh !important;}")),
                                      title = "Graficos", 
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      status = "success",
                                      width = 12,
                                      solidHeader = TRUE,
                                      
                                      fluidRow(
                                              
                                              tabBox(title = NULL,
                                                     width = 12,
                                                     side = "left",
                                                     tabPanel("Grafico1",
                                                              plotOutput("Plot2.1")),
                                                     tabPanel("Grafico2")
                                              )
                                              
                                      )    
                                      
                                  )
                                  
                          ),
                          
                          fluidRow(
                                  
                                  box(title = "Visualizar:",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      status = "success",
                                      width = 12,
                                      solidHeader = TRUE,
                                      dataTableOutput(outputId="dTable2.1")
                                  )
                                  
                          )
                          
                      ),
                      
                      box(title = "Acoes",
                          collapsible = TRUE,
                          collapsed = TRUE,
                          solidHeader = TRUE,
                          width = 13,
                          status = "primary",
                          
                          fluidRow(
                                  
                                  box(title = "Estatisticas",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      width = 12,
                                      status = "success",
                                      solidHeader = TRUE,
                                      
                                      fluidRow(
                                              
                                              tabBox(title = NULL, 
                                                     width = 4,
                                                     selected = "Estatisticas1",
                                                     tabPanel("Estatisticas1"),
                                                     tabPanel("Estatisticas2")
                                                     
                                              ),
                                              
                                              tabBox(title = NULL,
                                                     side = "left", 
                                                     width = 8,
                                                     tabPanel("I.Financeiro"),
                                                     tabPanel("I.Fisico"),
                                                     tabPanel("Escalonamento")
                                                     
                                              )
                                      )
                                      
                                  )),
                          
                          fluidRow(
                                  
                                  box(tags$head(tags$style(".shiny-plot-output{height:25vh !important;}")),
                                      title = "Graficos", 
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      status = "success",
                                      width = 12,
                                      solidHeader = TRUE,
                                      
                                      fluidRow(
                                              
                                              tabBox(title = NULL,
                                                     width = 12,
                                                     side = "left",
                                                     tabPanel("Grafico1"),
                                                     tabPanel("Grafico2")
                                              )
                                              
                                      )    
                                      
                                  )
                                  
                          ),
                          
                          fluidRow(
                                  
                                  box(title = "Visualizar:",
                                      collapsible = TRUE,
                                      collapsed = TRUE,
                                      status = "success",
                                      width = 12,
                                      solidHeader = TRUE,
                                      dataTableOutput(outputId="dTable3.1")
                                  )
                                  
                          )
                          
                      )
                      
        )
        
))


server <- shinyServer(function(input, output) {
        
        ## renderUI
        
        output$programa_ui <- renderUI({
                
                programas <- sort(unique(dataTable1.1()$programa))
                
                selectInput("programa_escolhido", 
                            "Selecionar o programa a ser analisado:",
                            multiple = TRUE,
                            selected = programas,
                            c(programas))
                
        })
        
        
        ## Sub-Acoes
        
        
        dataTable1.1 <- reactive({
                
                TESTE_filtro1.1(a, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida)
                
        })
        
        
        dataTable1.2 <- reactive({
                
                TESTE_filtro1.2(dataTable1.1(), input$programa_escolhido)
                
        })
        
        
        output$dTable1 <- renderDataTable({
                
                dataTable1.2()
                
        }, filter = "bottom")
        
        
        TextoEstatisticas1.1 <- reactive({
                
                TESTE_MinhasTretas1.1(a, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
        })
        
        
        output$Estatisticas1.1 <- renderUI({
                
                TextoEstatisticas1.1()
                
        })
        
        
        TextoEstatisticas1.2 <- reactive({
                
                TESTE_MinhasTretas1.2(a, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
        })
        
        
        output$Estatisticas1.2 <- renderUI({
                
                TextoEstatisticas1.2()
                
        })
        
        
        output$Plot1.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable1.2(), aes(x = relacao_financeira_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable1.2(), aes(x = relacao_fisica_agrupamento)) + 
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
                
        }, height = 270)   
        
        
        ## Programas
        
        
        dataTable2.1 <- reactive({
                
                TESTE_filtro2.1(b, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
        })
        
        
        output$dTable2.1 <- renderDataTable({
                
                dataTable2.1()
                
        })
        
        
        TextoEstatisticas2.1 <- reactive({
                
                
                TESTE_MinhasTretas2.1(b, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
                
        })
        
        
        output$Estatisticas2.1 <- renderUI({
                
                TextoEstatisticas2.1()
                
        })
        
        
        TextoEstatisticas2.2 <- reactive({
                
                
                TESTE_MinhasTretas2.2(b, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
                
        })
        
        
        output$Estatisticas2.2 <- renderUI({
                
                TextoEstatisticas2.2()
                
        })
        
        
        output$Plot2.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable2.1(), aes(x = relacao_financeira_total_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable2.1(), aes(x = relacao_fisica_total_agrupamento)) + 
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
                
        }, height = 270) 
        
        
        ## Acoes
        
        
        dataTable3.1 <- reactive({
                
                TESTE_filtro3.1(c, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
        })
        
        
        output$dTable3.1 <- renderDataTable({
                
                dataTable3.1()
                
        })
        
        
})


shinyApp(ui = ui, server = server)

## https://github.com/Xiaodan/Coursera-Developing-Data-Products/tree/master/project
