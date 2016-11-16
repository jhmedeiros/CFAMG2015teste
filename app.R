source("treta.R")

ui <- shinyUI(dashboardPage(  
        
        dashboardHeader(title = "CFAMGBH2015_v01", 
                        titleWidth = 230),
        
        dashboardSidebar(tags$head(
                tags$style(HTML(".sidebar {
                                height: 95vh; overflow-y: auto;
                                }"
               ) # close HTML
                )            # close tags$style
                ),          
               
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
               uiOutput("programa_ui"),
               uiOutput("acao_ui"),
               selectInput("treta_escolhida",
                           "Selecionar a treta a ser analisada:",
                           selected = "0",
                           c("Nenhuma" = "0",
                             "Unidade Orcamentaria" = "1",
                             "Funcao" = "2",
                             "Sub Funcao" = "3")),
               uiOutput("muchas_tretas")
               
               
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
                                  
                                  box(tags$head(tags$style(".shiny-plot-output{height:400px !important;}")),
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
                                                     tabPanel("Grafico2",
                                                              plotOutput(outputId = "Plot1.2"))
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
                                      DT::dataTableOutput(outputId="dTable1")
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
                                                     tabPanel("Grafico2",
                                                              plotOutput("Plot2.2"))
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
                                      DT::dataTableOutput(outputId="dTable2.1")
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
                                                     tabPanel("Grafico1",
                                                              plotOutput(outputId = "Plot3.1")),
                                                     tabPanel("Grafico2",
                                                              plotOutput(outputId = "Plot3.2"))
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
                                      DT::dataTableOutput(outputId="dTable3.1")
                                  )
                                  
                          )
                          
                      ),
                      
                      box(title = "Unidades Orcamentarias",
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
                                                     tabPanel("Grafico1",
                                                              plotOutput(outputId = "Plot4.1")),
                                                     tabPanel("Grafico2",
                                                              plotOutput(outputId = "Plot4.2"))
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
                                      DT::dataTableOutput(outputId="dTable4.1")
                                  )
                                  
                          )
                          
                      ),
                      
                      box(title = "Funcoes",
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
                                                     tabPanel("Grafico1",
                                                              plotOutput(outputId = "Plot5.1")),
                                                     tabPanel("Grafico2",
                                                              plotOutput(outputId = "Plot5.2"))
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
                                      DT::dataTableOutput(outputId="dTable5.1")
                                  )
                                  
                          )
                          
                      ),
                      
                      box(title = "Sub-Funcoes",
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
                                                     tabPanel("Grafico1",
                                                              plotOutput(outputId = "Plot6.1")),
                                                     tabPanel("Grafico2",
                                                              plotOutput(outputId = "Plot6.2"))
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
                                      DT::dataTableOutput(outputId="dTable6.1")
                                  )
                                  
                          )
                          
                      ),
                      
                      box(title = "Sub-Acoes por Produto",
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
                                      DT::dataTableOutput(outputId="dTable7.1")
                                  )
                                  
                          )
                          
                      )
                      
        )
        
        ))


server <- shinyServer(function(input, output, session) {
        
        ## renderUI
        
        output$programa_ui <- renderUI({
                
                programas <- sort(unique(dataTable1.1()$programa))
                
                selectInput("programa_escolhido", 
                            "Selecionar o programa a ser analisado:",
                            multiple = TRUE,
                            selected = programas,
                            c(programas))
                
        })
        
        
        output$acao_ui <- renderUI({
                
                acoes <- sort(unique(dataTable1.2()$acao))
                
                selectInput("acao_escolhida", 
                            "Selecionar a acao a ser analisada:",
                            multiple = TRUE,
                            selected = acoes,
                            c(acoes))
                
        })
        
        
        output$muchas_tretas <- renderUI({
                
                if (input$treta_escolhida == 0){
                        
                        return()
                        
                }
                
                else if(input$treta_escolhida == 1) {
                        
                        UOs <- sort(unique(as.character(dataTable1.3()$nome_uo)))
                        
                        selectInput(inputId = "UO_escolhida", 
                                    label = "Selecionar a unidade orcamentaria a ser analisada:",
                                    multiple = TRUE,
                                    selected = UOs,
                                    c(UOs)) 
                        
                }
                
                else if(input$treta_escolhida == 2) {
                        
                        funcoes <- sort(unique(as.character(dataTable1.3()$funcao)))
                        
                        selectInput(inputId = "FUNCAO_escolhida", 
                                    label = "Selecionar a funcao a ser analisada:",
                                    multiple = TRUE,
                                    selected = funcoes,
                                    c(funcoes)) 
                        
                }
                
                else if(input$treta_escolhida == 3){
                        
                        
                        subfuncoes <- sort(unique(as.character(dataTable1.3()$subfuncao)))
                        
                        selectInput(inputId = "SUB_FUNCAO_escolhida", 
                                    label = "Selecionar a sub funcao a ser analisada:",
                                    multiple = TRUE,
                                    selected = subfuncoes,
                                    c(subfuncoes)) 
                        
                }
                
        })
        
        
        
        ## Sub-Acoes
        
        
        dataTable1.1 <- reactive({
                
                TESTE_filtro1.1(a, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida)
                
        })
        
        
        dataTable1.2 <- reactive({
                
                TESTE_filtro1.2(dataTable1.1(), input$programa_escolhido) 
                
        })
        
        
        dataTable1.3 <- reactive({
                
                TESTE_filtro1.3(dataTable1.2(), input$acao_escolhida)
                
        })
        
        
        dataTable1.4 <- reactive({
                
                TESTE_filtro1.4(dataTable1.3(), input$treta_escolhida, input$UO_escolhida, input$FUNCAO_escolhida, input$SUB_FUNCAO_escolhida)
                
        })
        
        
        output$dTable1 <- DT::renderDataTable({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        a <- formatCurrency(datatable(dataTable1.4(), colnames = c("AR", 
                                                                                   "Programa", 
                                                                                   "Acao", 
                                                                                   "Sub Acao", 
                                                                                   "Valor Orcado", 
                                                                                   "Valor Empenhado", 
                                                                                   "Relacao", 
                                                                                   "Agrupamento", 
                                                                                   "Tipo do Programa", 
                                                                                   "Nome da Sub Acao", 
                                                                                   "Unidade Orcamentaria",
                                                                                   "Funcao",
                                                                                   "Sub Funcao")), 
                                            columns = c("valor_orcado", "valor_empenhado"),
                                            currency = "R$",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica") {
                        
                        a <- formatCurrency(datatable(dataTable1.4(), colnames = c("AR", 
                                                                                   "Programa", 
                                                                                   "Acao", 
                                                                                   "Sub Acao", 
                                                                                   "Meta Fisica", 
                                                                                   "Execucao Fisica", 
                                                                                   "Relacao", 
                                                                                   "Agrupamento", 
                                                                                   "Tipo do Programa", 
                                                                                   "Nome da Sub Acao", 
                                                                                   "Unidade Orcamentaria",
                                                                                   "Funcao",
                                                                                   "Sub Funcao")), 
                                            columns = c("quantificacao_meta", "quantificacao_executada"),
                                            currency = "",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                a
                
        })
        
        
        TextoEstatisticas1.1 <- reactive({
                
                TESTE_MinhasTretas1.1(dataTable1.4(), input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido, input$acao_escolhida)
                
        })
        
        
        output$Estatisticas1.1 <- renderUI({
                
                TextoEstatisticas1.1()
                
        })
        
        
        TextoEstatisticas1.2 <- reactive({
                
                TESTE_MinhasTretas1.2(dataTable1.4(), a0, input$dimensao_escolhida)
                
        })
        
        
        output$Estatisticas1.2 <- renderUI({
                
                TextoEstatisticas1.2()
                
        })
        
        
        output$Plot1.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable1.4(), aes(x = relacao_financeira_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable1.4(), aes(x = relacao_fisica_agrupamento)) + 
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
                
        }, height = 225)
        
        
        output$Plot1.2<- renderPlot({
                
                q = input$dTable1_rows_selected
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- ggplot() +
                                geom_point(data = dataTable1.4(), aes(x = valor_orcado, y = valor_empenhado)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable1.3()$valor_orcado), yend = max(dataTable1.3()$valor_orcado), linetype = 2) +
                                #geom_line(linetype = 2, aes(x = 1:1000000000, y = 1:1000000000)) +
                                #geom_point(data = q, x = q$valor_orcado, y = q$valor_empenhado, colour = "red") +
                                theme_bw() +
                                ylab("Valor Empenhado") + 
                                xlab("Valor Orcado") + 
                                scale_x_continuous(labels = format_real()) +
                                scale_y_continuous(labels = format_real()) +
                                theme()
                        #coord_fixed() +
                        ggtitle(NULL)
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        e <- ggplot(dataTable1.4(), aes(x = quantificacao_meta, y = quantificacao_executada)) +
                                geom_point() +
                                theme_bw() +
                                ylab("Execucao Fisica") + 
                                xlab("Meta Fisica") + 
                                scale_x_continuous(labels = format_absoluto()) +
                                scale_y_continuous(labels = format_absoluto()) +
                                #coord_fixed() +
                                ggtitle(NULL)
                        
                }
                
                e
                
        }, height = 225)   
        
        
        ## Programas
        
        
        dataTable2.1 <- reactive({
                
                TESTE_filtro2.1(b, input$area_de_resultado, input$tipo_de_programa, input$dimensao_escolhida, input$programa_escolhido)
                
        })
        
        
        output$dTable2.1 <- DT::renderDataTable({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        b <- formatCurrency(datatable(dataTable2.1(), colnames = c("AR", "Programa", "Tipo do Programa", "Valor Orcado", "Valor Empenhado", "Relacao", "Agrupamento", "Nome do Programa")), 
                                            columns = c("valor_orcado_total", "valor_empenhado_total"),
                                            currency = "R$ ",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica") {
                        
                        b <- formatCurrency(datatable(dataTable2.1(), colnames = c("AR", "Programa", "Tipo do Programa", "Meta Fisica", "Execucao Fisica", "Relacao", "Agrupamento", "Nome do Programa")), 
                                            columns = c("meta_fisica_total", "execucao_fisica_total"),
                                            currency = "",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                b
                
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
                
        }) 
        
        
        output$Plot2.2<- renderPlot({
                
                q = input$dTable1_rows_selected
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- ggplot() +
                                geom_point(data = dataTable2.1(), aes(x = valor_orcado_total, y = valor_empenhado_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable2.1()$valor_orcado_total), yend = max(dataTable2.1()$valor_orcado_total), linetype = 2) +
                                #geom_line(linetype = 2, aes(x = 1:1000000000, y = 1:1000000000)) +
                                #geom_point(data = q, x = q$valor_orcado, y = q$valor_empenhado, colour = "red") +
                                theme_bw() +
                                ylab("Valor Empenhado") + 
                                xlab("Valor Orcado") + 
                                scale_x_continuous(labels = format_real()) +
                                scale_y_continuous(labels = format_real()) +
                                theme()
                        #coord_fixed() +
                        ggtitle(NULL)
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        e <- ggplot(dataTable2.1(), aes(x = meta_fisica_total, y = execucao_fisica_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable2.1()$meta_fisica_total), yend = max(dataTable2.1()$meta_fisica_total), linetype = 2) +
                                geom_point() +
                                theme_bw() +
                                ylab("Execucao Fisica") + 
                                xlab("Meta Fisica") + 
                                scale_x_continuous(labels = format_absoluto()) +
                                scale_y_continuous(labels = format_absoluto()) +
                                #coord_fixed() +
                                ggtitle(NULL)
                        
                }
                
                e
                
        })
        
        
        ## Acoes
        
        
        dataTable3.1 <- reactive({
                
                TESTE_filtro3.1(c, input$acao_escolhida, input$dimensao_escolhida)
                
        })
        
        
        output$dTable3.1 <- DT::renderDataTable({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        c <- formatCurrency(datatable(dataTable3.1(), 
                                                      colnames = c("ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Sub Acoes", "Acao", 
                                                                   "Valor Orcado", 
                                                                   "Valor Empenhado", 
                                                                   "Relacao", 
                                                                   "Agrupamento", 
                                                                   "Nome da Acao"),
                                                      options = list(columnDefs = list(list(
                                                              targets = c(1,2,3,4),
                                                              render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data.length > 20 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                      "}"))))), 
                                            columns = c("valor_orcado_total", "valor_empenhado_total"),
                                            currency = "R$",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica") {
                        
                        c <- formatCurrency(datatable(dataTable3.1(), 
                                                      colnames = c("ARs",
                                                                   "Programas",
                                                                   "Tipo dos Programas",
                                                                   "Sub Acoes",
                                                                   "Acao",
                                                                   "Meta Fisica",
                                                                   "Execucao Fisica",
                                                                   "Relacao",
                                                                   "Agrupamento",
                                                                   "Nome da Acao"),
                                                      options = list(columnDefs = list(list(
                                                              targets = c(1,2,3,4),
                                                              render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data.length > 20 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                      "}"))))),  
                                            columns = c("meta_fisica_total", "execucao_fisica_total"),
                                            currency = "",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                c
                
        })
        
        
        output$Plot3.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable3.1(), aes(x = relacao_financeira_total_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable3.1(), aes(x = relacao_fisica_total_agrupamento)) + 
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
                
        }) 
        
        
        output$Plot3.2<- renderPlot({
                
                q = input$dTable1_rows_selected
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- ggplot() +
                                geom_point(data = dataTable3.1(), aes(x = valor_orcado_total, y = valor_empenhado_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable3.1()$valor_orcado_total), yend = max(dataTable3.1()$valor_orcado_total), linetype = 2) +
                                #geom_line(linetype = 2, aes(x = 1:1000000000, y = 1:1000000000)) +
                                #geom_point(data = q, x = q$valor_orcado, y = q$valor_empenhado, colour = "red") +
                                theme_bw() +
                                ylab("Valor Empenhado") + 
                                xlab("Valor Orcado") + 
                                scale_x_continuous(labels = format_real()) +
                                scale_y_continuous(labels = format_real()) +
                                theme()
                        #coord_fixed() +
                        ggtitle(NULL)
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        e <- ggplot(dataTable3.1(), aes(x = meta_fisica_total, y = execucao_fisica_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable3.1()$meta_fisica_total), yend = max(dataTable3.1()$meta_fisica_total), linetype = 2) +
                                geom_point() +
                                theme_bw() +
                                ylab("Execucao Fisica") + 
                                xlab("Meta Fisica") + 
                                scale_x_continuous(labels = format_absoluto()) +
                                scale_y_continuous(labels = format_absoluto()) +
                                #coord_fixed() +
                                ggtitle(NULL)
                        
                }
                
                e
                
        })
        
        
        ## UOs
        
        
        dataTable4.1 <- reactive({
                
                TESTE_filtro4.1(d, input$dimensao_escolhida, input$treta_escolhida, input$UO_escolhida)
                
        })
        
        
        output$dTable4.1 <- DT::renderDataTable({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        d <- formatCurrency(datatable(dataTable4.1(), 
                                                      colnames = c("Unidade Orcamentaria",
                                                                   "ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Acoes",
                                                                   "Sub Acoes",
                                                                   "Valor Orcado", 
                                                                   "Valor Empenhado", 
                                                                   "Relacao", 
                                                                   "Agrupamento"),
                                                      options = list(columnDefs = list(list(
                                                              targets = c(2,3,4,5),
                                                              render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data.length > 20 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                      "}"))),
                                                              list(
                                                                      targets = c(1),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 40 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                ))), 
                                            columns = c("valor_orcado_total", "valor_empenhado_total"),
                                            currency = "R$",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica") {
                        
                        d <- formatCurrency(datatable(dataTable4.1(), 
                                                      colnames = c("Unidade Orcamentaria",
                                                                   "ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Acoes",
                                                                   "Sub Acoes",
                                                                   "Meta Fisica",
                                                                   "Execucao Fisica",
                                                                   "Relacao",
                                                                   "Agrupamento"),
                                                      options = list(columnDefs = list(
                                                              list(
                                                                      targets = c(2,3,4,5),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 20 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              ),
                                                              list(
                                                                      targets = c(1),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 40 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              )
                                                      ))
                                        ),  
                                        columns = c("meta_fisica_total", "execucao_fisica_total"),
                                        currency = "",
                                        dec.mark = ",",
                                        mark = "\\.")
                        
                }
                
                d
                
        })
        
        
        output$Plot4.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable4.1(), aes(x = relacao_financeira_total_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable4.1(), aes(x = relacao_fisica_total_agrupamento)) + 
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
                
        }) 
        
        
        output$Plot4.2<- renderPlot({
                
                q = input$dTable1_rows_selected
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- ggplot() +
                                geom_point(data = dataTable4.1(), aes(x = valor_orcado_total, y = valor_empenhado_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable4.1()$valor_orcado_total), yend = max(dataTable4.1()$valor_orcado_total), linetype = 2) +
                                #geom_line(linetype = 2, aes(x = 1:1000000000, y = 1:1000000000)) +
                                #geom_point(data = q, x = q$valor_orcado, y = q$valor_empenhado, colour = "red") +
                                theme_bw() +
                                ylab("Valor Empenhado") + 
                                xlab("Valor Orcado") + 
                                scale_x_continuous(labels = format_real()) +
                                scale_y_continuous(labels = format_real()) +
                                theme()
                        #coord_fixed() +
                        ggtitle(NULL)
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        e <- ggplot(dataTable4.1(), aes(x = meta_fisica_total, y = execucao_fisica_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable4.1()$meta_fisica_total), yend = max(dataTable4.1()$meta_fisica_total), linetype = 2) +
                                geom_point() +
                                theme_bw() +
                                ylab("Execucao Fisica") + 
                                xlab("Meta Fisica") + 
                                scale_x_continuous(labels = format_absoluto()) +
                                scale_y_continuous(labels = format_absoluto()) +
                                #coord_fixed() +
                                ggtitle(NULL)
                        
                }
                
                e
                
        })
        
        
        ## Funcoes
        
        
        dataTable5.1 <- reactive({
                
                TESTE_filtro5.1(e, input$dimensao_escolhida, input$treta_escolhida, input$FUNCAO_escolhida)
                
        })
        
        
        output$dTable5.1 <- DT::renderDataTable({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- formatCurrency(datatable(dataTable5.1(), 
                                                      colnames = c("Funcao",
                                                                   "ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Acoes",
                                                                   "Valor Orcado", 
                                                                   "Valor Empenhado", 
                                                                   "Relacao", 
                                                                   "Agrupamento"),
                                                      options = list(columnDefs = list(list(
                                                              targets = c(2,3,4,5),
                                                              render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data.length > 20 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                      "}"))),
                                                              list(
                                                                      targets = c(1),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 40 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              ))), 
                                            columns = c("valor_orcado_total", "valor_empenhado_total"),
                                            currency = "R$",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica") {
                        
                        e <- formatCurrency(datatable(dataTable5.1(), 
                                                      colnames = c("Funcao",
                                                                   "ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Acoes",
                                                                   "Meta Fisica",
                                                                   "Execucao Fisica",
                                                                   "Relacao",
                                                                   "Agrupamento"),
                                                      options = list(columnDefs = list(
                                                              list(
                                                                      targets = c(2,3,4,5),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 20 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              ),
                                                              list(
                                                                      targets = c(1),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 40 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              )
                                                      ))
                        ),  
                        columns = c("meta_fisica_total", "execucao_fisica_total"),
                        currency = "",
                        dec.mark = ",",
                        mark = "\\.")
                        
                }
                
                e
                
        })
        
        
        output$Plot5.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable5.1(), aes(x = relacao_financeira_total_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable5.1(), aes(x = relacao_fisica_total_agrupamento)) + 
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
                
        }) 
        
        
        output$Plot5.2<- renderPlot({
                
                q = input$dTable1_rows_selected
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- ggplot() +
                                geom_point(data = dataTable5.1(), aes(x = valor_orcado_total, y = valor_empenhado_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable5.1()$valor_orcado_total), yend = max(dataTable5.1()$valor_orcado_total), linetype = 2) +
                                #geom_line(linetype = 2, aes(x = 1:1000000000, y = 1:1000000000)) +
                                #geom_point(data = q, x = q$valor_orcado, y = q$valor_empenhado, colour = "red") +
                                theme_bw() +
                                ylab("Valor Empenhado") + 
                                xlab("Valor Orcado") + 
                                scale_x_continuous(labels = format_real()) +
                                scale_y_continuous(labels = format_real()) +
                                theme()
                        #coord_fixed() +
                        ggtitle(NULL)
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        e <- ggplot(dataTable5.1(), aes(x = meta_fisica_total, y = execucao_fisica_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable5.1()$meta_fisica_total), yend = max(dataTable5.1()$meta_fisica_total), linetype = 2) +
                                geom_point() +
                                theme_bw() +
                                ylab("Execucao Fisica") + 
                                xlab("Meta Fisica") + 
                                scale_x_continuous(labels = format_absoluto()) +
                                scale_y_continuous(labels = format_absoluto()) +
                                #coord_fixed() +
                                ggtitle(NULL)
                        
                }
                
                e
                
        })
        
        
        ## Sub Funcoes
        
        
        dataTable6.1 <- reactive({
                
                TESTE_filtro6.1(f, input$dimensao_escolhida, input$treta_escolhida, input$SUB_FUNCAO_escolhida)
                
        })
        
        
        output$dTable6.1 <- DT::renderDataTable({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- formatCurrency(datatable(dataTable6.1(), 
                                                      colnames = c("Sub Funcao",
                                                                   "Funcoes",
                                                                   "ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Acoes",
                                                                   "Valor Orcado", 
                                                                   "Valor Empenhado", 
                                                                   "Relacao", 
                                                                   "Agrupamento"),
                                                      options = list(columnDefs = list(list(
                                                              targets = c(2,3,4,5,6),
                                                              render = JS(
                                                                      "function(data, type, row, meta) {",
                                                                      "return type === 'display' && data.length > 20 ?",
                                                                      "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                      "}"))),
                                                              list(
                                                                      targets = c(1),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 40 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              ))), 
                                            columns = c("valor_orcado_total", "valor_empenhado_total"),
                                            currency = "R$",
                                            dec.mark = ",",
                                            mark = "\\.")
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica") {
                        
                        e <- formatCurrency(datatable(dataTable6.1(), 
                                                      colnames = c("Sub Funcao",
                                                                   "Funcoes",
                                                                   "ARs",
                                                                   "Programas", 
                                                                   "Tipo dos Programas", 
                                                                   "Acoes",
                                                                   "Meta Fisica",
                                                                   "Execucao Fisica",
                                                                   "Relacao",
                                                                   "Agrupamento"),
                                                      options = list(columnDefs = list(
                                                              list(
                                                                      targets = c(2,3,4,5,6),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 20 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 20) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              ),
                                                              list(
                                                                      targets = c(1),
                                                                      render = JS(
                                                                              "function(data, type, row, meta) {",
                                                                              "return type === 'display' && data.length > 40 ?",
                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                                              "}"
                                                                      )
                                                              )
                                                      ))
                        ),  
                        columns = c("meta_fisica_total", "execucao_fisica_total"),
                        currency = "",
                        dec.mark = ",",
                        mark = "\\.")
                        
                }
                
                e
                
        })
        
        
        output$Plot6.1<- renderPlot({
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        w <- ggplot(dataTable6.1(), aes(x = relacao_financeira_total_agrupamento)) + 
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
                        
                        w <- ggplot(dataTable6.1(), aes(x = relacao_fisica_total_agrupamento)) + 
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
                
        }) 
        
        
        output$Plot6.2<- renderPlot({
                
                q = input$dTable1_rows_selected
                
                if(input$dimensao_escolhida == "Financeira") {
                        
                        e <- ggplot() +
                                geom_point(data = dataTable6.1(), aes(x = valor_orcado_total, y = valor_empenhado_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable6.1()$valor_orcado_total), yend = max(dataTable6.1()$valor_orcado_total), linetype = 2) +
                                #geom_line(linetype = 2, aes(x = 1:1000000000, y = 1:1000000000)) +
                                #geom_point(data = q, x = q$valor_orcado, y = q$valor_empenhado, colour = "red") +
                                theme_bw() +
                                ylab("Valor Empenhado") + 
                                xlab("Valor Orcado") + 
                                scale_x_continuous(labels = format_real()) +
                                scale_y_continuous(labels = format_real()) +
                                theme()
                        #coord_fixed() +
                        ggtitle(NULL)
                        
                }
                
                else if(input$dimensao_escolhida == "Fisica"){
                        
                        e <- ggplot(dataTable6.1(), aes(x = meta_fisica_total, y = execucao_fisica_total)) +
                                annotate("segment", x = 0, y = 0, xend = max(dataTable6.1()$meta_fisica_total), yend = max(dataTable6.1()$meta_fisica_total), linetype = 2) +
                                geom_point() +
                                theme_bw() +
                                ylab("Execucao Fisica") + 
                                xlab("Meta Fisica") + 
                                scale_x_continuous(labels = format_absoluto()) +
                                scale_y_continuous(labels = format_absoluto()) +
                                #coord_fixed() +
                                ggtitle(NULL)
                        
                }
                
                e
                
        })
        
        
        ## Produto
        
        
        dataTable7.1 <- reactive({
                
                TESTE_filtro7.1(g, dataTable1.4(), input$area_de_resultado, input$acao_escolhida, input$programa_escolhido)
                
        })
        
        
        output$dTable7.1 <- DT::renderDataTable({
                        
                e <- datatable(dataTable7.1(),
                               colnames = c("Nome",
                                            "AR",
                                            "Programa",
                                            "Acao", 
                                            "Sub-Acao", 
                                            "Produto da Sub-Acao",
                                            "Valor Orcado",
                                            "Meta Fisica",
                                            "Preco Previsto",
                                            "Valor Empenhado",
                                            "Execucao Fisica",
                                            "Preco Executado"),
                               options = list(columnDefs = list(
                                       
                                       list(
                                               targets = c(1),
                                               render = JS(
                                                       "function(data, type, row, meta) {",
                                                       "return type === 'display' && data.length > 40 ?",
                                                       "'<span title=\"' + data + '\">' + data.substr(0, 40) + ' (...)</span>' : data;",
                                                       "}"
                                               )
                                       )
                                       
                               ))) %>%
                        
                        formatCurrency(c("quantificacao_meta", "quantificacao_executada"),
                                       currency = "",
                                       dec.mark = ",",
                                       mark = "\\.") %>%
                        
                        formatCurrency(c("valor_orcado", "valor_empenhado", "preco_previsto", "preco_executado"),
                                       currency = "R$",
                                       dec.mark = ",",
                                       mark = "\\.") %>%
                        
                        formatStyle(c("preco_previsto","preco_executado"), fontWeight = "bold")
                        
                       
                e
                
        })
        
})


shinyApp(ui = ui, server = server)

## https://github.com/Xiaodan/Coursera-Developing-Data-Products/tree/master/project
## https://rstudio.github.io/DT/shiny.html
