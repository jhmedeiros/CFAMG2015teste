https://github.com/Xiaodan/Coursera-Developing-Data-Products/tree/master/projectlibrary(shiny)
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
                           "12" = "12"))
        
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
            
            tabBox(
                title = NULL,
                side = "right", 
                width = 8,
                selected = "Theorem",
                tabPanel("Variance", 
                         HTML("<p>Also note that the expected variance of the new distribution is also related
                              to the variance of the original distribution (if the number of samples and 
                              simulations are large enough). For the given statistics:</p>"),
                         dataTableOutput(outputId="dTable")),
                tabPanel("Mean", 
                         HTML("<p>Note that the new distribution of means is centered around the mean
                              of the original distribution (if the number of samples and 
                              simulations are large enough). For the given statistics:</p>"),
                         textOutput("originalmean")),
                tabPanel("Experience",
                         HTML("<p>In the present experience, the user may choose a type of
                              distribution, along with its satatistics, depending on the 
                              chosen distribution. The initial distribution is shown in the 
                              preview box, on the left.</p>
                              <p>Then, the user may choose the number of samples within each
                              simulation and the number of total simulations to be used on the
                              experience. The software will generate that number of simulations,
                              each with that number of samples, and then take the mean of each
                              simulation, only to plot the resulting distribution in box on the bottom.</p>
                              <p>As stated before, if the user chooses big enough samples and 
                              simulates a large enough amount of means, the resulting distribution will be gaussian-
                              shaped, regardless of the initial shape of the chosen distribution.</p>")),
                tabPanel("Theorem", 
                         HTML("<p>According to the central limit theorem, 
                              the distribution of means of an independent and identically 
                              distributed variable  assumes the shape of a normal 
                              distribution, as the sample size increases.</p> <p> Therefore, 
                              even though the original distribution of the sample might not 
                              be gaussian-shaped, the distribution of the simulated means will be, 
                              depending on the number of samples in each distribution and on the 
                              number of simulations. The bigger they are set, the more gaussian the 
                              distribution of means will be.</p> <p>Also, the 
                              ditribution of means should present a mean equivalent to 
                              the mean of the underlying distribution and a variance 
                              equivalent to the variance of the underlying distribution 
                              divided by the sample size.</p>"))
                         )
                         ),
        
        fluidRow(
            
            box(title = "Histogram of the Simulations' Means", 
                status = "primary",
                width = 12,
                height = 350,
                solidHeader = TRUE,
                plotOutput("Plot"))
            
        )
        
    )
))


server <- shinyServer(function(input, output) {
    
    dataTable <- reactive({
        
        TESTE_filtro(a, input$area_de_resultado, input$tipo_de_programa)
        
    })
    
    output$dTable <- renderDataTable({
        
        head(dataTable(),10)
        
    })
    
    output$Plot <- renderPlot({
        
        ggplot(dataTable(), aes(x = indicadores_agrupamento)) + 
            geom_bar(colour = "black", 
                           fill = "white", 
                           aes(y = ..count..)) +
            theme_bw() +
            ylab(NULL) + xlab(NULL) + ggtitle(NULL) +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y  = element_blank())
        
        
    }, height = 290)      
    
})


shinyApp(ui = ui, server = server)

## https://github.com/Xiaodan/Coursera-Developing-Data-Products/tree/master/project
