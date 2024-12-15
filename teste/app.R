library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)


# Carregar os dados
carregar_dados <- function() {
  tryCatch({
    # Carregar os dados
    dados <- read.csv("health_nutrition_population_statistics.csv", sep = ",")
    
    # Remover espaços em branco nos nomes das colunas e dos países
    names(dados) <- trimws(names(dados))
    dados$Country.Name <- trimws(dados$Country.Name)
    
    # Identificar colunas de anos
    cols_anos <- grep("^X\\d{4}$", names(dados), value = TRUE)
    if (length(cols_anos) == 0) {
      stop("Nenhuma coluna de anos encontrada no dataset.")
    }
    
    # Transformar os dados para formato longo
    dados_long <- dados %>%
      pivot_longer(cols = all_of(cols_anos),
                   names_to = "Ano",
                   values_to = "Valor") %>%
      mutate(Ano = as.integer(sub("X", "", Ano)))  # Remover o prefixo 'X' dos anos e transformar em integer
    
    return(dados_long)
  }, error = function(e) {
    stop("Erro ao carregar os dados: ", e$message)
  })
}

# Função para criar gráfico com base no continente e país
criar_grafico <- function(dados, indicador, pais, continente, periodo) {
  dados_filtrados <- dados %>%
    filter(Indicator.Name == indicador, 
           Country.Name == pais, 
           Continent == continente,
           Ano >= periodo[1], Ano <= periodo[2])
  
  ggplot(dados_filtrados, aes(x = Ano, y = Valor)) +
    geom_line() +
    labs(title = paste("Análise do Indicador:", indicador, "no País:", pais),
         x = "Ano", y = "Valor") +
    theme_minimal()
}

# Função para comparar dois países
criar_grafico_comparacao <- function(dados, indicador, paises, continentes, periodo) {
  dados_filtrados <- dados %>%
    filter(Indicator.Name == indicador, 
           Country.Name %in% paises, 
           Continent %in% continentes,
           Ano >= periodo[1], Ano <= periodo[2])
  
  ggplot(dados_filtrados, aes(x = Ano, y = Valor, color = Country.Name)) +
    geom_line() +
    labs(title = paste("Comparação do Indicador:", indicador, "entre", paises[1], "e", paises[2]),
         x = "Ano", y = "Valor") +
    theme_minimal()
}

# Função para comparar dois períodos
criar_grafico_comparacao_periodos <- function(dados, indicador, pais, periodo1, periodo2) {
  dados_filtrados <- dados %>%
    filter(Indicator.Name == indicador, 
           Country.Name == pais,
           (Ano >= periodo1[1] & Ano <= periodo1[2]) | 
             (Ano >= periodo2[1] & Ano <= periodo2[2]))
  
  ggplot(dados_filtrados, aes(x = Ano, y = Valor, color = as.factor(ifelse(Ano <= periodo1[2], "Período 1", "Período 2")))) +
    geom_line() +
    labs(title = paste("Comparação entre Períodos para o Indicador:", indicador, "no País:", pais),
         x = "Ano", y = "Valor") +
    theme_minimal()
}

# Função para comparar dois indicadores
criar_grafico_comparacao_indicadores <- function(dados, indicadores, pais, periodo) {
  dados_filtrados <- dados %>%
    filter(Indicator.Name %in% indicadores, 
           Country.Name == pais, 
           Ano >= periodo[1], Ano <= periodo[2])
  
  ggplot(dados_filtrados, aes(x = Ano, y = Valor, color = Indicator.Name)) +
    geom_line() +
    labs(title = paste("Comparação dos Indicadores:", paste(indicadores, collapse = " e "), "no País:", pais),
         x = "Ano", y = "Valor") +
    theme_minimal()
}

# Função para análise continental
criar_grafico_continente <- function(dados, indicador, continentes, periodo) {
  dados_filtrados <- dados %>%
    filter(Indicator.Name == indicador, 
           Continent %in% continentes, 
           Ano >= periodo[1], Ano <= periodo[2])
  
  ggplot(dados_filtrados, aes(x = Ano, y = Valor, color = Continent)) +
    geom_line() +
    labs(title = paste("Análise Continental para o Indicador:", indicador),
         x = "Ano", y = "Valor") +
    theme_minimal()
}

# Criar o aplicativo Shiny
ui <- fluidPage(
  titlePanel("Estatísticas Populacionais"),
  tabsetPanel(
    tabPanel("Visualização",
             sidebarLayout(
               sidebarPanel(
                 selectInput("continente", "Selecione o Continente:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), 
                             selected = "Africa"),
                 selectInput("pais", "Selecione o País:",
                             choices = NULL, selected = "Brazil"),
                 selectInput("indicador", "Selecione o Indicador:",
                             choices = NULL, selected = NULL),
                 sliderInput("periodo", "Selecione o Período:",
                             min = 1960, max = 2022, value = c(1960, 2022), step = 1)
               ),
               mainPanel(
                 plotOutput("grafico"),
                 verbatimTextOutput("resumo")
               )
             )
    ),
    tabPanel("Comparação de Países",
             sidebarLayout(
               sidebarPanel(
                 selectInput("continente_comp1", "Selecione o Continente do Primeiro País:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), 
                             selected = "Africa"),
                 selectInput("pais1", "Selecione o Primeiro País:",
                             choices = NULL, selected = "Brazil"),
                 selectInput("continente_comp2", "Selecione o Continente do Segundo País:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), 
                             selected = "Asia"),
                 selectInput("pais2", "Selecione o Segundo País:",
                             choices = NULL, selected = "India"),
                 selectInput("indicador_comp", "Selecione o Indicador para Comparação:",
                             choices = NULL, selected = NULL),
                 sliderInput("periodo_comp", "Selecione o Período:",
                             min = 1960, max = 2022, value = c(1960, 2022), step = 1)
               ),
               mainPanel(
                 plotOutput("grafico_comp")
               )
             )
    ),
    tabPanel("Comparação de Períodos",
             sidebarLayout(
               sidebarPanel(
                 selectInput("continente_periodo", "Selecione o Continente:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                             selected = "Africa"),
                 selectInput("pais_periodo", "Selecione o País:",
                             choices = NULL, selected = "Brazil"),
                 selectInput("indicador_periodo", "Selecione o Indicador para Comparação:",
                             choices = NULL, selected = NULL),
                 sliderInput("periodo1", "Selecione o Primeiro Período:",
                             min = 1960, max = 2022, value = c(1960, 1980), step = 1),
                 sliderInput("periodo2", "Selecione o Segundo Período:",
                             min = 1960, max = 2022, value = c(1981, 2000), step = 1)
               ),
               mainPanel(
                 plotOutput("grafico_periodo")
               )
             )
    ),
    tabPanel("Comparador de Indicador",
             sidebarLayout(
               sidebarPanel(
                 selectInput("continente_indicador", "Selecione o Continente:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                             selected = "Africa"),
                 selectInput("pais_indicador", "Selecione o País:",
                             choices = NULL, selected = "Brazil"),
                 selectInput("indicador1", "Selecione o Primeiro Indicador para Comparação:",
                             choices = NULL, selected = NULL),
                 selectInput("indicador2", "Selecione o Segundo Indicador para Comparação:",
                             choices = NULL, selected = NULL),
                 sliderInput("periodo_indicador", "Selecione o Período:",
                             min = 1960, max = 2022, value = c(1960, 2022), step = 1)
               ),
               mainPanel(
                 plotOutput("grafico_indicador")
               )
             )
    ),
    tabPanel("Análise Continental",
             sidebarLayout(
               sidebarPanel(
                 selectInput("continente_analise", "Selecione o Primeiro Continente:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                             selected = "Africa"),
                 selectInput("continente_comparacao", "Selecione o Segundo Continente:",
                             choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"),
                             selected = "Asia"),
                 selectInput("indicador_continente", "Selecione o Indicador para Análise:",
                             choices = NULL, selected = NULL),
                 sliderInput("periodo_continente", "Selecione o Período:",
                             min = 1960, max = 2022, value = c(1960, 2022), step = 1)
               ),
               mainPanel(
                 plotOutput("grafico_continente")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  dados <- reactive({ carregar_dados() })
  
  observe({
    updateSelectInput(session, "pais", choices = unique(dados()$Country.Name[dados()$Continent == input$continente]))
    updateSelectInput(session, "pais1", choices = unique(dados()$Country.Name[dados()$Continent == input$continente_comp1]))
    updateSelectInput(session, "pais2", choices = unique(dados()$Country.Name[dados()$Continent == input$continente_comp2]))
    updateSelectInput(session, "pais_periodo", choices = unique(dados()$Country.Name[dados()$Continent == input$continente_periodo]))
    updateSelectInput(session, "pais_indicador", choices = unique(dados()$Country.Name[dados()$Continent == input$continente_indicador]))
    updateSelectInput(session, "indicador", choices = unique(dados()$Indicator.Name[dados()$Continent == input$continente]))
    updateSelectInput(session, "indicador_comp", choices = unique(dados()$Indicator.Name[dados()$Continent == input$continente_comp1]))
    updateSelectInput(session, "indicador_periodo", choices = unique(dados()$Indicator.Name[dados()$Continent == input$continente_periodo]))
    updateSelectInput(session, "indicador1", choices = unique(dados()$Indicator.Name[dados()$Continent == input$continente_indicador]))
    updateSelectInput(session, "indicador2", choices = unique(dados()$Indicator.Name[dados()$Continent == input$continente_indicador]))
    updateSelectInput(session, "indicador_continente", choices = unique(dados()$Indicator.Name[dados()$Continent == input$continente_analise]))
  })
  
  output$grafico <- renderPlot({
    req(input$indicador)
    criar_grafico(dados(), input$indicador, input$pais, input$continente, input$periodo)
  })
  
  output$grafico_comp <- renderPlot({
    req(input$indicador_comp)
    criar_grafico_comparacao(dados(), input$indicador_comp, c(input$pais1, input$pais2), c(input$continente_comp1, input$continente_comp2), input$periodo_comp)
  })
  
  output$grafico_periodo <- renderPlot({
    req(input$indicador_periodo)
    criar_grafico_comparacao_periodos(dados(), input$indicador_periodo, input$pais_periodo, input$periodo1, input$periodo2)
  })
  
  output$grafico_indicador <- renderPlot({
    req(input$indicador1, input$indicador2)
    criar_grafico_comparacao_indicadores(dados(), c(input$indicador1, input$indicador2), input$pais_indicador, input$periodo_indicador)
  })
  
  output$grafico_continente <- renderPlot({
    req(input$indicador_continente)
    criar_grafico_continente(dados(), input$indicador_continente, c(input$continente_analise, input$continente_comparacao), input$periodo_continente)
  })
}

shinyApp(ui, server)
