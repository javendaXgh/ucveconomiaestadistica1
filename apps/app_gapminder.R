library(shiny)
library(plotly)
library(dplyr)

gap_2007 <- read.csv('datos/gap_2007.csv')
# para el conjunto de datos gap_2007 que es un subset de gapminder para el año 2007 se
# crea una aplicación shiny con las siguientes característica
# en sidebar hay selector de continente con multiples choices
# en sidebar hay selector multiple de paises segun filtro de continente seleccionado
# aplicación shiny para visualizar histograma sobre el conjunto de datos gap_2007 la variable
# gdpPercap en plotly
# también se muestra gráfico con valores gdpPercap por country
# se representa en el histograma los valores mean y median y se incluye selector para que se visualizen o no

# se muestran valores, mean, median, harmonic mean, mode
# selector para mostrar gráfico de GDP per Capita por país

ui <- fluidPage(
  titlePanel("Análisis de GDP per Capita por País"),
  sidebarLayout(
    sidebarPanel(
      numericInput('bins',
                   label='# bins',
                   value=10,
                   min=5,
                   max=20),
      selectInput("continente",
                  "Selecciona un continente:", 
                  choices = unique(gap_2007$continent), 
                  selected ='Americas', 
                  multiple = TRUE),
      selectInput("paises", "Selecciona países:", 
                  choices = NULL, 
                  selected = NULL, 
                  multiple = TRUE),
      checkboxInput('gr_barra',
                    label= 'Gráfico de GDP per Capita por país',
                    value= FALSE),

      # sliderInput('median',
      #             label= 'media',
      #             
      #             )
      
      uiOutput("paises_ui")
    ),
    mainPanel(
      plotlyOutput("histograma"),
      plotlyOutput("grafico_pais"),
      verbatimTextOutput("valores")
    )
  )
)

server <- function(input, output, session) {
  # Filtrar los países según el continente seleccionado
  observe({
    paises_filtrados <- gap_2007 %>%
      filter(continent %in% input$continente) %>%
      pull(country) %>%
      unique()
    
    updateSelectInput(session, "paises", "Selecciona países:", 
                      choices = paises_filtrados, 
                      selected = paises_filtrados)
  })
  
  # Crear el histograma
  output$histograma <- renderPlotly({
    req(input$paises)
    
    data_filtrada <- gap_2007 %>%
      filter(country %in% input$paises)
    
    p <- plot_ly(data_filtrada, 
                 x = ~gdpPercap, 
                 type = "histogram", 
                 name = "Histograma de GDP per Capita",
                 nbinsx = input$bins) %>%
      layout(title = "Histograma de GDP per Capita",
             xaxis = list(title = "GDP per Capita"),
             yaxis = list(title = "Frecuencia"))
    
    # if (input$mean) {
      p <- p %>% add_lines(x = c( mean(data_filtrada$gdpPercap), 
                            mean(data_filtrada$gdpPercap)), 
                     y = c(0, 10), 
                     name= "promedio",
                     line = list(color = 'red', 
                                 dash = 'dash'))
    # }
    
    # if (input$median) {
      p <- p %>% add_lines(x = c( median(data_filtrada$gdpPercap), 
                                  median(data_filtrada$gdpPercap)), 
                           y = c(0, 10), 
                           name= "mediana",
                           line = list(color = 'orange',
                                       dash = 'dash',
                                       text= "Mediana"))
    # }
    
    p
  })
  
  # Crear gráfico de GDP per Capita por país
  observe({
    if (input$gr_barra) {
      output$grafico_pais <- renderPlotly({
        req(input$paises)
        
        data_filtrada <- gap_2007 %>%
          filter(country %in% input$paises)%>%
          arrange((gdpPercap))
        
        data_filtrada$country <- factor(data_filtrada$country, 
                                        levels = data_filtrada$country)
        
        plot_ly(data_filtrada, x = ~country, y = ~gdpPercap, type = 'bar', 
                name = 'GDP per Capita') %>%
          layout(title = 'GDP per Capita por País',
                 xaxis = list(title = 'País'),
                 yaxis = list(title = 'GDP per Capita'))
      })
    }
  })
  # output$grafico_pais <- renderPlotly({
  #   req(input$paises)
  #   
  #   data_filtrada <- gap_2007 %>%
  #     filter(country %in% input$paises)%>%
  #     arrange((gdpPercap))
  #   
  #   data_filtrada$country <- factor(data_filtrada$country, 
  #                                     levels = data_filtrada$country)
  #   
  #   plot_ly(data_filtrada, x = ~country, y = ~gdpPercap, type = 'bar', 
  #           name = 'GDP per Capita') %>%
  #     layout(title = 'GDP per Capita por País',
  #            xaxis = list(title = 'País'),
  #            yaxis = list(title = 'GDP per Capita'))
  # })
  # Mostrar valores estadísticos
  output$valores <- renderPrint({
    req(input$paises)
    
    data_filtrada <- gap_2007 %>%
      filter(country %in% input$paises)
    
    mean_val <- mean(data_filtrada$gdpPercap)
    median_val <- median(data_filtrada$gdpPercap)
    harmonic_mean_val <- 1 / mean(1 / data_filtrada$gdpPercap)
    mode_val <- as.numeric(names(sort(table(data_filtrada$gdpPercap), decreasing = TRUE)[1]))
    
    cat("Media:", mean_val, "\n")
    cat("Mediana:", median_val, "\n")
    cat("Media armónica:", harmonic_mean_val, "\n")
    cat("Moda:", mode_val, "\n")
  })
}
shinyApp(ui = ui, server = server)
