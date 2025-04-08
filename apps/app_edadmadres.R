#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(openintro)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Edad Madres"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Cantidad de barras:",
                        min = 5,
                        max = 50,
                        step = 5,
                        value = 15)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      x    <- babies$age[!is.na(babies$age)]
        # print(x[1:10])
        bins <- seq(min(x), 
                    max(x), 
                    length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins, 
             col = 'darkgray', 
             border = 'white',
             xlab = 'Edad Madre',
             main = 'Histograma of de Edades de Madres')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
