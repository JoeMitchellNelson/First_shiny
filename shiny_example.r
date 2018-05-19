library(shiny)
library(ggplot2)
library(tidyverse)
ui <- fluidPage(
  numericInput(inputId="text",
               label="Sample size",
               min=1, 
               max=500000, 
               value=500, 
               step=1),
  
  plotOutput("circle"),
  textOutput("message"),
  textOutput("pi")
)

server <- function(input, output) {
  pointys <- reactive({
    check <- data.frame(x=runif(input$text, min=-1, max=1),y=runif(input$text, min=-1, max=1)) %>%  
      mutate(dist=(x^2+y^2)^(.5)) %>% 
      mutate(in_circle=if_else(dist<1,1,0))
    print(check)
    check
  })
  
  output$message <- renderText({"The estimate of pi is:"})
  output$pi <- renderText({4*(sum(pointys()$in_circle)/input$text)})
  
  output$circle <- renderPlot({
    ggplot(pointys(), aes(x, y)) + 
      geom_point(aes(color=factor(in_circle)), size=if_else(input$text>10000,0.5,2)) +
      theme_minimal() +
      labs(color="In circle", title="Estimating Pi")
  }, height=400, width=450)
}

shinyApp(ui = ui, server = server)


