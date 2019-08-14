library(shiny)
library(datasets)
Logged = FALSE;

PASSWORD <- data.frame(Brukernavn = "withr", Passord = "e10adc3949ba59abbe56e057f20f883e")
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  source("www/Login.R",  local = TRUE)
  
  observe({
    if (USER$Logged == TRUE) {
      output$obs <- renderUI({
        sliderInput("obs", "Number of observations:", 
                    min = 10000, max = 90000, 
                    value = 50000, step = 10000)
      })
      
      output$distPlot <- renderPlot({
        dist <- NULL
        dist <- rnorm(input$obs)
        hist(dist, breaks = 100, main = paste("Your password:", input$passwd))
      })
    }
  })
})