library(shiny)
library(rsconnect)
library(ggplot2)

ui <- fluidPage(
  h2("Effect of reliability of predictors on sample estimate accuracy"),
  br(),
  h3("Author: Eli Clapper"),
  h4(Sys.time()),
  br(),
  
  tabsetPanel(
    tabPanel("Actual App",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "obs", 
                             label = "Choose the sample size",
                             min = 3,
                             max = 1000,
                             step = 1,
                             value = 200),
                 
                 sliderInput(inputId = "variation", 
                             label = "Choose reliability of predictor",
                             min = 0.01,
                             max = 1,
                             value = 0.05),
                 
                 sliderInput(inputId = "beta", 
                             label = "Choose true effect in population",
                             min = 0,
                             max = 1,
                             step = 0.05,
                             value = 0.4)
               ),
               
               mainPanel(
                 fluidRow(
                   column(7, plotOutput(outputId = "Scatterplot")),
                   column(5, tableOutput(outputId = "stats"))
                 )
               )
             ),
             h4("As the reliability of the predictor gets bigger, 
                the closer the sample effect reflects the population effect")
            ),
            
    tabPanel("Surprise me",
             h3("pick a number between 1 and 5"),
             numericInput(inputId = "number", label = "", min = 1, max = 9, step = 1, value = 1),
             textOutput("text")
             )
    
  ),
  
)

server <- function(input, output){
  
  dat <- reactive({
   Predictor <-  rnorm(input$obs, 0, input$variation)
   Outcome <- rnorm(input$obs, input$beta*Predictor, sqrt(1-input$beta^2))
   data.frame(Predictor, Outcome)
    })

  output$Scatterplot <- renderPlot({
    fit <- lm(Outcome ~ Predictor, data = dat())
    ggplot2::ggplot(dat(), aes(x = Predictor, y = Outcome)) +
      geom_point(color = "darkblue") +
      stat_smooth(method = "lm", se = F, color = "red", lwd = 1.5) +
      theme_minimal()
  })
  
  output$stats <- renderTable({
    fit2 <- lm(Outcome ~ Predictor, data = dat())
    b1s <- as.numeric(fit2$coefficients[2])
    statistics <- as.data.frame(cbind(
      effect = c(input$beta, b1s),
      N = rep(input$obs, 2),
      reliability = rep(input$variation, 2)
    ))
    rownames(statistics) = c("population stats", "sample stats")
    statistics
  }, rownames = T)
  
  output$text <- renderText({
    randomFacts <- list(
      one = "Pick something else than the number 1.",
      two = "Nooit zal ik je opgeven, nooit zal ik je teleurstellen, nooit zal ik omdraaien en je verlaten.",
      three = "The crew of Apollo 13 have travelled the furthest from earth than any other being (400,171 kilometers).",
      four = "There is no such thing as the 5-second rule, you are just eating dirty food from the ground.",
      five = "Don't forget to drink at least 2 litres of water today.",
      six = "I told you to pick a number between 1 and 5..",
      seven = "Can you count?",
      eight = "Now you are just being tedious.",
      nine = "Alright, i'll stop working now."
    )
    randomFacts[[input$number]]})
}

shinyApp(ui, server)


