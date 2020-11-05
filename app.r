library(shiny)

winner = array("Biden", c(2,2,2,2,2))
winner[2,2,2,2,] = "Trump"; 
winner[2,2,1,2,2] = "Trump"; 
winner[2,2,2,1,2] = "Trump"
winner[2,1,2,2,2] = "Tie";

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Election endgame"),
  
  # Show a plot of the generated distribution
  sidebarLayout(
    sidebarPanel("Probability (%) of Biden win in states:",
      sliderInput("pa", "Pennsylvania", 0, 100, 70, 1),
      sliderInput("ga", "Georgia", 0, 100, 64, 1),
      sliderInput("nc", "North Carolina", 0, 100, 14, 1),
      sliderInput("az", "Arizona", 0, 100, 90, 1),
      sliderInput("nv", "Nevada", 0, 100, 90, 1)
    ),
    mainPanel("Probability (%) of Electoral College outcomes:",
      sliderInput("biden", "Biden win", 0, 100, 98.1, 0.1),
      sliderInput("trump", "Trump win", 0, 100, 1.8, 0.1),
      sliderInput("tie", "Tie", 0, 100, 0.2, 0.1)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    
    odds.biden = c(input$pa, input$ga, input$nc, input$az, input$nv)/100
    i = 1; 
    odds = c(odds.biden[i], 1 - odds.biden[i]); 
    for (i in 2:5) odds = odds %o% c(odds.biden[i], 1 - odds.biden[i])
    
    #sum(odds[winner == "Biden"])
    #sum(odds[winner == "Trump"])
    #sum(odds[winner == "Tie"])
    
    updateSliderInput(session, "biden", value = round(100*sum(odds[winner == "Biden"]), 1),
                      min = 0, max = 100, step = 0.1)
    updateSliderInput(session, "trump", value = round(100*sum(odds[winner == "Trump"]), 1),
                      min = 0, max = 100, step = 0.1)
    updateSliderInput(session, "tie", value = round(100*sum(odds[winner == "Tie"]), 1),
                      min = 0, max = 100, step = 0.1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
