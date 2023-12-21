#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library('ggplot2') # visualization
library(shinyWidgets) # buttons

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Titanic Survival Predictation"),
  
  #Passenger’s sex: check box group
  checkboxGroupInput("checkGroup", label = h3("Passenger's Sex"),
                     choices = list("Female" =1, "Male"=2, "No Informartion"=3),
                     selected = 1),
  
  # Passenger class: Dropdown-Menü (1st class, 2nd class, 3rd class)
  selectInput("select", label = h3("Passenger's class"),
              choices = list("Class 1"=1, "Class 2"=2, "Class 3" =3)),
  #Survived (1) or died (0)
  switchInput(
    inputId = "Id014",
    onStatus = "success", 
    offStatus = "danger",
    onLabel = "Survived",
    offLabel = "Died"
  ),
  
  #passenger's name
  sliderTextInput(
    inputId = "Id101",
    label = "Passenger's name start with: ", 
    choices = LETTERS,
    selected = c("A", "T"),
    from_min = "A", 
    to_max = "Z"
  ),
  
  

  
  #Number of siblings/spouses aboard
  numericInput("num", label = h3("Number of siblings/spouses aboard"), value = 0),
  
  #Number of parents/children aboard
  numericInput("Number of parents/children", label = h3("Number of parents/children aboard"),value=0),
  
  # Sidebar with a slider input for Passenger’s age
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of ages:",
                  min = 0,
                  max = 100,
                  value = 30)
    ),
  

    
    # Graphs display here
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the age histogram
    hist(x, breaks = bins, col = 'blue', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Something title')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)