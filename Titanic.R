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

#reading data


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Titanic Survival Predictation"),
  plotOutput("survivedPie"),
  

  
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
  
  #Passenger’s sex: check box group
  checkboxGroupInput("checkGroup", label = h3("Passenger's Sex"),
                     choices = list("Female" =1, "Male"=2, "No Informartion"=3),
                     selected = 1),
  
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
  
  # Reactive value để theo dõi trạng thái của nút switch
  switch_status <- reactiveVal("Survived")
  
  # Tính toán số liệu cho biểu đồ vòng "survived" hoặc "died"
  observe({
    if (input$Id014 == TRUE) {
      switch_status("Survived")
    } else {
      switch_status("Died")
    }
  })
  
  # Tính toán số liệu cho biểu đồ vòng "survived" hoặc "died"
  survived_counts <- table(titanic_data$Survived)
  
  # Tạo biểu đồ vòng "survived" hoặc "died"
  output$survivedPie <- renderPlot({
    labels <- c("Died", "Survived")
    colors <- c("red", "blue")
  
    # Chọn dữ liệu dựa trên trạng thái của nút switch
    if (switch_status() == "Survived") {
      survived_counts <- table(titanic_data$Survived)
    } else {
      survived_counts <- table(1 - titanic_data$Survived)
    }
    
    # Tạo biểu đồ vòng
    pie(survived_counts, labels = labels, col = colors, main = "Survival Distribution")
  })
  
  # Tạo biểu đồ phân phối (đối với distPlot)
  output$distPlot <- renderPlot({

  })
}

# Run the application 
shinyApp(ui = ui, server = server)