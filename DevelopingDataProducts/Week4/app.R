#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# set columns that can toggle on/off
columns <- c("Gender", "Major", "GPA")

# read student data from csv
Students <- read.csv("Students.csv")

# create ui input checkboxes and table
ui <- fluidPage(
  checkboxGroupInput("features", "Show:",columns),
  tableOutput("data")
)

# title
titlePanel("Student Information")

# define server logic 
server <- function(input, output) {
  output$data <- renderTable({
    Students[, c("Student", input$features), drop = FALSE]
  }, rownames = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
