#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# define server logic 
server <- function(input, output) {
  output$data <- renderTable({
    Students[, c("Student", input$features), drop = FALSE]
  }, rownames = TRUE)
}
