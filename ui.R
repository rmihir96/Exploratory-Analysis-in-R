#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Compare US heatmaps for Flu data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("graph", "Choose a graph to view:", 
                  choices = c("US Heat Map", "All tweets", "Keyword:Flu",  "Keyword:#flu", "CDC vs Tweets", "flu vs #flu" ))
      #submitButton("Update View")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Graph Viewer", plotOutput("selected_graph"))
    )
  )
)
))
