#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(DT)

source("C:/Users/ryand/Dropbox/Sports Analytics/hockey_data/Utah_HC_project/goalie_analysis.R")

goalie_stats <- read.csv("goalie_stats_csv")

head(goalie_stats)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Goalie Comparision"),
    
    sidebarLayout(
      sidebarPanel (
        selectInput(
          inputId = "goalie1",
          label = "Select First Goalie",
          choices = unique(goalie_stats_df$Goalie.Name),
          selected = unique(goalie_stats_df$Goalie.Name)[1]
        ),
        selectInput(
          inputId = "goalie2",
          label =  "Select Second Goalie",
          choices = unique(goalie_stats_df$Goalie.Name),
          selected = unique(goalie_stats_df$Goalie.Name)[2]
        )
      ),
      mainPanel(
        DTOutput("goalieTable")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    selected_data <- reactive({
      goalie_stats_df %>%
        filter(Goalie.Name %in% c(input$goalie1, input$goalie2)) %>%
        select(
          Goalie.Name,
          Goalie.Shots.Against,
          Goalie.Goals.Allowed,
          Goalie.Save..,
          Goalie.PK.Save..,
          Goalie.High.Danger.Shots.Against,
          Goalie.Mid.Range.Shots.Against,
          Goalie.Long.Range.Shots.Against
        )
    })
    output$goalieTable <- renderDT({
      selected_data() %>%
        datatable(
          options =  list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
