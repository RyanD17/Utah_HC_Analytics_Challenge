library(shiny)
library(dplyr)
library(DT)
library(rmarkdown)

goalie_stats <- read.csv("goalie_stats_csv")


metrics_list <- c(
  "Goalie.Games.Played",
  "Goalie.Shots.Against",
  "Goalie.Goals.Allowed",
  "Goalie.Save..",
  "Goalie.PK.Shots.Against",
  "Goalie.PK.Goals.Allowed",
  "Goalie.PK.Save..",
  "Goalie.High.Danger.Shots.Against",
  "Goalie.High.Danger.Goals.Allowed",
  "Goalie.High.Danger.Save..",
  "Goalie.Mid.Range.Shots.Against",
  "Goalie.Mid.Range.Goals.Allowed",
  "Goalie.Mid.Range.Save..",
  "Goalie.Long.Range.Shots.Against",
  "Goalie.Long.Range.Goals.Allowed",
  "Goalie.Long.Range.Save.."
)

ui <- fluidPage(
  titlePanel("Goalie Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "slider", 
        "Number of Games Played",
        min = 1,
        max = 82,
        value = 1
      ),
      selectInput(
        inputId = "goalies",
        label = "Select Goalies",
        choices = unique(goalie_stats$Goalie.Name),
        selected = unique(goalie_stats$Goalie.Name)[1],  # Default to the first goalie if nothing selected
        multiple = TRUE
      ),
      actionButton("select_all", "Select All Goalies"),
      actionButton("remove_all", "Remove All Goalies"),
      
      checkboxGroupInput(
        inputId = "metrics",
        label = "Select Metrics",
        choices = list(
          "Games Played" = "Goalie.Games.Played",
          "Shots Against" = "Goalie.Shots.Against",
          "Goals Allowed" = "Goalie.Goals.Allowed",
          "Save %" = "Goalie.Save..",
          "PK Shots Against" = "Goalie.PK.Shots.Against",
          "PK Goals Allowed" = "Goalie.PK.Goals.Allowed",
          "PK Save %" = "Goalie.PK.Save..",
          "High Danger Shots Against" = "Goalie.High.Danger.Shots.Against",
          "High Danger Goals Allowed" = "Goalie.High.Danger.Goals.Allowed",
          "High Danger Save %" = "Goalie.High.Danger.Save..",
          "Mid Range Shots Against" = "Goalie.Mid.Range.Shots.Against",
          "Mid Range Goals Allowed" = "Goalie.Mid.Range.Goals.Allowed",
          "Mid Range Save %" = "Goalie.Mid.Range.Save..",
          "Long Range Shots Against" = "Goalie.Long.Range.Shots.Against",
          "Long Range Goals Allowed" = "Goalie.Long.Range.Goals.Allowed",
          "Long Range Save %" = "Goalie.Long.Range.Save.."
        ),
        selected = c(
          "Goalie.Games.Played",
          "Goalie.Shots.Against",
          "Goalie.Goals.Allowed",
          "Goalie.Save.."
        ) # Default metrics
      ),
      actionButton("select_all_metrics", "Select All Metrics"),
      actionButton("deselect_all_metrics", "Deselect All Metrics")
    ),
    mainPanel(
      DTOutput("goalieTable")    
    )
  )
)

server <- function(input, output, session) {
  
  
  observeEvent(input$select_all, {
    updateSelectInput(session, "goalies", selected = unique(goalie_stats$Goalie.Name))
  })
  
  observeEvent(input$remove_all, {
    updateSelectInput(session, "goalies", selected = character(0))
  })
  
  observeEvent(input$select_all_metrics, {
    updateCheckboxGroupInput(session, "metrics", selected = metrics_list)
  })
  observeEvent(input$deselect_all_metrics, {
    updateCheckboxGroupInput(session, "metrics", selected = character(0))
  })
  
  selected_data <- reactive({
    # Ensure the selection is not empty
    if (length(input$goalies) == 0 || length(input$metrics) == 0) {
      return(data.frame())
    }
    filtered_data <- goalie_stats[goalie_stats$Goalie.Games.Played >= input$slider, ]
    # Filter data based on selected goalies
    filtered_data <- filtered_data %>%
      filter(Goalie.Name %in% input$goalies) %>%
      select(Goalie.Name, all_of(input$metrics))
    
    
    # Return the filtered data
    filtered_data
  })
  
  output$goalieTable <- renderDT({
    selected_data() %>%
      datatable(
        options = list(
          pageLength = 10, 
          scrollX = TRUE, 
          autoWidth = TRUE, 
          columnDefs = list(list(width = "auto", targets = "_all")),
          rownames = FALSE
        )
      )
  })
  
}

shinyApp(ui = ui, server = server)


