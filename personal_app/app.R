# Define UI for application that draws a histogram
library(shiny)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Genre Counts by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:",
                  min = 2000, max = 2022, value = 2000, step = 1)
    ),
    mainPanel(
      plotOutput("genre_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read the dataset
  songs_data <- read.csv("/Users/shawnji/Documents/billboard_used.csv")
  
  # Define genres of interest
  genres_of_interest <- c('r&b', 'pop', 'hip pop', 'salsa', 'soul', 'rock', 'country', 'neo')
  
  # Calculate genre counts based on selected year
  filtered_data <- reactive({
    songs_data %>%
      filter(Year == input$year)
  })
  
  genre_counts <- reactive({
    filtered_data() %>%
      summarise_at(vars(matches("Genres")), function(x) sum(grepl(genres_of_interest, x, ignore.case = TRUE)))
  })
  
  # Create bar plot
  output$genre_plot <- renderPlot({
    ggplot(genre_counts(), aes(x = names(genre_counts()), y = genre_counts())) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Genre Counts by Year",
           x = "Genre",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)