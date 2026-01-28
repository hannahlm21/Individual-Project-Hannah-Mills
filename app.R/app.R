library(shiny)
library(tidyverse)
library(bslib)
library(rsconnect)

#Loading data

movies <- read_csv("movies.csv") |> 
  mutate(
    binary = factor(binary, levels = c("PASS", "FAIL"))
  )

#UI Section

ui <- page_sidebar(
  title = "Movies that Pass the Bechdel Test Throughout the Years by Genre",
  sidebar = sidebar(
    sliderInput("year_range", "Select Year Range",
  min = 1970, max = 2013, value = c(1990, 2013), sep = ""),
  selectInput("genre_pick", "Available Genres in this Period", choices = "All")
  ),

  card(
    card_header("Percentage of Movies Passing the Test"),
    plotOutput("trendPlot")
  )
)

#Server Section

server <- function(input, output, session) {
  
  observe({
    available_genres <- movies |>
      filter(year >= input$year_range[1], year <= input$year_range[2]) |>
      separate_rows(genre, sep = ", ") |>
      pull(genre) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "genre_pick", 
    choices = c("All", available_genres),
    selected = input$genre_pick)
  })

  plot_data <- reactive({
    d <- movies |>
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$genre_pick != "All") {
      d <- d |> filter(str_detect(genre, input$genre_pick))
    }
    
    d |>
      group_by(year) |>
      summarize(pct_pass = mean(binary == "PASS", na.rm = TRUE))
  })

  output$trendPlot <- renderPlot({
    ggplot(plot_data(), aes(x = year, y = pct_pass)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme_minimal() +
      labs(x = "Year", y = "Percent Passing")
  })
}
  
#Running the App

  shinyApp(ui, server)

