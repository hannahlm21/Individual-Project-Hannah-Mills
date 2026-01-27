library(shiny)
library(tidyverse)
library(bslib)
library(plotly)

movies <- read_csv("movies.csv")

ui <- page_navbar(
  title = "Title",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel("Global Trends",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("year_range", "Years:", 1970, 2013, c(1990,2013), sep = ""),
        selectInput("genre_pick", "Genre", choices = "All")
      ),
      plotOutput("trendPlot")
    )
  ),

nav_panel("Financials",
  card(plotlyOutput("moneyPlot"))
  )
)

server <- function(input, output, session) {
  observe({
    available <- movies |> 
      filter(year >= input$year_range[1], year <= input$year_range[2]) |> 
      separate_rows(genre, sep = ", ") |> 
      pull(genre) |> 
      unique() |> 
      sort()

    updateSelectInput(session, "genre_pick", choices = c("All", available))
})
  
  output$trendPlot <- renderPlot({
    # Filter data based on UI inputs
    d <- movies %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    
    if (input$genre_pick != "All") {
      d <- d %>% filter(str_detect(genre, input$genre_pick))
    }
    
    # Calculate yearly percentage
    d_summary <- d %>%
      group_by(year) %>%
      summarise(pct_pass = mean(binary == "PASS", na.rm = TRUE))
    
    # Create the line chart
    ggplot(d_summary, aes(x = year, y = pct_pass)) +
      geom_line(color = "#2c3e50", linewidth = 1) +
      geom_point(color = "#18bc9c", size = 3) +
      # Adds a shaded trend area (Cool visual touch)
      geom_smooth(method = "loess", se = FALSE, color = "#e74c3c", linetype = "dashed") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme_minimal() +
      labs(title = paste("Pass Rate for", input$genre_pick, "Movies"),
           x = "Year", y = "Percent Passing")
  })

  # 3. TAB 2: FINANCIAL PLOT LOGIC
  output$moneyPlot <- renderPlotly({
    # Create the ggplot object first
    p <- ggplot(movies, aes(
      x = budget_2013, 
      y = domgross_2013, 
      color = binary,
      # Custom tooltip string
      text = paste0(
        "<b>", title, "</b> (", year, ")<br>",
        "Budget: $", scales::label_comma()(budget_2013), "<br>",
        "Domestic: $", scales::label_comma()(domgross_2013), "<br>",
        "Result: ", clean_test
      )
    )) +
      geom_point(alpha = 0.5) +
      scale_x_log10(labels = scales::label_comma()) +
      scale_y_log10(labels = scales::label_comma()) +
      scale_color_manual(values = c("PASS" = "#18bc9c", "FAIL" = "#e74c3c")) +
      theme_minimal() +
      labs(x = "Budget (Log Scale)", y = "Domestic Gross (Log Scale)")

    # Convert to interactive Plotly
    ggplotly(p, tooltip = "text") %>% toWebGL()
  })

}

shinyApp(ui, server)