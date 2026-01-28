library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(scales)
library(plotly)

movies <- read_csv("movies.csv") |> 
  mutate(
    domgross_2013 = as.numeric(domgross_2013),
    budget_2013 = as.numeric(budget_2013),
    roi = (domgross_2013 - budget_2013) / budget_2013,
    binary = factor(binary, levels = c("PASS", "FAIL"))) |> 
  filter(!is.na(roi))

#This first graph shows a box plot, with the ROI distribution based on the bechdel status

box_plot <- ggplot(movies, aes(x = binary, y = roi, fill = binary, text = title)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("PASS" = "#379c09ff", "FAIL" = "#cb1c08ff")) +
  geom_jitter(alpha = 0.3, width = 0.2) +
  coord_cartesian(ylim = c(-1, 20)) +
  labs(title = "ROI Distribution by Bechdel Status", y = "Return on Investment (percentage)")

ggplotly(box_plot, tooltip = "text")

#This second graph shows a histogram, with the number of movies per year, filled by its pass status

histogram <- ggplot(movies, aes(x = year, fill = binary)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values = c("PASS" = "#379c09ff", "FAIL" = "#cb1c08ff")) +
  theme_minimal() +
  labs(title = "Movie Count per Year")

ggplotly(histogram)

#Bar chart of Bechdel test results

p <- ggplot(movies, aes(x = clean_test, fill = binary)) +
  geom_bar() +
    scale_fill_manual(values = c("PASS" = "#379c09ff", "FAIL" = "#cb1c08ff")) +
  theme_minimal() +
  labs(title = "Breakdown of Bechdel Test Results",
       x = "Detailed Result", y = "Number of Movies")
ggplotly(p, tooltip = "text")
  
#This is a genre analysis bar chart

genre_analysis <- movies |> 
  separate_rows(genre, sep = ", ") |> 
  group_by(genre) |> 
  summarize(
    total_movies = n(),
    pass_rate = mean(binary == "PASS")
    ) |> 
  filter(total_movies > 20)

genre_graph <- ggplot(genre_analysis, aes(x=reorder(genre, pass_rate), y = pass_rate, fill = pass_rate)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Genre Analysis",
  x = "Genre", y = "Number of Movies") +
  scale_y_continuous(labels = scales::percent)
ggplotly(genre_graph, tooltip = "text")

#Creating a scatter plot

scatter_plot <- ggplot(movies, aes(x = budget_2013, y = domgross_2013, color = binary, text = paste("Movie:", title))) +
  geom_jitter(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c("PASS" = "#379c09ff", "FAIL" = "#cb1c08ff")) +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    x = "Budget (log)",
    y = "Domestic Gross (log)",
    title = "Budget and Domestic Gross"
  ) +
  theme_minimal()

ggplotly(scatter_plot, tooltip = "text")