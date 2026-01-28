#Load packages

library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(scales)
library(plotly)

#Key
#no women: there are not two named women in the movie
#no talk: the two women don't talk
#men: the two women talk about men

#Let's begin with cleaning the data.
#Mutate: We create a column called ROI which is the domestic gross less its budget. It is normalized for 2013, or adjusted for inflation. This helps us compare movies across years.
#We also convert the pass and fail column to binary set of values (categorized), rather than just them being random letters
#Finally, we filter out all the NA

movies <- read_csv("movies.csv") |> 
  mutate(
    domgross_2013 = as.numeric(domgross_2013),
    budget_2013 = as.numeric(budget_2013),
    roi = (domgross_2013 - budget_2013) / budget_2013,
    binary = factor(binary, levels = c("PASS", "FAIL"))) |> 
  filter(!is.na(roi))

#Making a bar chart that shows the frequency of why the movie failed (or if it didn't)

ggplot(data = movies, aes(x=clean_test)) +
  geom_bar() +
  labs(
  x = "Result",
  y = "Count",
  title = "Count of What Category the Movie Fell Into")

#ROI violin density plot 

ggplot(data = movies, aes(x = binary, y = roi)) +
  geom_violin() +
  coord_cartesian(ylim = c(-1, 5)) +
  labs(
  x = "Result",
  y = "ROI",
  title = "Density Plot of the Movie's ROI Based on its Outcome")

#Sorting the pass rate by genres
#Separate rows usees the separator (the comma) and duplicates the row for each genre lasted
#Group by collects all the genre (ie action) into a bucket
#Total movies (n) counts how many are in each bucket
#Pass rate created an average of how many movies are a pass (1) vs fail (0)
#I filtered out genres with less than 20 movies - this dropped documentary and western (western was the lowest and documentary was above war)
#Reorder just made sure that the genre was sorted by its pass rate rather than alphabetically
#Scale continuous made sure the x axis was formatted like 20% rather than .2
#What is the difference between MUSIC and MUSICAl? As this redditor put it: "If a random bypasser with no connection to the main cast starts dancing and doing back up singing when the main cast breaks into song, it's a musical"

genre_analysis <- movies |> 
  separate_rows(genre, sep = ", ") |> 
  group_by(genre) |> 
  summarize(
    total_movies = n(),
    pass_rate = mean(binary == "PASS")
    ) |> 
  filter(total_movies > 20)

ggplot(genre_analysis, aes(x=reorder(genre, pass_rate), y = pass_rate, fill = pass_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
  x = "Genre",
  y = "Pass Rate",
  title = "Genre and Pass Rate")

# There are 199 movies with genre being NA, and 1776 movies total. That's about 11%
movies |> 
  count(genre == "NA")

movies |> 
  count()

199/1776

#Creating a scatter plot of the rating and its domestic gross (normalized for 2013). Colored by binary or whether it passed or failed
#This kinda looks bad

ggplot(data = movies, aes(x=imdb_rating, y = domgross_2013, color = binary)) +
  geom_jitter(size = .5) +
  scale_y_log10(labels = label_comma()) +
  labs(
  x = "IMDB Rating",
  y = "Domestic Gross (log)",
  title = "IMDB Rating and Domestic Gross")


# EDA: Who are the ROI outliers?
p <- ggplot(movies, aes(x = binary, y = roi, fill = binary, text = title)) +
  geom_boxplot(outlier.shape = NA) + # Hide static outliers
  geom_jitter(alpha = 0.3, width = 0.2) + # Show them as points instead
  coord_cartesian(ylim = c(-1, 20)) + # Zoom into the most common ROI range
  labs(title = "ROI Distribution by Bechdel Status", y = "Return on Investment")

ggplotly(p, tooltip = "text")

p <- ggplot(movies, aes(x = year, fill = binary)) +
  geom_histogram(binwidth = 1, color = "white") +
  theme_minimal() +
  labs(title = "Movie Count per Year")

ggplotly(p)

p <- ggplot(movies, aes(x = clean_test, y = budget_2013, fill = clean_test)) +
  geom_violin(alpha = 0.7) +
  scale_y_log10(labels = scales::label_comma()) +
  theme_minimal() +
  labs(title = "Budget Distribution by Bechdel Detail")

ggplotly(p)

library(plotly)

# Bubble chart: Time vs Budget, Sized by Revenue
p <- ggplot(movies, aes(x = year, y = budget_2013, 
                              size = domgross_2013, 
                              color = binary, 
                              text = title)) +
  geom_point(alpha = 0.5) +
  scale_y_log10(labels = scales::label_comma()) +
  theme_minimal() +
  labs(title = "The Evolution of Big-Budget Cinema",
       x = "Year", y = "Budget (Log Scale)")

ggplotly(p, tooltip = "text") %>% toWebGL()

# Simple version using clean_test categories
plot_ly(
  labels = c("Total", "PASS", "FAIL", "No Women", "Men Only", "Talk About Men", "Ok", "Dubious"),
  parents = c("", "Total", "Total", "FAIL", "FAIL", "FAIL", "PASS", "PASS"),
  values = c(1794, 803, 991, 141, 194, 514, 803, 142),
  type = 'sunburst'
)

# Create decade bins
movies_decade <- movies %>%
  mutate(decade = floor(year / 10) * 10) %>%
  separate_rows(genre, sep = ", ") %>%
  group_by(decade, genre) %>%
  summarise(pass_rate = mean(binary == "PASS"), .groups = "drop")

p <- ggplot(movies_decade, aes(x = decade, y = genre, fill = pass_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal() +
  labs(title = "Pass Rate Heatmap: Genre vs. Decade")

ggplotly(p)

p <- ggplot(movies, aes(x = imdb_rating, fill = clean_test)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~clean_test) +
  theme_minimal() +
  labs(title = "Rating Distributions by Detailed Test Result")

ggplotly(p)

p <- ggplot(movies, aes(x = clean_test, fill = binary)) +
  geom_bar() +
  scale_fill_manual(values = c("PASS" = "#18bc9c", "FAIL" = "#e74c3c")) +
  theme_minimal() +
  labs(title = "Breakdown of Bechdel Test Results",
       x = "Detailed Result", y = "Number of Movies")
ggplotly(p, tooltip = "text")
  

genre_analysis <- movies |> 
  separate_rows(genre, sep = ", ") |> 
  group_by(genre) |> 
  summarize(
    total_movies = n(),
    pass_rate = mean(binary == "PASS")
    ) |> 
  filter(total_movies > 20)

p <- ggplot(genre_analysis, aes(x=reorder(genre, pass_rate), y = pass_rate, fill = pass_rate)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Genre Analysis",
       x = "Genre", y = "Number of Movies") +
  scale_y_continuous(labels = scales::percent)
ggplotly(p, tooltip = "text")
