#Load packages

library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(scales)

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


  