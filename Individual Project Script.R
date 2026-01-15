library(tidyverse)

movies <- read_csv("movies.csv")

#no women: there are not two named women in the movie
#no talk: the two women don't talk
#men: the two women only talk about men

ggplot(movies, aes(x= binary, fill = clean_test)) + 
  geom_bar(position = "fill")

ggplot(movies, aes(x= binary, fill = clean_test)) + 
  geom_bar()

movies |> 
  count(year)

ggplot(movies, aes(x= year)) + 
  geom_bar()

ggplot(movies, aes(x= year)) + 
  geom_histogram()



