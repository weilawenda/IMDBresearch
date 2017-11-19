# SQL query to get dataframe

library(mdsr)
library(RMySQL)
library(tidyverse)
library(ggthemes)

db <- dbConnect_scidb(dbname = "imdb")

sql <- "
SELECT t.id, mi.info, t.production_year
FROM title as t
LEFT JOIN movie_info as mi ON mi.movie_id = t.id
LEFT JOIN info_type as it ON mi.info_type_id = it.id
WHERE production_year is not NULL
AND mi.info_type_id = 3;
"
df <- db %>%
  dbGetQuery(sql)

df <- df %>%
  filter(production_year < 2017 & production_year > 1927)

glimpse(df)

# First Chart: genre variety

genres <- df %>%
  group_by(info) %>%
  summarise(g = first(info))

df1_total <- df %>%
  group_by(production_year) %>%
  summarise(genre_total = n_distinct(info))

df1_avg <- df %>%
  group_by(production_year, id) %>%
  summarise(genre_count = n_distinct(info)) %>%
  group_by(production_year) %>%
  summarise(genre_avg = mean(genre_count))
glimpse(df1_avg)

#plot1 <- df1_avg %>%
#  ggplot(aes(production_year, genre_avg)) +
#  geom_line()
#plot1


df1 <- df1_total %>%
  left_join(df1_avg, on = 'production_year') %>%
  gather(key = "value_type", value = "value",-production_year)

glimpse(df1)

plot1 <- df1 %>%
  ggplot(aes(production_year, value)) +
  geom_line() +
  facet_wrap(~factor(value_type,
                     labels = c("Average Number of Genres Per Film", "Total Number of Genres")), scale = "free")

# genre_avg , genre_total
plot1 <- plot1 +
  theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
  
plot1


df2_total <- df %>%
  group_by(production_year) %>%
  summarise(movie_total = n_distinct(id))

df2 <- df %>%
  left_join(df2_total, on = production_year) %>%
  group_by(production_year, info) %>%
  summarise(proportion = n() / first(movie_total))

plot2 <- df2 %>%
  filter(info == 'Short' | info == 'Drama'| info == 'Comedy') %>%
  ggplot(aes(production_year, proportion, col = info)) +
  geom_line() +
  ggtitle("Proportion of Short, Drama, and Comedy in Film Genres") +
  theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_segment(aes(x = 1947, y =0, xend = 1947, yend = 0.5), 
               color = "black", size =0.1) +
  geom_text(aes(x = 1947, y = 0.55, label = "United States v.\nParamount Pictures, Inc. 1947"), 
            size = 3, color = "black") +
  geom_segment(aes(x = 1994, y =0, xend = 1994, yend = 0.3), 
               color = "black", size =0.1) +
  geom_text(aes(x = 1994, y = 0.35, label = "Independent\nFilms Rise 1994"), 
            size = 3, color = "black") +
  geom_segment(aes(x = 2000, y =0, xend = 2000, yend = 0.43), 
               color = "black", size =0.1) +
  geom_text(aes(x = 2000, y = 0.46, label = "Digital Camera 2000"), 
            size = 3, color = "black") +
  geom_segment(aes(x = 2009, y =0, xend = 2009, yend = 0.55), 
               color = "black", size =0.1) +
  geom_text(aes(x = 2009, y = 0.58, label = "3D Avatar 2009"), 
            size = 3, color = "black") +
  geom_segment(aes(x = 1976, y =0, xend = 1976, yend = 0.35), 
               color = "black", size =0.1) +
  geom_text(aes(x = 1976, y = 0.4, label = "Star Wars\n'blockbuster' 1976"), 
            size = 3, color = "black") +
  geom_segment(aes(x = 1960, y =0, xend = 1960, yend = 0.35), 
               color = "black", size =0.1) +
  geom_text(aes(x = 1960, y = 0.38, label = "Hitchcock 1960"), 
            size = 3, color = "black")

plot2

