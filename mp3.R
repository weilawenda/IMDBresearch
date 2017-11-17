# SQL query to get dataframe

library(mdsr)
library(RMySQL)
library(tidyverse)

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
  filter(production_year < 2017)

glimpse(df)

# First Chart: genre variety

genres <- df %>%
  group_by(info) %>%
  summarise(g = first(info))


df1_total <- df %>%
  filter(info != 'Short') %>%
  group_by(production_year) %>%
  summarise(genre_total = n_distinct(info))

df1_avg <- df %>%
  filter(info != 'Short') %>%
  group_by(production_year, id) %>%
  summarise(genre_count = n_distinct(info)) %>%
  group_by(production_year) %>%
  summarise(genre_avg = mean(genre_count))
glimpse(df1_avg)

plot1 <- df1_avg %>%
  ggplot(aes(production_year, genre_avg)) +
  geom_line()

plot1


df1 <- df1_total %>%
  left_join(df1_avg, on = 'production_year') %>%
  gather(key = "value_type", value = "value", -production_year)

glimpse(df1)

plot1 <- df1 %>%
  ggplot(aes(production_year, value)) +
  geom_line() +
  facet_wrap(~value_type, scales = "free")

plot1

df2_total <- df %>%
  group_by(production_year) %>%
  summarise(movie_total = n_distinct(id))

df2 <- df %>%
  left_join(df2_total, on=production_year) %>%
  group_by(production_year, info) %>%
  summarise(proportion = n()/first(movie_total))

plot2 <- df2 %>%
  filter(info=='Short' | info=='Action' | info == 'Sex') %>%
  ggplot(aes(production_year, proportion, col = info)) +
  geom_line()

plot2
  