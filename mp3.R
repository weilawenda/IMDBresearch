library(mdsr)
library(RMySQL)
db <- dbConnect_scidb(dbname = "imdb")

sql <- "
SELECT mi.info_type_id, it.info, t.production_year
FROM movie_info as mi
LEFT JOIN info_type as it ON mi.info_type_id = it.id
LEFT JOIN title as t ON mi.id = t.id
WHERE production_year is not NULL
AND mi.info_type_id is not NULL; 
"
df <- db %>%
  dbGetQuery(sql)

glimpse(df)