library(tidyverse)
# install.packages("recommenderlab")
library(recommenderlab)
library(qdapTools)


# tags <- read.csv("00_data/Movies/tags.csv")

tags <- read_delim("00_data/Movies/tags.dat",delim = "::",col_names = FALSE)

tags %>% glimpse()

movies <- read_delim("00_data/Movies/movies.dat",delim = "::",col_names = FALSE)

ratings <- read_delim("00_data/Movies/ratings.dat",delim = "::",col_names = FALSE)

ratings %>% glimpse()
movies %>% glimpse()

tags <- tags %>% 
  select(
    user_id = X1,
    movie_id = X2,
    tag = X3,
    timestamp = X4
  )

movies <- movies %>% 
  select(movie_id = X1,
         title = X2,
         genre = X3)

ratings <- ratings %>% 
  select(user_id = X1,
         movie_id = X2,
         rating = X3,
         timestamp = X4)

object.size(tags)
object.size(tags_1)

# Avg rating 

movie_avg_ratings <- ratings %>% group_by(movie_id) %>% summarise(avg_rating = mean(rating)) %>% ungroup()

# Split Genres Data ----

movies_clean <- movies %>% 
  cbind(mtabulate(str_split(movies$genre,pattern = "\\|"))) %>%
  select(-title, -genre, -`(no genres listed)`)
  
# Add Movies with ratings 
movies_rated <- movies_clean %>% 
  inner_join(movie_avg_ratings,by = "movie_id")

# prepare the Matrix for Recommended Engine 

movies_matrix <- movies_rated %>% 
  select(-avg_rating) %>% 
  column_to_rownames(var = "movie_id") %>%
  as.matrix() %>% 
  as("binaryRatingMatrix")


# Get Full list of genres as a vector 

genres <- movies_matrix %>% colnames() %>% as_tibble()


# Retrieve to 15 movie tags to filter rarely used tags 

tags_sel <- tags %>% 
  # Filter out tags that are genres or irrelevant
  filter(!(tag %in% c("sci-fi","action","action", "comedy","BD-R","funny","horror","romance"))) %>%
  group_by(tag) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice_max(n,n = 18) %>% 
  filter(!(row_number()%in% c(7,14,17)
           )
         )


genre_sel <- movies %>% 
  group_by(genre) %>% 
  tally() %>% 
  slice_max(n,n = 15)
   

# Clean up tags only top 15 tags

tags_valid <- tags %>% 
  select(-user_id,-timestamp) %>% 
  filter(tag %in% tags_sel$tag) %>% 
  group_by(movie_id) %>% 
  mutate(tag = paste0(unique(tag),collapse = ",")) %>% 
  ungroup() %>% 
  unique()


movies_full <- movies %>% inner_join(movie_avg_ratings, by = "movie_id") %>% 
  left_join(tags_valid,by = "movie_id")


### Cross validation of testing multiple models in one go 

scheme <- movies_matrix %>% 
  evaluationScheme(method = "cross",
                   k      = 5,
                   train  = 0.8,
                   given  = 0)


models <- list(
  "association rules" = list(name = "AR",
                             param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name = "RANDOM", param = NULL),
  "popular items"     = list(name = "POPULAR", param = NULL),
  "item-based CF"     = list(name = "IBCF", param = list(k=5)),
  "user-based CF"     = list(name = "UBCF", param = list(method = "Cosine",nn = 500))
  )

results <- evaluate(scheme,
                    models,
                    type = "topNList",
                    n    = 1
                    )  
?evaluate

results

# One Model 

recom <- Recommender(movies_matrix,method = "IBCF",param = list(k = 5))

genres

genre_choice <- c("Action","Crime","Drama")

genre_choice_matrix <- genres %>% 
  mutate(genre = as.numeric(value %in% genre_choice)) %>% 
  pivot_wider(names_from = "value",values_from = genre)%>%
  as.matrix() %>% 
  as("binaryRatingMatrix")


# Make Predictions 

pred <- predict(recom,newdata = genre_choice_matrix,n = 3)

pred1 <- predict(recom,newdata = genre_choice_matrix,n = 1)

genre_sug <- getList(pred1) %>% 
  as.character()

fav_rating <- getRatingMatrix(pred1) %>% 
  as.numeric()

fav_rating <- pred1@ratings %>% pluck() %>% as.numeric()
# Use @ for S4 class

tags_sel

tag_choice <- c("based on a book")

top5 <- movies_full %>% 
  filter(str_detect(genre, genre_sug) == TRUE , str_detect(tag, tag_choice) == TRUE ) %>% 
  mutate(match = fav_rating* avg_rating) %>% 
  arrange(desc(match)) %>% 
  select(title, match) %>% 
  head()
