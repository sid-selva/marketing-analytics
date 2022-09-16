library(tidyverse)
library(recommenderlab)
library(qdapTools)
library(data.table)

movies <- read_delim("00_data/latest/movies.dat",delim = "::",col_names = FALSE)

ratings <- read_delim("00_data/latest/ratings.dat",delim = "::",col_names = FALSE)

movies <- read_csv("00_data/IMDB-Dataset/movies.csv") %>% janitor::clean_names()

ratings <- read_csv("00_data/IMDB-Dataset/ratings.csv")

movies <- movies %>% 
  select(movie_id = X1,
         title = X2,
         genre = X3)

ratings <- ratings %>% 
  select(user_id = X1,
         movie_id = X2,
         rating = X3,
         timestamp = X4)

str(movies)
?str

## Add One hot encoding ---- 

genre_dummy <- mtabulate(str_split(movies$genres, pattern = "\\|")) %>% glimpse()

movies_clean <- movies %>% 
  cbind(genre_dummy) %>% 
  select(-genres, -title) %>% janitor::clean_names() %>% 
  select(-no_genres_listed)

?dcast

ratings %>% glimpse()

ratings %>% 
  select(-timestamp) %>% 
  pivot_wider(names_from = movie_id,values_from = rating, values_fill = NA) %>% glimpse()

rating_matrix <- ratings %>% 
  janitor::clean_names() %>% 
  dcast(formula = user_id ~ movie_id,value.var = "rating",na.rm = FALSE) 

rating_matrix[,1:50]%>% glimpse()

rating_matrix <- as.matrix(rating_matrix[,-1]) #remove userIds

rating_matrix <- as(rating_matrix, "realRatingMatrix")


recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

recommendation_model$IBCF_realRatingMatrix$parameters


# Similarity Matrix ---- 

similarity_mat <- similarity(rating_matrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)

image(as.matrix(similarity_mat), main = "User's Similarities")


# Items Similarity 

movie_similarity <- similarity(rating_matrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "Movies similarity")


# Unique Ratings 

rating_values <- as.vector(rating_matrix@data)
unique(rating_values) # extracting unique ratings

table_of_ratings <- table(rating_values) # creating a count of movie ratings
table_of_ratings


# Most Viewed Movies 

movie_views <- colCounts(rating_matrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views

table_views <- table_views %>% as_tibble() %>% mutate(movie = as_factor(movie))%>%
  left_join((movies %>% 
               select(-genres, movie = movie_id) %>% 
               mutate(movie = as_factor(movie))
             ),by = c("movie"))

table_views %>% head() %>% 
  ggplot(aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")

image(rating_matrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")


# Selecting and Preparing the Data 

movie_ratings <- rating_matrix[rowCounts(rating_matrix) > 50,
                              colCounts(rating_matrix) > 50]
movie_ratings


minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")


average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")

# Normalize Data 

normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

# Binarzation 

binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

### Collaborative Filtering System

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]


recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)


model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")


sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

# prediction 

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations


user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movies,
                                             movies$movie_id == movies_user1[index])$title)
}
movies_user2


recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]

recommendation_matrix
