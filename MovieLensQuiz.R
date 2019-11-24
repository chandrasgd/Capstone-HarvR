### https://github.com/chandrasgd/Capstone-HarvR.git
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if (!require(tidyverse)) install.packages("tidyverse", repos="http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos="http://cran.us.r-project.org")
if (!require(data.table)) installed.packages("data.table", repos="http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by="movieId")
str(movielens)

# Validation set is 10% of the total data set

set.seed(1)

test_index <- createDataPartition(movielens$rating,times=1, p=0.1, list=FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Ensure validation set includes movies and users present in edx data set
validation <- temp %>% 
  semi_join(edx, by="movieId") %>% 
  semi_join(edx, by="userId")

# remove records from validation sets for non-existent movies and users in edx
removed <- anti_join(temp, validation)

# add removed records back to edx data set
edx <- rbind(edx, removed)

# remove all temporary resources created thus far
rm(dl, removed, temp, movielens, ratings, test_index)

# How many rows and columns are there in the edx dataset?
dim(edx)

# How many zeros were given as ratings in the edx dataset?
# How many threes were given as ratings in the edx dataset?
edx %>% group_by(rating) %>% summarize(count=n()) %>% arrange(desc(count))

table(edx$rating)
#table(edx$genres)

sum(ifelse(edx$rating==0,1,0))

#How many different movies are in the edx dataset?
#How many different users are in the edx dataset?
edx %>% summarize(n_movies=n_distinct(movieId), n_users=n_distinct(userId))

genreList <- c("Romance", "Drama", "Thriller", "Comedy")
for (n in genreList) {
  gen <- str_detect(edx$genres, n)
  print(paste(n,'/',sum(ifelse(gen==TRUE,1,0))))
}

edx %>% 
  separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

movies <- edx %>% 
    group_by(movieId) %>% 
    summarize(count=n()) %>% 
    dplyr::arrange(desc(count)) %>%
    left_join(movies, by="movieId") %>%
    select(movieId, title, count)
#    slice(1:10)

edx %>% 
  group_by(rating) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

# extract the genres into a matrix
genres <- as.data.frame(edx$genres, stringsAsFactors=FALSE)

genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
dim(genres2)
colnames(genres2) <- c(1:8)

# find out the unique Genres from the genres dataframe (first columkn will gives all the genres present in the edx dataset)
uniqueGenres <- unique(genres2[,1])
uniqueGenres

#gather(edx, key="userId", "movieId", value="rating", -title, -genres, -timestamp)

# model fitting ################################################
# Y(u,i) = mu + b(i) + b(u) + e(u,i)
# global avg rating for all movies and all users
mu <- mean(edx$rating)
naive_RMSE <- RMSE(validation$rating, mu)
rmse_results <- data_frame(method="Naive", RMSE=naive_RMSE)

# observe effect of the Movie bias  #########################################
movie_avg <- edx %>% group_by (movieId) %>% summarize(b_i=mean(rating-mu))

# histogram to show the movie bias to the no of ratings
hist(movie_avg$b_i)

# prediction with movie bias over the average rating
y_movie <- validation %>% 
  left_join(movie_avg, by="movieId") %>% 
  mutate(pred = mu + b_i) %>% 
  .$pred
model_1_rmse <- RMSE(y_movie, validation$rating)

rmse_results <- bind_rows(rmse_results, data_frame(method="Model-1", RMSE=model_1_rmse))  
rmse_results %>% knitr::kable()

# observe effect of the User bias  #########################################
user_avg <- edx %>% group_by(userId) %>% summarize(b_u = mean(rating - mu), n=n())
# filter when user rated more than 100 movies
user_avg_100 <- user_avg %>% filter(n>100)

# histogram to show the movie bias to the no of ratings
hist(user_avg$b_u )
hist(user_avg_100$b_u )

# residual user bias on the rating
blend1_avg <- edx %>% 
  left_join(movie_avg, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

# prediction with movie bias and user bias over the average rating
blend1_rating <- validation %>% 
  left_join(movie_avg, by="movieId") %>% 
  left_join(blend1_avg, by="userId") %>%
  mutate(pred=mu + b_i + b_u) %>% 
  .$pred

model_2_rmse <- RMSE(blend1_rating, validation$rating)

rmse_results <- bind_rows(rmse_results, data_frame(method="Model-2", RMSE=model_2_rmse))
rmse_results %>% knitr::kable()

# biggest influence of the movie bias
validation %>% left_join(movie_avg, by="movieId") %>% 
  mutate(res=rating-(mu + b_i)) %>%
  arrange(desc(abs(res))) %>%
  select(movieId, title, rating, res, b_i) %>%
  distinct() %>%
  slice(1:10) %>% knitr::kable()

# validation %>% filter(movieId == 6371 & rating > 4)

# Top 10 and Bottom 10 movies based on the b_i
movies %>% left_join(movie_avg, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(movieId, title, b_i) %>%
  slice(1:10) %>% knitr::kable()

movies %>% left_join(movie_avg, by="movieId") %>%
  arrange(b_i) %>%
  select(movieId, title, b_i) %>%
  slice(1:10) %>% knitr::kable()

# observe the #ratings each movies got
edx %>% 
  count(movieId) %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(movies, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(movieId, title, b_i, count) %>%
  slice(1:10) %>% knitr::kable()

edx %>% 
  count(movieId) %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(movies, by="movieId") %>%
  arrange(b_i) %>%
  select(movieId, title, b_i, count) %>%
  slice(1:10) %>% knitr::kable()

# regularization on the movie bias
lambda <- 3
movie_reg_avg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i=sum(rating-mu)/(lambda + n()), n_i = n())
  
plot(movie_avg$b_i, movie_reg_avg$b_i)

edx %>% 
  count(movieId) %>%
  left_join(movie_reg_avg, by="movieId") %>%
  left_join(movies, by="movieId") %>%
  arrange(b_i) %>%
  select(movieId, title, b_i, count) %>%
  slice(1:10) %>% knitr::kable()

y_movie_reg <- validation %>%
  left_join(movie_reg_avg, by="movieId") %>%
  mutate(pred = (mu + b_i) )  %>%
  .$pred

model_3_rmse <- RMSE(y_movie_reg, validation$rating)

rmse_results <- bind_rows(rmse_results, data_frame(method="Model-M-Reg", RMSE=model_3_rmse))
rmse_results %>% knitr::kable()

# blended prediction with regularized movie bias 
blend2_rating <- validation %>% 
  left_join(movie_reg_avg, by="movieId") %>% 
  left_join(blend1_avg, by="userId") %>%
  mutate(pred=mu + b_i + b_u) %>% 
  .$pred

model_4_rmse <- RMSE(blend2_rating, validation$rating)

rmse_results <- bind_rows(rmse_results, data_frame(method="Model-Blend-Reg-Mov", RMSE=model_4_rmse))
rmse_results %>% knitr::kable()

# regularization on the user bias
lambda <- 3
user_reg_avg <- edx %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu)/(lambda + n()), n_u = n())

y_user_reg <- validation %>%
  left_join(user_reg_avg, by="userId") %>%
  mutate(pred = mu + b_u) %>%
  .$pred

model_5_rmse <- RMSE(y_user_reg, validation$rating)

plot(user_avg$b_u, user_reg_avg$b_u)

