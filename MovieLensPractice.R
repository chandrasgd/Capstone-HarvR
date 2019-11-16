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
download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)

mFile <- readLines(unzip(dl, "ml-latest-small/movies.csv" ))
rFile <- readLines(unzip(dl, "ml-latest-small/ratings.csv"))
tFile <- readLines(unzip(dl, "ml-latest-small/tags.csv"))

movies <- str_split_fixed(mFile,",", 3)
movies <- movies[-1,]
head(movies,15)

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
  mutate(movieId=as.numeric(levels(movieId))[movieId],
        title=as.character(title),
        genres=as.character(genres)
  )

ratings <- str_split_fixed(rFile,",", 4)
ratings <- ratings[-1,]
head(ratings,15)
colnames(ratings) <- c("userId","movieId","rating", "timestamp")

ratings <- as.data.frame(ratings) %>% 
  mutate(userId=as.numeric(levels(userId))[userId],
        movieId=as.numeric(levels(movieId))[movieId],
        rating=as.numeric(levels(rating))[rating],
        timestamp=as.numeric(timestamp)
  )
                                             
  
movielens <- left_join(ratings, movies, by="movieId")
str(movielens)

# Validation set is 10% of the total data set

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(movielens$rating,times=1, p=0.1, list=FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Ensure validation set includes movies and users present in edx data set
validation <- temp %>% semi_join(edx, by="movieId") %>% semi_join(edx, by="userId")

# remove records from validation sets for non-existent movies and users in edx
removed <- anti_join(temp, validation)

# add removed records back to edx data set
edx <- rbind(edx, removed)

# remove all temporary resources created thus far
rm(removed, temp, movielens, ratings, movies, mFile, rFile, tFile)


