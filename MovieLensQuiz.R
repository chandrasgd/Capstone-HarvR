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
dl

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
validation <- temp %>% semi_join(edx, by="movieId") %>% semi_join(edx, by="userId")

# remove records from validation sets for non-existent movies and users in edx
removed <- anti_join(temp, validation)

# add removed records back to edx data set
edx <- rbind(edx, removed)

# remove all temporary resources created thus far
rm(removed, temp, movielens, ratings, movies, test_index)
#rm(dl)

# How many rows and columns are there in the edx dataset?
dim(edx)

# How many zeros were given as ratings in the edx dataset?
# How many threes were given as ratings in the edx dataset?
edx %>% group_by(rating) %>% summarize(count=n()) %>% arrange(desc(count))

sum(ifelse(edx$rating==0,1,0))

#How many different movies are in the edx dataset?
#How many different users are in the edx dataset?
length(unique(edx$title))
length(unique(edx$userId))

genreList <- c("Romance", "Drama", "Thriller", "Comedy")
for (n in genreList) {
  gen <- str_detect(edx$genres, n)
  print(paste(n,'/',sum(ifelse(gen==TRUE,1,0))))
}

edx %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarize(count=n()) %>% arrange(desc(count))

edx %>% group_by(movieId, title) %>% summarize(count=n()) %>% arrange(desc(count))
edx %>% group_by(rating) %>% summarize(count=n()) %>% arrange(desc(count))

