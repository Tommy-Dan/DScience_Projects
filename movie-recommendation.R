###Install and Importing Essential Libraries:
install.packages("recommenderlab")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("data.table")

library(recommenderlab)
library("ggplot2")
library("data.table")
library("reshape2")


###Reading data into directory:
setwd("/home/dan/Desktop/Application_Workbench/R-applications")
movie_data <- read.csv("movies.csv", stringsAsFactors = FALSE)
ratting_data <- read.csv("ratings.csv")
str(movie_data)

###Summary view of the data:
summary(movie_data)
summary(ratting_data)

head(movie_data)
head(ratting_data)

                                   ###Data Pre-processing
#Converting the genres in the movie dataframe into a more usable format by creating one-hot encoding:
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0, 10330, 18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1, gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors = FALSE)  #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])   #convert from characters to integers.
}
str(genre_mat2)

###Search matrix
SearchMatrix <- cbind(movie_data[ ,1:2], genre_mat2[])
head(SearchMatrix)

ratingMatrix <- dcast(ratting_data, userId~movieId, value.var= "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[ ,-1]) #remove userIds

#Covert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

recommendation_model$IBCF_realRatingMatrix$parameters

## Exploring Similar Data:

similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)

# Now, we delineate the similarity that is shared between the films -

movie_similarity <- similarity(ratingMatrix[, 1:4],
                               method = "cosine", which = "items")
as.matrix(movie_similarity)

# Extracting the most unique ratings:
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique rating

# creating a count of movie rating:
Table_of_Ratings <- table(rating_values) #creating a count of movie ratings
Table_of_Ratings

## Most Viewed Movies Visualization:
library(ggplot2)
movie_views <- colCounts(ratingMatrix)  # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views)  # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ]  # sort by number of views
table_views$title <- NA
for (index in 1:10325) {
  table_views[index,3] <- as.character(subset(movie_data,
                                              movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

# We will visualize a bar plot for the total number of views of the top films:
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  geom_text(aes(label =views), vjust=-0.3, size=3.5) +
  theme(axis.text = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")
  
}
