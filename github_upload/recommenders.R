setwd('/home/shuvayan/Downloads/RecommenderData')
data_path = '/home/shuvayan/Downloads/RecommenderData/'
#------------------------Load all the required libraries-------------------------------------#
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(biclust)
library(nmf)
library(tsne)
library(magrittr)
library(dplyr)
library(ggplot2)
library(cluster)
library(MKmisc)

# --------------------------------- Recommender Problem-------------------------------------------------#
# Read in the item csv with the new item added:
item_data <- read.csv(paste0(data_path,'itemData.csv'))

# Calculate the similarity matrix between items:
sim_matrix <- daisy(item_data[,-1], metric = "gower")
summary(sim_matrix)
gower_mat <- as.matrix(sim_matrix)
# Output most similar pairs:
item_data[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),arr.ind = TRUE)[1, ], ]
# Create cluster of items to see which cluster our new item fall into:
# Calculate silhouette width for many k using PAM;
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(sim_matrix,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:10, sil_width)
# From this we can see that clusters = 10 will be the best:
pam_fit <- pam(sim_matrix, diss = TRUE, k = 10)
# Append the cluster membership details to item_data:
item_data$cluster = pam_fit$clustering
# See which cluster our new item belongs to:
item_data[which(item_data$itemID == 'New_3456'),]$cluster # Cluster 4
# Plotting the results:
tsne_obj <- tsne(sim_matrix)

tsne_data <- tsne_obj %>% data.frame() %>% setNames(c("X", "Y")) %>% 
              mutate(cluster = factor(pam_fit$clustering),
              name = item_data$itemID)

ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
# There is clear demarcation between the items as the clusters are very well seperated.
# Our new item lies in the green cluster.
# We will take only the items in cluster 4 to assign user ratings for the new item.
# Subset the item_data for only cluster 4:
item_data_cluster4 <- subset(item_data,item_data$cluster == 4)
#length(unique(item_data_cluster4$itemID)) - we use these 41 items,and the users ratings for them to assign ratings to the new item.
# Algo:
# If any user has rated >= 1 of these 41 items,assign the average of the ratings as the rating for the new item for that user,else NA.

##--------------- Proceed with data preparation with the new item------------------------##
data_file = paste0(data_path,'UserItemRatingMatrix_DataFrame.csv')
user_item <- read.csv(data_file)

##### ----- 
# Convert the user item rating matrix to long format:
user_item_rating= melt(user_item, id.vars=c("X"),na.rm = T)
colnames(user_item_rating) = c("user","item","rating")
# Users:
#length(unique(user_item_rating$user))
# Items:
#length(unique(user_item_rating$item))
# Remove NA rows:
user_item_rating_final <- na.omit(user_item_rating)
# Users:
#length(unique(user_item_rating_final$user))
# Items:
#length(unique(user_item_rating_final$item))
UIMatrix <- as(user_item_rating_final,"realRatingMatrix")
# Data exploration of the UI rating matrix:

# 1. Exploring the ratings:
vector_ratings <- as.vector(UIMatrix@data)
table_ratings <- table(vector_ratings)
vector_ratings <- factor(vector_ratings)
qplot(vector_ratings) + ggtitle("Distribution of the ratings")

# Exploring the items which have been rated:
views_per_item <- colCounts(UIMatrix)
# Sort the items by the number of ratings:

table_views <- data.frame(
  item = names(views_per_item),
  views = views_per_item
)
table_views <- table_views[order(table_views$views,decreasing = T),]
# Visualize:
ggplot(table_views[1:6, ], aes(x = item, y = views)) +
  geom_bar(stat="identity") + theme(axis.text.x =
                                      element_text(angle = 45, hjust = 1)) + ggtitle("Number of views of the top items")

# Visualising the ratings matrix:
#select the most relevant users and items,This means visualizing only the users who have rated many items and the items that have been rated by many users.
# To identify and select the most relevant users and items:
#1. Determine the minimum number of items per user.
#2. Determine the minimum number of users per item.
#3.	Select the users and items matching these criteria.
# For instance, we can visualize the top percentile of users and items. In order to do this, we use the quantile function:
min_n_items <- quantile(rowCounts(UIMatrix), 0.99)
min_n_users <- quantile(colCounts(UIMatrix), 0.99)
min_n_users;min_n_items
image(UIMatrix[rowCounts(UIMatrix) > min_n_items,colCounts(UIMatrix) > min_n_users], main = "Heatmap of the top users and items")

# Now proceed with data preparation for recommender model:

# Create a data frame with only the items which are in cluster 4:
user_new_item <- user_item_rating_final[ user_item_rating_final$item %in% item_data_cluster4$itemID, ]
# For each user get his average of ratings for all the items in the user_new_item:
library(sqldf)
user_avg_rating <- sqldf('select user,"New3456" as item,round(avg(rating)) as rating from user_new_item group by user')

# Append this to the existing user_item_final data:
user_item_final <- rbind(user_item_rating_final,user_avg_rating)
# Create csv for importing into python:
# write.csv(user_item_final,file = paste0(data_path,'user_item_new.csv'),row.names = F)

####--------- Start with recommender building ----------------------------------------------######
# 1. Collaborative Filtering:
UIMatrix <- as(user_item_final,"realRatingMatrix")
#UIMatrix_Data <- as(UIMatrix,"data.frame")

#Now proceed with the algorithms in recommenderlab
## create 90/10 split (known/unknown):
scheme = evaluationScheme(UIMatrix,method = "cross-validation",train = 0.9,k = 30,given = -1)

## UBCF---------------------------------------------------------------####
results_ubcf <- Recommender(getData(scheme,"train"),method = "UBCF")
results_ibcf <- Recommender(getData(scheme,"train"),method = "IBCF")
results_random <- Recommender(getData(scheme,"train"),method = "Random")
results_SVD <- Recommender(getData(scheme,"train"),method = "SVD")

## create predictions for the test data using known ratings (see given above)
pred_ubcf <- predict(results_ubcf, getData(scheme, "known"), type="ratings")
ubcf_acc <- calcPredictionAccuracy(pred_ubcf, getData(scheme, "unknown"))

##-----IBCF-----------------------------------------------------------------###
pred_ibcf <- predict(results_ibcf, getData(scheme, "known"), type="ratings")
ibcf_acc <- calcPredictionAccuracy(pred_ibcf, getData(scheme, "unknown"))

##---------------------------------- Random----------------------------------------####
## create predictions for the test data using known ratings (see given above)
pred_rand <- predict(results_random, getData(scheme, "known"), type="ratings")
rand_acc <- calcPredictionAccuracy(pred_rand, getData(scheme, "unknown"))


##-------------------------- Popular---------------------------------------------------####
pred_SVD <- predict(results_SVD, getData(scheme, "known"), type="ratings")
SVD_acc <- calcPredictionAccuracy(pred_SVD, getData(scheme, "unknown"))

## ---------------------Hybrid Recommender ------------------------------------------######
## mix all the model recommendations for diversity.
# Create a model RMSE matrix:
model_names <- c("UBCF","IBCF","RANDOM","SVD")
acc_values <- c(ubcf_acc['RMSE'],ibcf_acc['RMSE'],SVD_acc['RMSE'],rand_acc['RMSE'])
model_rmse <- data.frame(model_names,acc_values)
#Standardise the weights
range01 <- function(x){
  (max(x)-x)/(max(x)-min(x))
  }
model_rmse$std_weights <- range01(model_rmse$acc_values)

results_hybrid <- HybridRecommender(
  Recommender(getData(scheme,"train"),method = "UBCF"),
  Recommender(getData(scheme,"train"),method = "IBCF"),
  Recommender(getData(scheme,"train"),method = "RANDOM"),
  Recommender(getData(scheme,"train"),method = "SVD"),
  weights = model_rmse$std_weights
)

getModel(results_hybrid)
pred_hybrid <- predict(results_hybrid, getData(scheme, "known"), type="ratings")
hybrid_acc <- calcPredictionAccuracy(pred_hybrid, getData(scheme, "unknown"))

# The hybrid model doesn't give good RMSE

#First, we randomly define the which_train vector that is TRUE for users in the training set and FALSE for the others.
# We will set the probability in the training set as 80 percent:
which_train <- sample(x = c(TRUE, FALSE), size = nrow(UIMatrix),replace = TRUE, prob = c(0.8, 0.2))
head(which_train)
#Let's define the training and the test sets:
recc_data_train <- UIMatrix[which_train, ]
recc_data_test <- UIMatrix[!which_train, ]

recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))
recc_model

n_recommended = 6
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)

recc_matrix <- sapply(recc_predicted@items, function(x){colnames(UIMatrix)[x]})


#Let's visualize the recommendations for a particular user:
recc_matrix[['niy883']]

# Transpose the matrix and recommend users to items:
T_UIMatrix <- as(t(UIMatrix@data),"realRatingMatrix")
which_train <- sample(x = c(TRUE, FALSE), size = nrow(T_UIMatrix),replace = TRUE, prob = c(0.8, 0.2))
head(which_train)
#Let's define the training and the test sets:
recc_data_train <- T_UIMatrix[which_train, ]
recc_data_test <- T_UIMatrix[!which_train, ]

recc_model <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 30))
recc_model

n_recommended = 20
recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)

recc_matrix <- sapply(recc_predicted@items, function(x){colnames(T_UIMatrix)[x]})


#Let's visualize the recommendations for a particular user:
recc_matrix[['New3456']]
