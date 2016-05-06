library("recommenderlab")

data("MovieLense")

MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100,]
MovieLense100 <- MovieLense[, colCounts(MovieLense100) > 100,]
train <- MovieLense100[1:500]
test1 <- MovieLense100[501]
test3 <- MovieLense100[501:503]

## RECOM_POPULAR.R  
rec <- Recommender(train, method = "POPULAR")
rec

pre <- predict(rec, test1, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test3, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test1, n = 10, type = "ratings")
pre 
pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

pre <- predict(rec, test3, n = 10, type = "ratings")
pre 
pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

## RECOM_PCA.R      
rec <- Recommender(train, method = "PCA")
rec

pre <- predict(rec, test1, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test3, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test1, n = 10, type = "ratings")
pre 
pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

pre <- predict(rec, test3, n = 10, type = "ratings")
pre 
pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

## RECOM_RANDOM.R  
rec <- Recommender(train, method = "RANDOM")
rec

pre <- predict(rec, test1, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test3, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test1, n = 10, type = "ratings")
pre 
pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

pre <- predict(rec, test3, n = 10, type = "ratings")
pre 
pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")


## RECOM_UBCF.R
rec <- Recommender(train, method = "UBCF")
rec

pre <- predict(rec, test1, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test3, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test1, n = 10, type = "ratings")
pre 
pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

pre <- predict(rec, test3, n = 10, type = "ratings")
pre 
pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")


## RECOM_IBCF.R
rec <- Recommender(train, method = "IBCF")
rec

pre <- predict(rec, test1, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test3, n = 10)
pre
as(pre, "list")

pre <- predict(rec, test1, n = 10, type = "ratings")
pre 
pre <- predict(rec, test1, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")

pre <- predict(rec, test3, n = 10, type = "ratings")
pre 
pre <- predict(rec, test3, n = 10, type = "ratingMatrix")
pre
#as(pre, "matrix")



