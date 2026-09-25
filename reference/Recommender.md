# Create a Recommender Model

Learns a recommender model from given data.

## Usage

``` r
Recommender(data, ...)
# S4 method for class 'ratingMatrix'
Recommender(data, method, parameter=NULL)
```

## Arguments

- data:

  training data.

- method:

  a character string defining the recommender method to use (see
  details).

- parameter:

  parameters for the recommender algorithm.

- ...:

  further arguments.

## Details

Recommender uses the registry mechanism from package registry to manage
methods. This let's the user easily specify and add new methods. The
registry is called `recommenderRegistry`. See examples section.

## Value

An object of class 'Recommender'.

## See also

[`Recommender`](http://michael.hahsler.net/recommenderlab/reference/Recommender-class.md),
[`ratingMatrix`](http://michael.hahsler.net/recommenderlab/reference/ratingMatrix-class.md),
[`predict`](http://michael.hahsler.net/recommenderlab/reference/predict.md).

## Examples

``` r
data("MSWeb")
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 100)

rec <- Recommender(MSWeb10, method = "POPULAR")
rec
#> Recommender of type ‘POPULAR’ for ‘binaryRatingMatrix’ 
#> learned using 100 users.

getModel(rec)
#> $topN
#> Recommendations as ‘topNList’ with n = 285 for 1 users. 
#> 
#> $ratings
#> 1 x 285 rating matrix of class ‘realRatingMatrix’ with 285 ratings.
#> 

## save and read a recommender model
saveRDS(rec, file = "rec.rds")
rec2 <- readRDS("rec.rds")
rec2
#> Recommender of type ‘POPULAR’ for ‘binaryRatingMatrix’ 
#> learned using 100 users.
unlink("rec.rds")

## look at registry and a few methods
recommenderRegistry$get_entry_names()
#>  [1] "HYBRID_realRatingMatrix"         "HYBRID_binaryRatingMatrix"      
#>  [3] "ALS_realRatingMatrix"            "ALS_implicit_realRatingMatrix"  
#>  [5] "ALS_implicit_binaryRatingMatrix" "AR_binaryRatingMatrix"          
#>  [7] "IBCF_binaryRatingMatrix"         "IBCF_realRatingMatrix"          
#>  [9] "LIBMF_realRatingMatrix"          "POPULAR_binaryRatingMatrix"     
#> [11] "POPULAR_realRatingMatrix"        "RANDOM_realRatingMatrix"        
#> [13] "RANDOM_binaryRatingMatrix"       "RERECOMMEND_realRatingMatrix"   
#> [15] "RERECOMMEND_binaryRatingMatrix"  "SVD_realRatingMatrix"           
#> [17] "SVDF_realRatingMatrix"           "UBCF_binaryRatingMatrix"        
#> [19] "UBCF_realRatingMatrix"          

recommenderRegistry$get_entry("POPULAR", dataType = "binaryRatingMatrix")
#> Recommender method: POPULAR for binaryRatingMatrix Description:
#>   Recommender based on item popularity. Reference: NA
#> Parameters: None

recommenderRegistry$get_entry("SVD", dataType = "realRatingMatrix")
#> Recommender method: SVD for realRatingMatrix Description: Recommender
#>   based on SVD approximation with column-mean imputation. Reference: NA
#> Parameters:
#>    k maxiter normalize
#> 1 10     100  "center"
```
