R package recommenderlab - Lab for Developing and Testing Recommender
Algorithms
================

[![CRAN
version](http://www.r-pkg.org/badges/version/recommenderlab)](https://CRAN.R-project.org/package=recommenderlab)
[![stream r-universe
status](https://mhahsler.r-universe.dev/badges/recommenderlab)](https://mhahsler.r-universe.dev/ui#package:recommenderlab)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/grand-total/recommenderlab)](https://CRAN.R-project.org/package=recommenderlab)

Provides a research infrastructure to develop and evaluate collaborative
filtering recommender algorithms. This includes a sparse representation
for user-item matrices, many popular algorithms, top-N recommendations,
and cross-validation. The package supports rating (e.g., 1-5 stars) and
unary (0-1) data sets. Supported algorithms are:

-   User-based collborative filtering (UBCF)
-   Item-based collborative filtering (IBCF)
-   SVD with column-mean imputation (SVD)
-   Funk SVD (SVDF)
-   Alternating Least Squares (ALS)
-   Matrix factorization with LIBMF (LIBMF)
-   Association rule-based recommender (AR)
-   Popular items (POPULAR)
-   Randomly chosen items for comparison (RANDOM)
-   Re-recommend liked items (RERECOMMEND)
-   Hybrid recommendations (HybridRecommender)

For evaluation, the framework supports given-n and all-but-x protocols
with

-   Train/test split
-   Cross-validation
-   Repeated bootstrap sampling

Evaluation measures are:

-   Rating errors: MSE, RMSE, MAE
-   Top-N recommendations: TPR/FPR (ROC), precision and recall

## Installation

**Stable CRAN version:** install from within R with

``` r
install.packages("recommenderlab")
```

**Current development version:** Install from
[r-universe.](https://mhahsler.r-universe.dev/ui#package:recommenderlab)

## Usage

Load the package and prepare a dataset (included in the package).

``` r
library("recommenderlab")
data("MovieLense")
### use only users with more than 100 ratings
MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100, ]
MovieLense100
```

    ## 358 x 1664 rating matrix of class 'realRatingMatrix' with 73610 ratings.

Train a user-based collaborative filtering recommender using a small
training set.

``` r
train <- MovieLense100[1:50]
rec <- Recommender(train, method = "UBCF")
rec
```

    ## Recommender of type 'UBCF' for 'realRatingMatrix' 
    ## learned using 50 users.

Create top-N recommendations for new users (users 101 and 102)

``` r
pre <- predict(rec, MovieLense100[101:102], n = 5)
pre
```

    ## Recommendations as 'topNList' with n = 5 for 2 users.

``` r
as(pre, "list")
```

    ## $`0`
    ## [1] "Braindead (1992)"                           
    ## [2] "Bad Taste (1987)"                           
    ## [3] "Mrs. Brown (Her Majesty, Mrs. Brown) (1997)"
    ## [4] "Cry, the Beloved Country (1995)"            
    ## [5] "Inspector General, The (1949)"              
    ## 
    ## $`1`
    ## [1] "Once Upon a Time... When We Were Colored (1995)"
    ## [2] "Go Fish (1994)"                                 
    ## [3] "Crooklyn (1994)"                                
    ## [4] "Great Day in Harlem, A (1994)"                  
    ## [5] "Shaggy Dog, The (1959)"

A simple Shiny App running recommenderlab can be found at
<https://mhahsler-apps.shinyapps.io/Jester/> ([source
code](https://github.com/mhahsler/recommenderlab/tree/master/Work/apps)).

## References

-   Michael Hahsler (2022) recommenderlab: An R framework for developing
    and testing recommendation algorithms. arXiv:2205.12371 \[cs.IR\].
    DOI:
    [10.48550/arXiv.2205.12371](https://doi.org/10.48550/arXiv.2205.12371).
-   recommenderlab [reference
    manual](https://CRAN.R-project.org/package=recommenderlab/recommenderlab.pdf)
-   Suresh K. Gorakala and Michele Usuelli (2015) [Building a
    Recommendation System with
    R](https://www.amazon.com/Building-Recommendation-System-Suresh-Gorakala/dp/1783554495)
    (Packt Publishing) features the package recommenderlab.
