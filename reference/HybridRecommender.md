# Create a Hybrid Recommender

Creates and combines recommendations using several recommender
algorithms.

## Usage

``` r
HybridRecommender(..., weights = NULL, aggregation_type = "sum")
```

## Arguments

- ...:

  objects of class 'Recommender'.

- weights:

  weights for the recommenders. The recommenders are equally weighted by
  default.

- aggregation_type:

  How are the recommendations aggregated. Options are "sum", "min", and
  "max".

## Details

The hybrid recommender is initialized with a set of pretrained
Recommender objects. Typically, the algorithms are trained using the
same training set. If different training sets are used, then, at least
the training sets need to have the same items in the same order.

Alternatively, hybrid recommenders can be created using the regular
[`Recommender()`](http://michael.hahsler.net/recommenderlab/reference/Recommender.md)
interface. Here `method` is set to `HYBRID` and `parameter` contains a
list with recommenders and weights. recommenders are a list of
recommender alorithms, where each algorithms is represented as a list
with elements name (method of the recommender) and parameters (the
algorithms parameters). This method can be used in
[`evaluate()`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md)

For creating recommendations (`predict`), each recommender algorithm is
used to create ratings. The individual ratings are combined using a
weighted sum where missing ratings are ignored. Weights can be specified
in `weights`.

## Value

An object of class 'Recommender'.

## See also

[`Recommender`](http://michael.hahsler.net/recommenderlab/reference/Recommender-class.md)

## Examples

``` r
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
train <- MovieLense100[1:100]
test <- MovieLense100[101:103]

## mix popular movies with a random recommendations for diversity and
## rerecommend some movies the user liked.
recom <- HybridRecommender(
  Recommender(train, method = "POPULAR"),
  Recommender(train, method = "RANDOM"),
  Recommender(train, method = "RERECOMMEND"),
  weights = c(.6, .1, .3)
  )

recom
#> Recommender of type ‘HYBRID’ for ‘ratingMatrix’ 
#> learned using 100 users.

getModel(recom)
#> $recommenders
#> $recommenders[[1]]
#> Recommender of type ‘POPULAR’ for ‘realRatingMatrix’ 
#> learned using 100 users.
#> 
#> $recommenders[[2]]
#> Recommender of type ‘RANDOM’ for ‘realRatingMatrix’ 
#> learned using 100 users.
#> 
#> $recommenders[[3]]
#> Recommender of type ‘RERECOMMEND’ for ‘realRatingMatrix’ 
#> learned using 100 users.
#> 
#> 
#> $weights
#> [1] 0.6 0.1 0.3
#> 

as(predict(recom, test), "list")
#> $`0`
#>  [1] "Great Day in Harlem, A (1994)"                            
#>  [2] "Santa with Muscles (1996)"                                
#>  [3] "Tough and Deadly (1995)"                                  
#>  [4] "Two or Three Things I Know About Her (1966)"              
#>  [5] "Boys, Les (1997)"                                         
#>  [6] "Brassed Off (1996)"                                       
#>  [7] "Dangerous Beauty (1998)"                                  
#>  [8] "Saint of Fort Washington, The (1993)"                     
#>  [9] "Cure, The (1995)"                                         
#> [10] "I Don't Want to Talk About It (De eso no se habla) (1993)"
#> 
#> $`1`
#>  [1] "Santa with Muscles (1996)"                  
#>  [2] "Tough and Deadly (1995)"                    
#>  [3] "Two or Three Things I Know About Her (1966)"
#>  [4] "Great Day in Harlem, A (1994)"              
#>  [5] "Dangerous Beauty (1998)"                    
#>  [6] "Boys, Les (1997)"                           
#>  [7] "Brassed Off (1996)"                         
#>  [8] "Hearts and Minds (1996)"                    
#>  [9] "Primary Colors (1998)"                      
#> [10] "Love! Valour! Compassion! (1997)"           
#> 
#> $`2`
#>  [1] "Cure, The (1995)"              "MURDER and murder (1996)"     
#>  [3] "It Takes Two (1995)"           "Great Day in Harlem, A (1994)"
#>  [5] "Senseless (1998)"              "Tainted (1998)"               
#>  [7] "Wedding Gift, The (1994)"      "Boys, Les (1997)"             
#>  [9] "Spanish Prisoner, The (1997)"  "Romper Stomper (1992)"        
#> 

## create a hybrid recommender using the regular Recommender interface.
## This is needed to use hybrid recommenders with evaluate().
recommenders <- list(
  RANDOM = list(name = "POPULAR", param = NULL),
  POPULAR = list(name = "RANDOM", param = NULL),
  RERECOMMEND = list(name = "RERECOMMEND", param = NULL)
)

weights <- c(.6, .1, .3)

recom <- Recommender(train, method = "HYBRID",
  parameter = list(recommenders = recommenders, weights = weights))
recom
#> Recommender of type ‘HYBRID’ for ‘ratingMatrix’ 
#> learned using 100 users.

as(predict(recom, test), "list")
#> $`0`
#>  [1] "Great Day in Harlem, A (1994)"              
#>  [2] "Two or Three Things I Know About Her (1966)"
#>  [3] "Dangerous Beauty (1998)"                    
#>  [4] "Tough and Deadly (1995)"                    
#>  [5] "Boys, Les (1997)"                           
#>  [6] "Whole Wide World, The (1996)"               
#>  [7] "Brassed Off (1996)"                         
#>  [8] "Crooklyn (1994)"                            
#>  [9] "He Walked by Night (1948)"                  
#> [10] "Hearts and Minds (1996)"                    
#> 
#> $`1`
#>  [1] "Dangerous Beauty (1998)"                    
#>  [2] "Two or Three Things I Know About Her (1966)"
#>  [3] "Hearts and Minds (1996)"                    
#>  [4] "Santa with Muscles (1996)"                  
#>  [5] "Tough and Deadly (1995)"                    
#>  [6] "Brassed Off (1996)"                         
#>  [7] "Great Day in Harlem, A (1994)"              
#>  [8] "Boys, Les (1997)"                           
#>  [9] "Horse Whisperer, The (1998)"                
#> [10] "Saint of Fort Washington, The (1993)"       
#> 
#> $`2`
#>  [1] "Next Karate Kid, The (1994)"                           
#>  [2] "World of Apu, The (Apur Sansar) (1959)"                
#>  [3] "Legal Deceit (1997)"                                   
#>  [4] "It Takes Two (1995)"                                   
#>  [5] "MURDER and murder (1996)"                              
#>  [6] "Silence of the Palace, The (Saimt el Qusur) (1994)"    
#>  [7] "Warriors of Virtue (1997)"                             
#>  [8] "Rendezvous in Paris (Rendez-vous de Paris, Les) (1995)"
#>  [9] "Wedding Bell Blues (1996)"                             
#> [10] "Dangerous Beauty (1998)"                               
#> 
```
