# Predict Recommendations

Creates recommendations using a recommender model and data about new
users.

## Usage

``` r
# S4 method for class 'Recommender'
predict(object, newdata, n = 10, data=NULL,
    type="topNList", ...)
```

## Arguments

- object:

  a recommender model (class `"Recommender"`).

- newdata:

  data for active users (class `"ratingMatrix"`) or the index of users
  in the training data to create recommendations for. If an index is
  used then some recommender algorithms need to be passed the training
  data as argument `data`. Some algorithms may only support user
  indices.

- n:

  number of recommendations in the top-N list.

- data:

  training data needed by some recommender algorithms if `newdata` is a
  user index and not user data.

- type:

  type of recommendation. The default type is `"topNList"` which creates
  a top-N recommendation list with recommendations. Some recommenders
  can also predict ratings with type `"ratings"` which returns only
  predicted ratings with known ratings represented by `NA`, or type
  `"ratingMatrix"` which returns a completed rating matrix (Note that
  the predicted ratings may differ from the known ratings).

- ...:

  further arguments.

## Value

Returns an object of class `"topNList"` or of other appropriate classes.

## See also

[`Recommender`](http://michael.hahsler.net/recommenderlab/reference/Recommender-class.md),
[`ratingMatrix`](http://michael.hahsler.net/recommenderlab/reference/ratingMatrix-class.md).

## Examples

``` r
data("MovieLense")
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
train <- MovieLense100[1:50]

rec <- Recommender(train, method = "POPULAR")
rec
#> Recommender of type ‘POPULAR’ for ‘realRatingMatrix’ 
#> learned using 50 users.

## create top-N recommendations for new users
pre <- predict(rec, MovieLense100[101:102], n = 10)
pre
#> Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre, "list")
#> $`291`
#>  [1] "Titanic (1997)"               "Sting, The (1973)"           
#>  [3] "Alien (1979)"                 "Schindler's List (1993)"     
#>  [5] "2001: A Space Odyssey (1968)" "Amadeus (1984)"              
#>  [7] "Contact (1997)"               "Godfather, The (1972)"       
#>  [9] "Casablanca (1942)"            "Young Frankenstein (1974)"   
#> 
#> $`292`
#>  [1] "Titanic (1997)"                           
#>  [2] "Empire Strikes Back, The (1980)"          
#>  [3] "Usual Suspects, The (1995)"               
#>  [4] "Schindler's List (1993)"                  
#>  [5] "Braveheart (1995)"                        
#>  [6] "Amadeus (1984)"                           
#>  [7] "Contact (1997)"                           
#>  [8] "Indiana Jones and the Last Crusade (1989)"
#>  [9] "Young Frankenstein (1974)"                
#> [10] "Back to the Future (1985)"                
#> 

## predict ratings for new users
pre <- predict(rec, MovieLense100[101:102], type="ratings")
pre
#> 2 x 1664 rating matrix of class ‘realRatingMatrix’ with 2076 ratings.
as(pre, "matrix")[,1:10]
#>     Toy Story (1995) GoldenEye (1995) Four Rooms (1995) Get Shorty (1995)
#> 291               NA         3.329429                NA                NA
#> 292               NA               NA          3.838559           4.36015
#>     Copycat (1995) Shanghai Triad (Yao a yao yao dao waipo qiao) (1995)
#> 291             NA                                             4.444149
#> 292       4.028518                                             4.988599
#>     Twelve Monkeys (1995) Babe (1995) Dead Man Walking (1995)
#> 291                    NA          NA                      NA
#> 292                    NA    4.753997                      NA
#>     Richard III (1995)
#> 291           3.992739
#> 292                 NA


## create recommendations using user ids with ids 1..10 in the
## training data
pre <- predict(rec, 1:10 , data = train, n = 10)
pre
#> Recommendations as ‘topNList’ with n = 10 for 10 users. 
as(pre, "list")
#> $`1`
#>  [1] "Titanic (1997)"                           
#>  [2] "E.T. the Extra-Terrestrial (1982)"        
#>  [3] "Schindler's List (1993)"                  
#>  [4] "Casablanca (1942)"                        
#>  [5] "Lawrence of Arabia (1962)"                
#>  [6] "Butch Cassidy and the Sundance Kid (1969)"
#>  [7] "Glory (1989)"                             
#>  [8] "To Kill a Mockingbird (1962)"             
#>  [9] "It's a Wonderful Life (1946)"             
#> [10] "Rear Window (1954)"                       
#> 
#> $`5`
#>  [1] "Shawshank Redemption, The (1994)"  "Titanic (1997)"                   
#>  [3] "Pulp Fiction (1994)"               "Usual Suspects, The (1995)"       
#>  [5] "Terminator 2: Judgment Day (1991)" "Terminator, The (1984)"           
#>  [7] "Schindler's List (1993)"           "Braveheart (1995)"                
#>  [9] "Amadeus (1984)"                    "Contact (1997)"                   
#> 
#> $`6`
#>  [1] "Return of the Jedi (1983)"                
#>  [2] "Titanic (1997)"                           
#>  [3] "Empire Strikes Back, The (1980)"          
#>  [4] "Terminator 2: Judgment Day (1991)"        
#>  [5] "Indiana Jones and the Last Crusade (1989)"
#>  [6] "Dead Poets Society (1989)"                
#>  [7] "Henry V (1989)"                           
#>  [8] "Glory (1989)"                             
#>  [9] "Star Trek: The Wrath of Khan (1982)"      
#> [10] "Rear Window (1954)"                       
#> 
#> $`7`
#>  [1] "Titanic (1997)"                  "Toy Story (1995)"               
#>  [3] "This Is Spinal Tap (1984)"       "Mr. Holland's Opus (1995)"      
#>  [5] "Postino, Il (1994)"              "Close Shave, A (1995)"          
#>  [7] "Wrong Trousers, The (1993)"      "Big Night (1996)"               
#>  [9] "Hoop Dreams (1994)"              "Star Trek: First Contact (1996)"
#> 
#> $`10`
#>  [1] "Return of the Jedi (1983)"                
#>  [2] "Princess Bride, The (1987)"               
#>  [3] "Titanic (1997)"                           
#>  [4] "Empire Strikes Back, The (1980)"          
#>  [5] "Terminator 2: Judgment Day (1991)"        
#>  [6] "E.T. the Extra-Terrestrial (1982)"        
#>  [7] "Schindler's List (1993)"                  
#>  [8] "Contact (1997)"                           
#>  [9] "Indiana Jones and the Last Crusade (1989)"
#> [10] "Young Frankenstein (1974)"                
#> 
#> $`11`
#>  [1] "Star Wars (1977)"                  "Raiders of the Lost Ark (1981)"   
#>  [3] "Shawshank Redemption, The (1994)"  "Return of the Jedi (1983)"        
#>  [5] "Titanic (1997)"                    "Empire Strikes Back, The (1980)"  
#>  [7] "Terminator 2: Judgment Day (1991)" "Terminator, The (1984)"           
#>  [9] "Alien (1979)"                      "Toy Story (1995)"                 
#> 
#> $`13`
#>  [1] "Citizen Kane (1941)"          "Mr. Holland's Opus (1995)"   
#>  [3] "It's a Wonderful Life (1946)" "Close Shave, A (1995)"       
#>  [5] "Vertigo (1958)"               "Wrong Trousers, The (1993)"  
#>  [7] "Unforgiven (1992)"            "Killing Fields, The (1984)"  
#>  [9] "Time to Kill, A (1996)"       "Primal Fear (1996)"          
#> 
#> $`15`
#>  [1] "Fargo (1996)"                          
#>  [2] "Raiders of the Lost Ark (1981)"        
#>  [3] "Shawshank Redemption, The (1994)"      
#>  [4] "Princess Bride, The (1987)"            
#>  [5] "Titanic (1997)"                        
#>  [6] "Pulp Fiction (1994)"                   
#>  [7] "Silence of the Lambs, The (1991)"      
#>  [8] "Empire Strikes Back, The (1980)"       
#>  [9] "Usual Suspects, The (1995)"            
#> [10] "Monty Python and the Holy Grail (1974)"
#> 
#> $`16`
#>  [1] "Star Wars (1977)"                         
#>  [2] "Return of the Jedi (1983)"                
#>  [3] "Princess Bride, The (1987)"               
#>  [4] "Titanic (1997)"                           
#>  [5] "Contact (1997)"                           
#>  [6] "Indiana Jones and the Last Crusade (1989)"
#>  [7] "Casablanca (1942)"                        
#>  [8] "Wizard of Oz, The (1939)"                 
#>  [9] "Lawrence of Arabia (1962)"                
#> [10] "Butch Cassidy and the Sundance Kid (1969)"
#> 
#> $`18`
#>  [1] "Princess Bride, The (1987)"          "Titanic (1997)"                     
#>  [3] "Terminator 2: Judgment Day (1991)"   "Alien (1979)"                       
#>  [5] "Contact (1997)"                      "Lawrence of Arabia (1962)"          
#>  [7] "Glory (1989)"                        "Star Trek: The Wrath of Khan (1982)"
#>  [9] "Hunt for Red October, The (1990)"    "Die Hard (1988)"                    
#> 
```
