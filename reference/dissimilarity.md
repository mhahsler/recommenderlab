# Dissimilarity and Similarity Calculation Between Rating Data

Calculate dissimilarities/similarities between ratings by users and for
items.

## Usage

``` r
# S4 method for class 'binaryRatingMatrix'
dissimilarity(x, y = NULL, method = NULL, args = NULL, which = "users")
# S4 method for class 'realRatingMatrix'
dissimilarity(x, y = NULL, method = NULL, args = NULL, which = "users")

similarity(x, y = NULL, method = NULL, args = NULL, ...)
# S4 method for class 'ratingMatrix'
similarity(x, y = NULL, method = NULL, args = NULL, which = "users",
  min_matching = 0, min_predictive = 0)
```

## Arguments

- x:

  a ratingMatrix.

- y:

  `NULL` or a second ratingMatrix to calculate cross-(dis)similarities.

- method:

  (dis)similarity measure to use. Available measures are typically
  `"cosine"`, `"pearson"`, `"jaccard"`, etc. See `dissimilarity` for
  class `itemMatrix` in arules for details about measures for
  `binaryRatingMatrix` and `dist` in proxy for `realRatingMatrix`.
  Default for `realRatingMatrix` is cosine and for `binaryRatingMatrix`
  is jaccard.

- args:

  a list of additional arguments for the methods.

- which:

  a character string indicating if the (dis)similarity should be
  calculated between `"users"` (rows) or `"items"` (columns).

- min_matching, min_predictive:

  Thresholds on the minimum number of ratings used to calculate the
  similarity and the minimum number of ratings that can be used for
  prediction.

- ...:

  further arguments.

## Details

Most dissimlarites and similarities are calculated using the proxy
package. Similarities are typically converted into dissimilarities using
\\s = 1 / (1 + d)\\ or \\s = 1 - d\\ (used for Jaccard, Cosine and
Pearson correlation) depending on the measure.

Similarities are usually defined in the range of \\\[0, 1\]\\, however,
Cosine similarity and Pearson correlation are defined in the interval
\\\[-1, 1\]\\. We rescale these measures with \\s' = 1 / 2 (s + 1)\\ to
the interval \\\[0, 1\]\\.

Similarities are calculated using only the ratings that are available
for both users/items. This can lead to calculating the measure using
only a very small number (maybe only one) of ratings. `min_matching` is
the required number of shared ratings to calculate similarities. To
predict ratings, there need to be additional ratings in argument `y`.
`min_predictive` is the required number of additional ratings to
calculate similarities. If `min_matching` or `min_predictive` fails,
then `NA` is reported instead of the calculated similarity.

## Value

returns an object of class `"dist"`, `"simil"` or an appropriate object
(e.g., a matrix with class `"crossdist"` o `"crosssimil"`) to represent
a cross-(dis)similarity.

## See also

[`ratingMatrix`](http://michael.hahsler.net/recommenderlab/reference/ratingMatrix-class.md),
[`dissimilarity`](https://rdrr.io/pkg/arules/man/dissimilarity.html) in
arules, and `dist` in proxy.

## Examples

``` r
data(MSWeb)

## between 5 users
dissimilarity(MSWeb[1:5,], method = "jaccard")
#>           1         2         3         4
#> 2 0.7500000                              
#> 3 0.8000000 0.3333333                    
#> 4 1.0000000 1.0000000 1.0000000          
#> 5 1.0000000 1.0000000 1.0000000 1.0000000
similarity(MSWeb[1:5,], method = "jaccard")
#>           1         2         3         4
#> 2 0.2500000                              
#> 3 0.2000000 0.6666667                    
#> 4 0.0000000 0.0000000 0.0000000          
#> 5 0.0000000 0.0000000 0.0000000 0.0000000

## between first 3 items
dissimilarity(MSWeb[,1:3], method = "jaccard", which = "items")
#>                           regwiz Support Desktop
#> Support Desktop        0.9407466                
#> End User Produced View 0.9753239       0.9533011
similarity(MSWeb[,1:3], method = "jaccard", which = "items")
#>                            regwiz Support Desktop
#> Support Desktop        0.05925341                
#> End User Produced View 0.02467613      0.04669887

## cross-similarity between first 2 users and users 10-20
similarity(MSWeb[1:2,], MSWeb[10:20,], method="jaccard")
#>           10         11         12         13         14         15         16
#> 1 0.12500000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#> 2 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>           17         18         19         20
#> 1 0.00000000 0.00000000 0.07692308 0.40000000
#> 2 0.00000000 0.00000000 0.08333333 0.20000000
```
