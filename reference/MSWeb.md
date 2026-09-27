# Anonymous web data from www.microsoft.com

Records the Vroots visited by users during a one-week period.

## Usage

``` r
data(MSWeb)
```

## Format

The format is: Formal class `"binaryRatingMatrix"`.

## Source

Asuncion, A., Newman, D.J. (2007). UCI Machine Learning Repository,
Irvine, CA: University of California, School of Information and Computer
Science. <https://archive.ics.uci.edu/>

## Details

The data set was created by sampling and processing the
www.microsoft.com logs. It records site use by 38,000 anonymous,
randomly selected users. For each user, it lists all areas of the web
site (Vroots) that the user visited during a one-week period in February
1998.

This data set contains 32,710 valid users and 285 Vroots.

## References

J. Breese, D. Heckerman., C. Kadie (1998). Empirical Analysis of
Predictive Algorithms for Collaborative Filtering, Proceedings of the
Fourteenth Conference on Uncertainty in Artificial Intelligence,
Madison, WI.

## See also

Other datasets:
[`Jester5k`](http://michael.hahsler.net/recommenderlab/reference/Jester5k.md),
[`MovieLense`](http://michael.hahsler.net/recommenderlab/reference/MovieLense.md)

## Examples

``` r
data(MSWeb)
MSWeb
#> 32710 x 285 rating matrix of class ‘binaryRatingMatrix’ with 98653 ratings.

nratings(MSWeb)
#> [1] 98653

## look at first two users
as(MSWeb[1:2,], "list")
#> $`1`
#> [1] "regwiz"                 "Support Desktop"        "End User Produced View"
#> 
#> $`2`
#> [1] "Support Desktop" "Knowledge Base" 
#> 

## items per user
hist(rowCounts(MSWeb), main="Distribution of Vroots visited per user")
```
