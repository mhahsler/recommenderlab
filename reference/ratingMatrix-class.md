# Class "ratingMatrix": Virtual Class for Rating Data

Defines a common class for rating data.

## Objects from the Class

A virtual Class: No objects may be created from it.

## Methods

- \[:

  `signature(x = "ratingMatrix", i = "ANY", j = "ANY", drop = "ANY")`:
  subset the rating matrix (`drop` is ignorred).

- coerce:

  `signature(from = "ratingMatrix", to = "list")`

- coerce:

  `signature(from = "ratingMatrix", to = "data.frame")`: a data.frame
  with three columns. Col 1 contains user ids, col 2 contains item ids
  and col 3 contains ratings.

- colCounts:

  `signature(x = "ratingMatrix")`: number of ratings per column.

- rowCounts:

  `signature(x = "ratingMatrix")`: number of ratings per row.

- colMeans:

  `signature(x = "ratingMatrix")`: column-wise rating means.

- rowMeans:

  `signature(x = "ratingMatrix")`: row-wise rating means.

- dim:

  `signature(x = "ratingMatrix")`: dimensions of the rating matrix.

- dimnames\<-:

  `signature(x = "ratingMatrix", value = "list")`: replace dimnames.

- dimnames:

  `signature(x = "ratingMatrix")`: retrieve dimnames.

- getNormalize:

  `signature(x = "ratingMatrix")`: returns a list with normalization
  information for the matrix (NULL if data is not normalized).

- getRatings:

  `signature(x = "ratingMatrix")`: returns all ratings in `x` as a
  numeric vector.

- getRatingMatrix:

  `signature(x = "ratingMatrix")`: returns the ratings as a sparse
  matrix. The format is different for binary and real rating matices.

- hasRating:

  `signature(x = "ratingMatrix")`: returns a sparse logical matrix with
  TRUE for user-item combinations which have a rating.

- image:

  `signature(x = "ratingMatrix")`: plot the matrix.

- nratings:

  `signature(x = "ratingMatrix")`: number of ratings in the matrix.

- sample:

  `signature(x = "ratingMatrix")`: sample from users (rows).

- show:

  `signature(object = "ratingMatrix")`

## See also

See implementing classes
[`realRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/realRatingMatrix-class.md)
and
[`binaryRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/binaryRatingMatrix-class.md).
See
[`getList`](http://michael.hahsler.net/recommenderlab/reference/getList.md),
[`getData.frame`](http://michael.hahsler.net/recommenderlab/reference/getList.md),
[`similarity`](http://michael.hahsler.net/recommenderlab/reference/dissimilarity.md),
[`dissimilarity`](http://michael.hahsler.net/recommenderlab/reference/dissimilarity.md)
and
[`dissimilarity`](http://michael.hahsler.net/recommenderlab/reference/dissimilarity.md).
