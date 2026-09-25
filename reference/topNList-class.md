# Class "topNList": Top-N List

Recommendations a Top-N list.

## Objects from the Class

Objects can be created by `predict` with a recommender model and new
data. Alternatively, objects can be created from a realRatingMatrix
using
[`getTopNLists`](http://michael.hahsler.net/recommenderlab/reference/realRatingMatrix-class.md).

## Slots

- `ratings`::

  Object of class `"list"`. Each element in the list represents a top-N
  recommendation (an integer vector) with item IDs (column numbers in
  the rating matrix). The items are ordered in each vector.

- `items`::

  Object of class `"list"` or `NULL`. If available, a list of the same
  structure as `items` with the ratings.

- `itemLabels`::

  Object of class `"character"`

- `n`::

  Object of class `"integer"` specifying the number of items in each
  recommendation. Note that the actual number on recommended items can
  be less depending on the data and the used algorithm.

## Methods

- coerce:

  `signature(from = "topNList", to = "list")`: returns a list with the
  items (labels) in the topNList.

- coerce:

  `signature(from = "topNList", to = "realRatingMatrix")`: creates a
  rating Matrix with entries for the items in the topN list.

- coerce:

  `signature(from = "topNList", to = "dgTMatrix")`

- coerce:

  `signature(from = "topNList", to = "dgCMatrix")`

- coerce:

  `signature(from = "topNList", to = "ngCMatrix")`

- coerce:

  `signature(from = "topNList", to = "matrix")`: returns a dense matrix
  with the ratings for the top-N items. All other items have a rating of
  NA.

- c:

  `signature(x = "topNList")`: combine several topN lists into a single
  list. The lists need to be for the same data (i.e., items).

- bestN:

  `signature(x = "topNList")`: returns only the best n recommendations
  (second argument is `n` which defaults to 10). The additional argument
  `minRating` can be used to remove all entries with a rating below this
  value.

- length:

  `signature(x = "topNList")`: for how many users does this object
  contain a top-N list?

- removeKnownItems:

  `signature(x = "topNList")`: remove items from the top-N list which
  are known (have a rating) for the user given as a ratingMatrix passed
  on as argument `known`.

- colCounts:

  `signature(x = "topNList")`: in how many top-N does each item occur?

- rowCounts:

  `signature(x = "topNList")`: number of recommendations per user.

- show:

  `signature(object = "topNList")`

## See also

[`evaluate`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`getList`](http://michael.hahsler.net/recommenderlab/reference/getList.md),
[`realRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/realRatingMatrix-class.md)
