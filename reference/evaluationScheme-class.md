# Class "evaluationScheme": Evaluation Scheme

An evaluation scheme created from a data set. The scheme can be a simple
split into training and test data, k-fold cross-evaluation or using k
bootstrap samples.

## Objects from the Class

Objects can be created by
`evaluationScheme(data, method="split", train=0.9, k=NULL, given=3).`

## Slots

- `data`::

  Object of class `"ratingMatrix"`; the data set.

- `given`::

  Object of class `"integer"`; given ratings are randomly selected for
  each evaluation user and presented to the recommender algorithm to
  calculate recommend items/ratings. The recommended items are compared
  to the remaining items for the evaluation user.

- `goodRating`::

  Object of class `"numeric"`; Rating at which an item is considered a
  positive for evaluation.

- `k`::

  Object of class `"integer"`; number of runs for evaluation. Default is
  1 for method "split" and 10 for "cross-validation" and "bootstrap".

- `knownData`::

  Object of class `"ratingMatrix"`; data set with only known (given)
  items.

- `method`::

  Object of class `"character"`; evaluation method. Available methods
  are: "split", "cross-validation" and "bootstrap".

- `runsTrain`::

  Object of class `"list"`; internal repesentation for the split in
  training and test data for the evaluation runs.

- `train`::

  Object of class `"numeric"`; portion of data used for training for
  "split" and "bootstrap".

- `unknownData`::

  Object of class `"ratingMatrix"`; data set with only unknown items.

## Methods

- getData:

  `signature(x = "evaluationScheme")`: access data. Parameters are
  `type` ("train", "known" or "unknown", "given") and `run` (1...k).
  `"train"` returns the training data for the run, `"known"` returns the
  known ratings used for prediction for the test data, `"unknown"`
  returns the ratings used for evaluation for the test data, and
  `"given"` returns the number of items that were given in "known." If
  the `given` items was a positive number, then this will be a vector
  with this number, but if `given` was negative (all-but-x), then the
  number of given items for each test user will be different.

- show:

  `signature(object = "evaluationScheme")`

## See also

[`ratingMatrix`](http://michael.hahsler.net/recommenderlab/reference/ratingMatrix-class.md)
and the creator function
[`evaluationScheme`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme.md).
