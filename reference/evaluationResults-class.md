# Class "evaluationResults": Results of the Evaluation of a Single Recommender Method

Contains evaluation results for several runs of the same recommender
method, represented as confusion matrices. The model used for each run
may also be available.

## See also

[`evaluate`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md)

Other evaluation:
[`calcPredictionAccuracy()`](http://michael.hahsler.net/recommenderlab/reference/calcPredictionAccuracy.md),
[`error`](http://michael.hahsler.net/recommenderlab/reference/error.md),
[`evaluate()`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`evaluationResultList-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationResultList-class.md),
[`evaluationScheme()`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme.md),
[`evaluationScheme-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme-class.md),
[`plot()`](http://michael.hahsler.net/recommenderlab/reference/plot.md)

## Objects from the Class

Objects are created by `evaluate`.

## Slots

- `results`::

  Object of class `"list"`: contains objects of class
  `"ConfusionMatrix"`, one for each run specified in the used evaluation
  scheme.

## Methods

- avg:

  `signature(x = "evaluationResults")`: returns evaluation metrics
  averaged across cross-validation folds.

- getConfusionMatrix:

  `signature(x = "evaluationResults")`: Deprecated. Use `getResults()`.

- getResults:

  `signature(x = "evaluationResults")`: returns a list of evaluation
  metrics with one element for each cross-validation fold.

- getModel:

  `signature(x = "evaluationResults")`: returns a list of recommender
  models used, if available.

- getRuns:

  `signature(x = "evaluationResults")`: returns the number of
  runs/number of confusion matrices.

- show:

  `signature(object = "evaluationResults")`
