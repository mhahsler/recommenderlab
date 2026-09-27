# Class "evaluationResultList": Results from Evaluating Multiple Recommender Methods

Contains evaluation results for several runs of multiple recommender
methods, represented as confusion matrices. The models used for each run
may also be available.

## See also

[`evaluate`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`evaluationResults`](http://michael.hahsler.net/recommenderlab/reference/evaluationResults-class.md).

Other evaluation:
[`calcPredictionAccuracy()`](http://michael.hahsler.net/recommenderlab/reference/calcPredictionAccuracy.md),
[`error`](http://michael.hahsler.net/recommenderlab/reference/error.md),
[`evaluate()`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`evaluationResults-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationResults-class.md),
[`evaluationScheme()`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme.md),
[`evaluationScheme-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme-class.md),
[`plot()`](http://michael.hahsler.net/recommenderlab/reference/plot.md)

## Objects from the Class

Objects are created by `evaluate`.

## Slots

- `.Data`::

  Object of class `"list"`: a list of `"evaluationResults"`.

## Extends

Class `"list"`, from data part.

## Methods

- avg:

  `signature(x = "evaluationResultList")`: returns a list of average
  confusion matrices.

- \[:

  `signature(x = "evaluationResultList", i = "ANY", j = "missing", drop = "missing")`

- coerce:

  `signature(from = "list", to = "evaluationResultList")`

- show:

  `signature(object = "evaluationResultList")`
