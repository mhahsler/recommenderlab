# Class "evaluationResultList": Results of the Evaluation of a Multiple Recommender Methods

Contains the evaluation results for several runs using multiple
recommender methods in form of confusion matrices. For each run the used
models might be avialable.

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

## See also

[`evaluate`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`evaluationResults`](http://michael.hahsler.net/recommenderlab/reference/evaluationResults-class.md).
