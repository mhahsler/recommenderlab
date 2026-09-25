# Class "evaluationResults": Results of the Evaluation of a Single Recommender Method

Contains the evaluation results for several runs using the same
recommender method in form of confusion matrices. For each run the used
model might be avialable.

## Objects from the Class

Objects are created by `evaluate`.

## Slots

- `results`::

  Object of class `"list"`: contains objects of class
  `"ConfusionMatrix"`, one for each run specified in the used evaluation
  scheme.

## Methods

- avg:

  `signature(x = "evaluationResults")`: returns the evaluation metrics
  averaged of cross-validation folds.

- getConfusionMatrix:

  `signature(x = "evaluationResults")`: Deprecated. Use `getResults()`.

- getResults:

  `signature(x = "evaluationResults")`: returns a list of evaluation
  metrics with one element for each cross-valudation fold.

- getModel:

  `signature(x = "evaluationResults")`: returns a list of used
  recommender models (if avilable).

- getRuns:

  `signature(x = "evaluationResults")`: returns the number of
  runs/number of confusion matrices.

- show:

  `signature(object = "evaluationResults")`

## See also

[`evaluate`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md)
