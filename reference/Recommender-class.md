# Class "Recommender": A Recommender Model

Represents a recommender model learned for a given data set (a rating
matrix).

## Objects from the Class

Objects are created by the creator function
`Recommender(data, method, parameter = NULL)`

## Slots

- `method`::

  Object of class `"character"`; used recommendation method.

- `dataType`::

  Object of class `"character"`; concrete class of the input data.

- `ntrain`::

  Object of class `"integer"`; size of training set.

- `model`::

  Object of class `"list"`; the model.

- `predict`::

  Object of class `"function"`; code to compute a recommendation using
  the model.

## Methods

- getModel:

  `signature(x = "Recommender")`: retrieve the model.

- predict:

  `signature(object = "Recommender")`: create recommendations for new
  data (argument `newdata`).

- show:

  `signature(object = "Recommender")`

## See also

See
[`Recommender`](http://michael.hahsler.net/recommenderlab/reference/Recommender.md)
for the constructor function and a description of availble methods.
