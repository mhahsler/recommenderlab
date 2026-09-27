# Plot Evaluation Results

Creates precision-recall or ROC plots for recommender evaluation
results.

## Usage

``` r
# S4 method for class 'evaluationResults'
plot(x, y,
        avg = TRUE, add=FALSE, type= "b", annotate = FALSE, ...)
# S4 method for class 'evaluationResultList'
plot(x, y,
        xlim=NULL, ylim=NULL, col = NULL, pch = NULL, lty = 1,
        avg = TRUE, type = "b", annotate= 0, legend="bottomright", ...)
```

## Arguments

- x:

  the object to be plotted.

- y:

  a character string indicating the type of plot (e.g., "ROC" or
  "prec/rec").

- avg:

  plot average of runs?

- add:

  add to a plot?

- type:

  line type (see `plot`).

- annotate:

  annotate N (recommendation list size) to plot.

- ...:

  further arguments passed on to `plot`.

- xlim, ylim:

  plot limits (see `plot`).

- col:

  colors (see `plot`).

- pch:

  point symbol to use (see `plot`).

- lty:

  line type (see `plot`)

- legend:

  where to place legend (see `legend`).

## See also

[`evaluationResults`](http://michael.hahsler.net/recommenderlab/reference/evaluationResults-class.md),
[`evaluationResultList`](http://michael.hahsler.net/recommenderlab/reference/evaluationResultList-class.md).
See
[`evaluate`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md)
for examples.

Other evaluation:
[`calcPredictionAccuracy()`](http://michael.hahsler.net/recommenderlab/reference/calcPredictionAccuracy.md),
[`error`](http://michael.hahsler.net/recommenderlab/reference/error.md),
[`evaluate()`](http://michael.hahsler.net/recommenderlab/reference/evaluate.md),
[`evaluationResultList-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationResultList-class.md),
[`evaluationResults-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationResults-class.md),
[`evaluationScheme()`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme.md),
[`evaluationScheme-class`](http://michael.hahsler.net/recommenderlab/reference/evaluationScheme-class.md)
