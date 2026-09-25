# Internal Utility Functions

Utility functions used internally by recommender algorithms. See files
starting with `RECOM` in the package's `R` directory for examples of
usage.

## Usage

``` r
returnRatings(ratings, newdata,
  type = c("topNList", "ratings", "ratingMatrix"),
  n, randomize = NULL, minRating = NA)

getParameters(defaults, parameter)
```

## Arguments

- ratings:

  a realRatingMatrix.

- newdata:

  a realRatingMatrix.

- type:

  type of recommendation to return.

- n:

  max. number of entries in the top-N list.

- randomize:

  randomization factor for producing the top-N list.

- minRating:

  do not include ratings less than this.

- defaults:

  list with parameters and default values.

- parameter:

  list with actual parameters.

## Details

`returnRatings` is used in the predict function of recommender
algorithms to return different types of recommendations.

`getParameters` is a helper function which checks parameters for
consistency and provides default values. Used in the Recommender
constructor.
