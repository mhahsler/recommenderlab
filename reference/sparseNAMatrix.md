# Sparse Matrix Representation With NAs Not Explicitly Stored

Coerce from and to a sparse matrix representation where `NA`s are not
explicitly stored.

## Usage

``` r
dropNA(x)
dropNA2matrix(x)
dropNAis.na(x)
```

## Arguments

- x:

  a matrix for `dropNA()`, or a sparse matrix with dropped NA values for
  `dropNA2matrix()` or `dropNAis.na()`.

## Details

The representation is based on the sparse `dgCMatrix` in Matrix but
instead of zeros, `NA`s are dropped. This is achieved by the following:

- Zeros are represented with a very small value (`.Machine$double.xmin`)
  so they do not get dropped in the sparse representation.

- NAs are converted to 0 before cercions to `dgCMatrix` to make them not
  explicitly stored.

**Caution:** Be careful when working with the sparse matrix and sparse
matrix operations (multiplication, addition, etc.) directly.

- Sparse matrix operations will see 0 where NAs should be.

- Actual zero ratings have a small, but non-zero value
  (`.Machine$double.xmin`).

- Sparse matrix operations that can result in a true 0 need to be
  followed by replacing the 0 with `.Machine$double.xmin` or other
  operations (like subsetting) may drop the 0.

`dropNAis.na()` correctly finds NA values in a sparse matrix with
dropped NA values, while [`is.na()`](https://rdrr.io/r/base/NA.html)
does not work.

`dropNA2matrix()` converts the sparse representation into a dense
matrix. NAs represented by dropped values are converted to true NAs.
Zeros are recovered by using
[`zapsmall()`](https://rdrr.io/r/base/zapsmall.html) which replaces
small values by 0.

## Value

Returns a dgCMatrix or a matrix, respectively.

## See also

[`dgCMatrix`](https://rdrr.io/pkg/Matrix/man/dgCMatrix-class.html) in
Matrix.

## Examples

``` r
m <- matrix(sample(c(NA,0:5),50, replace=TRUE, prob=c(.5,rep(.5/6,6))),
    nrow=5, ncol=10, dimnames = list(users=paste('u', 1:5, sep=''),
    items=paste('i', 1:10, sep='')))
m
#>      items
#> users i1 i2 i3 i4 i5 i6 i7 i8 i9 i10
#>    u1 NA  4 NA  4 NA NA  2  0  0   0
#>    u2 NA  3  1  5 NA NA  5  1  4   1
#>    u3  4  0  5 NA NA NA NA NA NA   2
#>    u4  4 NA NA NA  0  0 NA  0  0   0
#>    u5  4  3 NA NA NA NA NA NA NA   0

## drop all NAs in the representation. Zeros are represented by very small values.
sparse <- dropNA(m)
sparse
#> 5 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘i1’, ‘i2’, ‘i3’ ... ]]
#>      items
#> users                                                                
#>    u1 .  4.000000e+00 . 4 .             .             2 2.225074e-308
#>    u2 .  3.000000e+00 1 5 .             .             5  1.000000e+00
#>    u3 4 2.225074e-308 5 . .             .             . .            
#>    u4 4 .             . . 2.225074e-308 2.225074e-308 . 2.225074e-308
#>    u5 4  3.000000e+00 . . .             .             . .            
#>      items
#> users                            
#>    u1 2.225074e-308 2.225074e-308
#>    u2  4.000000e+00  1.000000e+00
#>    u3 .              2.000000e+00
#>    u4 2.225074e-308 2.225074e-308
#>    u5 .             2.225074e-308

## convert back to matrix
dropNA2matrix(sparse)
#>      items
#> users i1 i2 i3 i4 i5 i6 i7 i8 i9 i10
#>    u1 NA  4 NA  4 NA NA  2  0  0   0
#>    u2 NA  3  1  5 NA NA  5  1  4   1
#>    u3  4  0  5 NA NA NA NA NA NA   2
#>    u4  4 NA NA NA  0  0 NA  0  0   0
#>    u5  4  3 NA NA NA NA NA NA NA   0

## Note: be careful with the sparse representation!
## Do not use is.na, but use
dropNAis.na(sparse)
#> 5 x 10 Matrix of class "lgeMatrix"
#>      items
#> users    i1    i2    i3    i4    i5    i6    i7    i8    i9   i10
#>    u1  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
#>    u2  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE
#>    u3 FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
#>    u4 FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
#>    u5 FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
```
