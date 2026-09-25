# List and Data.frame Representation for Recommender Matrix Objects

Create a list or data.frame representation for various objects used in
recommenderlab. These functions are used in addition to available
coercion to allow for parameters like `decode`.

## Usage

``` r
getList(from, ...)
# S4 method for class 'realRatingMatrix'
getList(from, decode = TRUE, ratings = TRUE, ...)
# S4 method for class 'binaryRatingMatrix'
getList(from, decode = TRUE, ...)
# S4 method for class 'topNList'
getList(from, decode = TRUE, ...)

getData.frame(from, ...)
# S4 method for class 'ratingMatrix'
getData.frame(from, decode = TRUE, ratings = TRUE, ...)
```

## Arguments

- from:

  object to be represented as a list.

- decode:

  use item names or item IDs (column numbers) for items?

- ratings:

  include ratings in the list or data.frame?

- ...:

  further arguments (currently unused).

## Details

Lists have one vector with items (and ratings) per user. The data.frame
has one row per rating with the user in the first column, the item as
the second and the rating as the third.

## Value

Returns a list or a data.frame.

## See also

[`binaryRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/binaryRatingMatrix-class.md),
[`realRatingMatrix`](http://michael.hahsler.net/recommenderlab/reference/realRatingMatrix-class.md),
[`topNList`](http://michael.hahsler.net/recommenderlab/reference/topNList-class.md).

## Examples

``` r
data(Jester5k)

getList(Jester5k[1,])
#> $`0`
#>    j1    j2    j3    j4    j5    j6    j7    j8    j9   j10   j11   j12   j13 
#> -1.60 -3.54  4.17  1.84 -0.44 -0.78 -5.00  3.30  1.12  1.41  2.14  4.13 -1.99 
#>   j14   j15   j16   j17   j18   j19   j20   j21   j22   j23   j24   j25   j26 
#>  1.26 -4.27 -4.27 -4.27 -7.23 -3.69 -2.82  0.49 -0.92 -2.62  1.84 -6.12 -0.83 
#>   j27   j28   j29   j30   j31   j32   j33   j34   j35   j36   j37   j38   j39 
#>  2.62 -3.69 -3.16  2.09  2.18 -2.72  0.73  1.89 -4.71 -1.89 -3.54  0.78 -0.97 
#>   j40   j41   j42   j43   j44   j45   j46   j47   j48   j49   j50   j51   j52 
#>  0.68  0.68  1.12  2.91 -3.16 -4.17  4.90  1.75 -0.19  0.83  3.06 -1.46  2.77 
#>   j53   j54   j55   j56   j57   j58   j59   j60   j61   j62   j63   j64   j65 
#>  1.12  4.08  3.64  3.01 -3.16 -3.06  4.17  0.53  1.99  1.21 -0.73  2.18 -2.38 
#>   j66   j67   j68   j69   j70   j71   j72   j73   j74   j75   j76   j77   j78 
#> -2.57  1.50  0.73  1.99  2.62 -2.72  4.95  4.22 -3.74  1.80  0.92  2.86  4.51 
#>   j79   j80   j81   j82   j83   j84   j85   j86   j87   j88   j89   j90   j91 
#>  2.91  4.03  2.91  0.87  5.49 -2.28  1.26  1.26  2.33  3.83  4.61 -1.41 -2.91 
#>   j92   j93   j94   j95   j96   j97   j98   j99  j100 
#> -1.75  2.38  2.77  2.57  2.91  0.63  3.40  1.65 -4.08 
#> 
getData.frame(Jester5k[1,])
#>      user item rating
#> 1   u7452   j1  -1.60
#> 2   u7452   j2  -3.54
#> 3   u7452   j3   4.17
#> 4   u7452   j4   1.84
#> 5   u7452   j5  -0.44
#> 6   u7452   j6  -0.78
#> 7   u7452   j7  -5.00
#> 8   u7452   j8   3.30
#> 9   u7452   j9   1.12
#> 10  u7452  j10   1.41
#> 11  u7452  j11   2.14
#> 12  u7452  j12   4.13
#> 13  u7452  j13  -1.99
#> 14  u7452  j14   1.26
#> 15  u7452  j15  -4.27
#> 16  u7452  j16  -4.27
#> 17  u7452  j17  -4.27
#> 18  u7452  j18  -7.23
#> 19  u7452  j19  -3.69
#> 20  u7452  j20  -2.82
#> 21  u7452  j21   0.49
#> 22  u7452  j22  -0.92
#> 23  u7452  j23  -2.62
#> 24  u7452  j24   1.84
#> 25  u7452  j25  -6.12
#> 26  u7452  j26  -0.83
#> 27  u7452  j27   2.62
#> 28  u7452  j28  -3.69
#> 29  u7452  j29  -3.16
#> 30  u7452  j30   2.09
#> 31  u7452  j31   2.18
#> 32  u7452  j32  -2.72
#> 33  u7452  j33   0.73
#> 34  u7452  j34   1.89
#> 35  u7452  j35  -4.71
#> 36  u7452  j36  -1.89
#> 37  u7452  j37  -3.54
#> 38  u7452  j38   0.78
#> 39  u7452  j39  -0.97
#> 40  u7452  j40   0.68
#> 41  u7452  j41   0.68
#> 42  u7452  j42   1.12
#> 43  u7452  j43   2.91
#> 44  u7452  j44  -3.16
#> 45  u7452  j45  -4.17
#> 46  u7452  j46   4.90
#> 47  u7452  j47   1.75
#> 48  u7452  j48  -0.19
#> 49  u7452  j49   0.83
#> 50  u7452  j50   3.06
#> 51  u7452  j51  -1.46
#> 52  u7452  j52   2.77
#> 53  u7452  j53   1.12
#> 54  u7452  j54   4.08
#> 55  u7452  j55   3.64
#> 56  u7452  j56   3.01
#> 57  u7452  j57  -3.16
#> 58  u7452  j58  -3.06
#> 59  u7452  j59   4.17
#> 60  u7452  j60   0.53
#> 61  u7452  j61   1.99
#> 62  u7452  j62   1.21
#> 63  u7452  j63  -0.73
#> 64  u7452  j64   2.18
#> 65  u7452  j65  -2.38
#> 66  u7452  j66  -2.57
#> 67  u7452  j67   1.50
#> 68  u7452  j68   0.73
#> 69  u7452  j69   1.99
#> 70  u7452  j70   2.62
#> 71  u7452  j71  -2.72
#> 72  u7452  j72   4.95
#> 73  u7452  j73   4.22
#> 74  u7452  j74  -3.74
#> 75  u7452  j75   1.80
#> 76  u7452  j76   0.92
#> 77  u7452  j77   2.86
#> 78  u7452  j78   4.51
#> 79  u7452  j79   2.91
#> 80  u7452  j80   4.03
#> 81  u7452  j81   2.91
#> 82  u7452  j82   0.87
#> 83  u7452  j83   5.49
#> 84  u7452  j84  -2.28
#> 85  u7452  j85   1.26
#> 86  u7452  j86   1.26
#> 87  u7452  j87   2.33
#> 88  u7452  j88   3.83
#> 89  u7452  j89   4.61
#> 90  u7452  j90  -1.41
#> 91  u7452  j91  -2.91
#> 92  u7452  j92  -1.75
#> 93  u7452  j93   2.38
#> 94  u7452  j94   2.77
#> 95  u7452  j95   2.57
#> 96  u7452  j96   2.91
#> 97  u7452  j97   0.63
#> 98  u7452  j98   3.40
#> 99  u7452  j99   1.65
#> 100 u7452 j100  -4.08
```
