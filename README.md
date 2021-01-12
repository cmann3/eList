
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eList

<!-- badges: start -->

<!-- badges: end -->

Write list comprehensions in R\!

The eList package allows users to write vectorized `for` loops and
contains a variety of tools for working with lists and other vectors.
Just wrap a normal `for` loop within one of the comprehension functions,
such as `List()`, and let the package do the rest.

Features include, but are not limited to:

  - Multiple variables within the loop using `"."` to separate names.
  - Multiple returns types: list, numeric vectors, character vectors,
    matrices, environments, etc.
  - `if`, `else` statements to filter results.
  - Name assignment within the loop using `=`.
  - Parallelization by adding the option `, clust =` and the cluster.
  - Nested loops.
  - Helper functions such as `enum` and `items` to access the index or
    name within the loop, or `zip` objects together.
  - Summary comprehensions that calculate sums, means, etc. from `for`
    loops.
  - Use either `List(...)` or `List[...]` for the comprehension.
  - Higher order functions, such as map or filter, that accept functions
    built from formulas *(using `.` notation for variables)* or calls.

## Installation

You can install the released version of eList from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("eList")
```

## Examples

A simple “list” comprehension that accumulates all integer sequences to
4 using the `List` function. Though it looks like a `for` loop, it is
actually using `lapply` behind the scenes.

``` r
library(eList)
#> 
#> Attaching package: 'eList'
#> The following object is masked from 'package:stats':
#> 
#>     filter
#> The following object is masked from 'package:utils':
#> 
#>     zip
List(for (i in 1:4) 1:i)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 1 2 3
#> 
#> [[4]]
#> [1] 1 2 3 4
```

Loops can be nested and filtered using `if` statements. The example
below uses `Num` to produce a numeric vector rather than a list. Other
comprehensions include `Chr` for character vectors, `Logical` for
logical vectors, `Vec` for flat *(non-list)* vectors, etc.

``` r
Num(for (i in 1:4) for (j in 3:6) if (i == j) i^2)
#> [1]  9 16
```

Use the “dot” notation to use multiple variables within the loop.

``` r
Chr(for (i.j in zip(1:4, 5:8)) paste0(i, j))
#> [1] "15" "26" "37" "48"
```

Use `=` within the loop to assign a name to each item within the list,
or other item.

``` r
values <- zip(letters[1:4], 5:8)

List(for (i.j in values) i = j)
#> $a
#> [1] "5"
#> 
#> $b
#> [1] "6"
#> 
#> $c
#> [1] "7"
#> 
#> $d
#> [1] "8"
```

Parallelization is also very easy. Just create a cluster and add it to
the comprehension with the `clust` argument.

``` r
my_cluster <- auto_cluster()

x <- Num(for (i in sample(1:100, 50)) sqrt(i), clust = my_cluster)

# Close the cluster if not needed!
close_cluster(my_cluster)

x
#>  [1] 3.872983 9.949874 3.316625 6.633250 7.000000 9.165151 9.899495 9.539392
#>  [9] 9.055385 3.464102 5.830952 9.486833 3.741657 8.660254 4.898979 8.000000
#> [17] 4.690416 5.477226 5.385165 5.000000 8.485281 8.246211 4.358899 7.416198
#> [25] 8.717798 9.591663 9.433981 7.549834 3.162278 9.797959 7.615773 3.000000
#> [33] 9.746794 2.236068 5.291503 8.124038 9.219544 9.695360 7.483315 9.643651
#> [41] 3.605551 4.000000 1.732051 5.196152 2.828427 6.855655 7.681146 7.348469
#> [49] 7.211103 7.280110
```

Want a statistical summary using a comprehension? eList contains a
variety of summary functions for that purpose. `Stats` is a general
summary comprehension that computes many different values.

``` r
Stats(for (i in sample(1:100, 50)) sqrt(i))
#> $min
#> [1] 2
#> 
#> $q1
#> [1] 6.143211
#> 
#> $med
#> [1] 7.483016
#> 
#> $q3
#> [1] 8.587745
#> 
#> $max
#> [1] 10
#> 
#> $mean
#> [1] 7.172598
#> 
#> $sd
#> [1] 2.028831
```

eList also contains functional programming style functions for working
with lists and other vectors. These functions perform an operation using
a function on another object. They are similar to the higher order
functions in Base R, but are pipe-friendly, handle a wide ranger of
object types, and allow for different methods of specifying functions.

``` r
x <- list(1:4, 5:8, 9:12)
map(x, mean)
#> [[1]]
#> [1] 2.5
#> 
#> [[2]]
#> [1] 6.5
#> 
#> [[3]]
#> [1] 10.5
```

This can also be calculated using formula notation. Formulas can be be
written as either a two-sided formula or a one-sided formula by
prefixing variables with dots.

``` r
# Two-sided Formula
map(x, i ~ sqrt(i) + 1)
#> [[1]]
#> [1] 2.000000 2.414214 2.732051 3.000000
#> 
#> [[2]]
#> [1] 3.236068 3.449490 3.645751 3.828427
#> 
#> [[3]]
#> [1] 4.000000 4.162278 4.316625 4.464102

# One-sided Formula
map(x, ~ sqrt(.i) + 1)
#> [[1]]
#> [1] 2.000000 2.414214 2.732051 3.000000
#> 
#> [[2]]
#> [1] 3.236068 3.449490 3.645751 3.828427
#> 
#> [[3]]
#> [1] 4.000000 4.162278 4.316625 4.464102
```

The higher order functions also accept unevaluated “calls”.

``` r
round2 <- substitute(round(digits=2))
map(rnorm(5), round2)
#> [[1]]
#> [1] 0.16
#> 
#> [[2]]
#> [1] -3.18
#> 
#> [[3]]
#> [1] -0.17
#> 
#> [[4]]
#> [1] -0.13
#> 
#> [[5]]
#> [1] -0.27
```
