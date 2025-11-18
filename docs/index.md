# Homework6

`Homework6` provides an S4 class (`sparse_numeric`) for representing
sparse numeric vectors along with efficient operations such as addition,
subtraction, elementwise multiplication, cross-products, mean, norm, and
standardization. The package is useful when working with numeric vectors
that contain many zeros, allowing you to store only the non-zero
entries.

## Installation

You can install the development version of **Homework6** from GitHub

``` r

# install.packages("pak")
pak::pak("nikhildevireddy/Homework6")
```

## Example

Below is a basic example showing how to construct sparse vectors and
carry out arithmetic operations:

``` r

library(Homework6)

# Create two numeric vectors
x <- c(0, 5, 0, 3)
y <- c(2, 0, 0, 7)

# Convert to sparse_numeric
sx <- as(x, "sparse_numeric")
sy <- as(y, "sparse_numeric")

# Addition
sparse_add(sx, sy)
```

``` R
## An object of class 'sparse_numeric'
##  Length: 4
##  Non-zero entries: 3 
##  pos value
##    1     2
##    2     5
##    4    10
```

``` r

#> An object of class sparse_numeric ...

# Elementwise multiplication
sparse_mult(sx, sy)
```

``` R
## An object of class 'sparse_numeric'
##  Length: 4
##  Non-zero entries: 1 
##  pos value
##    4    21
```

``` r

# Compute the Euclidean norm
norm(sx)
```

``` R
## [1] 5.830952
```

``` r

# Standardize the vector
standardize(sx)
```

``` R
## An object of class 'sparse_numeric'
##  Length: 4
##  Non-zero entries: 4 
##  pos      value
##    1 -0.8164966
##    2  1.2247449
##    3 -0.8164966
##    4  0.4082483
```

## What the package provides

- A compact S4 representation of sparse numeric vectors  
- Vectorized arithmetic: `+`, `-`, `*`  
- Generic functions:   -
  [`sparse_add()`](https://nikhildevireddy.github.io/SDS375_Homework6/reference/sparse_add.md)
    -
  [`sparse_sub()`](https://nikhildevireddy.github.io/SDS375_Homework6/reference/sparse_sub.md)
    -
  [`sparse_mult()`](https://nikhildevireddy.github.io/SDS375_Homework6/reference/sparse_mult.md)
    -
  [`sparse_crossprod()`](https://nikhildevireddy.github.io/SDS375_Homework6/reference/sparse_crossprod.md)
    - `nnzero()`   -
  [`standardize()`](https://nikhildevireddy.github.io/SDS375_Homework6/reference/standardize.md)
    -
  [`norm()`](https://nikhildevireddy.github.io/SDS375_Homework6/reference/norm.md)  
- A custom [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method for visualizing overlapping non-zero entries  
- Full documentation and examples on the package website (built with
  pkgdown)

## Development

This package was built for the UT Austin SDS 375 Data Product course and
includes:

- Test coverage using **testthat**
- Automated documentation using **roxygen2**
- A pkgdown website
- GitHub Actions CI (R CMD check)
