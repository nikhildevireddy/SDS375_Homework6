
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Homework6

<!-- badges: start -->

[![R-CMD-check](https://github.com/nikhildevireddy/SDS375_Homework6/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/nikhildevireddy/SDS375_Homework6/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

`Homework6` offers an S4 sparse_numeric class for compactly storing
sparse numeric vectors plus fast operations—addition, subtraction,
elementwise products, cross-products, mean, norm, and standardization.
It’s designed for zero-heavy vectors so you keep only the nonzero
entries while still working with them efficiently.

## Installation

You can install the development version of **Homework6** from GitHub

``` r
# install.packages("pak")
pak::pak("nikhildevireddy/Homework6")
```

## Examples

How the package is used to construct sparse vectors and execute
arithmetic operations:

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

    ## An object of class 'sparse_numeric'
    ##  Length: 4
    ##  Non-zero entries: 3 
    ##  pos value
    ##    1     2
    ##    2     5
    ##    4    10

``` r
# Elementwise multiplication
sparse_mult(sx, sy)
```

    ## An object of class 'sparse_numeric'
    ##  Length: 4
    ##  Non-zero entries: 1 
    ##  pos value
    ##    4    21

``` r
# Compute the norm
norm(sx)
```

    ## [1] 5.830952

``` r
# Standardize the vector
standardize(sx)
```

    ## An object of class 'sparse_numeric'
    ##  Length: 4
    ##  Non-zero entries: 4 
    ##  pos      value
    ##    1 -0.8164966
    ##    2  1.2247449
    ##    3 -0.8164966
    ##    4  0.4082483

## Package Methods

- A compact S4 representation of sparse numeric vectors  
- Vectorized arithmetic: `+`, `-`, `*`  
- Generic functions:   - `sparse_add()`   - `sparse_sub()`   -
  `sparse_mult()`   - `sparse_crossprod()`   - `nnzero()`   -
  `standardize()`   - `norm()`  
- A custom `plot()` method for visualizing overlapping non-zero
  entries  
- Full documentation and examples on the package website (built with
  pkgdown)

## Development

This package was built for the UT Austin SDS 375 Data Product
Development course and includes:

- Test coverage using **testthat**
- Automated documentation using **roxygen2**
- A hosted website
- GitHub Actions CI (R CMD check)
