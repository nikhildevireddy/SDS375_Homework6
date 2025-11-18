# Standardize sparse_numeric vector

Computes (x - mean) / sd without dense coercion.

Standardize a sparse_numeric vector.

## Usage

``` r
standardize(x, ...)

# S4 method for class 'sparse_numeric'
standardize(x, ...)
```

## Arguments

- x:

  A sparse_numeric object.

- ...:

  Ignored.

## Value

A sparse_numeric object.

## Details

Computes (x - mean) / sd over all entries including zeros.
