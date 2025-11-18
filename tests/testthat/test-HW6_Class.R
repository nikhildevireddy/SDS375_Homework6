library(testthat)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 0, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 0, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})
## NEW TESTS
test_that("mean generic exists", {
  expect_true(isGeneric("mean"))
})

test_that("mean method for sparse_numeric exists", {
  expect_no_error({
    getMethod("mean", "sparse_numeric")
  })
})

test_that("mean for sparse_numeric matches dense mean", {
  expect_equal({
    x <- c(0, 1, 0, 3, -2)
    s <- as(x, "sparse_numeric")
    mean(s)
  }, {
    x <- c(0, 1, 0, 3, -2)
    mean(x)
  })
})

test_that("mean for empty sparse_numeric returns NaN", {
  expect_true({
    x <- new("sparse_numeric",
             value = numeric(0),
             pos = integer(0),
             length = 0L)
    is.nan(mean(x))
  })
})

test_that("norm generic exists", {
  expect_true(isGeneric("norm"))
})

test_that("norm method for sparse_numeric exists", {
  expect_no_error({
    getMethod("norm", "sparse_numeric")
  })
})

test_that("norm for sparse_numeric matches sqrt(sum(x^2))", {
  expect_equal({
    x <- c(0, 1, -2, 0, 4)
    s <- as(x, "sparse_numeric")
    norm(s)
  }, {
    x <- c(0, 1, -2, 0, 4)
    sqrt(sum(x^2))
  })
})

test_that("standardize generic exists", {
  expect_true(isGeneric("standardize"))
})

test_that("standardize method for sparse_numeric exists", {
  expect_no_error({
    getMethod("standardize", "sparse_numeric")
  })
})

test_that("standardize matches scale() on dense vector", {
  expect_equal({
    x <- c(0, 1, -2, 4, 0, 3)
    s <- as(x, "sparse_numeric")
    as(standardize(s), "numeric")
  }, {
    x <- c(0, 1, -2, 4, 0, 3)
    as.numeric(scale(x))
  })
})

test_that("standardize errors for length <= 1", {
  expect_error({
    x <- new("sparse_numeric",
             value = numeric(0),
             pos = integer(0),
             length = 1L)
    standardize(x)
  })
})

test_that("standardize errors for constant vector", {
  expect_error({
    x <- rep(5, 10)
    s <- as(x, "sparse_numeric")
    standardize(s)
  })
})


test_that("validity enforces scalar non-negative length", {
  expect_error({
    new("sparse_numeric",
        value = numeric(0),
        pos = integer(0),
        length = c(2L, 3L))
  }, "length")
  expect_error({
    new("sparse_numeric",
        value = numeric(0),
        pos = integer(0),
        length = as.integer(NA))
  }, "length")
  expect_error({
    new("sparse_numeric",
        value = numeric(0),
        pos = integer(0),
        length = -1L)
  }, "length")
})

test_that("validity requires matching value/pos without NA", {
  expect_error({
    new("sparse_numeric",
        value = c(1, 2),
        pos = 1L,
        length = 5L)
  }, "same length")
  expect_error({
    new("sparse_numeric",
        value = 1,
        pos = 6L,
        length = 5L)
  }, "between 1")
})

test_that("coercion handles empty and dense numeric vectors", {
  empty_sparse <- as(numeric(0), "sparse_numeric")
  expect_s4_class(empty_sparse, "sparse_numeric")
  expect_equal(empty_sparse@length, 0L)
  expect_equal(empty_sparse@pos, integer(0))
  expect_equal(empty_sparse@value, numeric(0))

  dense <- c(0, 2, 0, -3)
  s <- as(dense, "sparse_numeric")
  expect_equal(s@pos, c(2L, 4L))
  expect_equal(s@value, c(2, -3))
  expect_equal(as(s, "numeric"), dense)
})

test_that("sparse_add handles zero vectors and cancels entries", {
  zero1 <- as(rep(0, 4), "sparse_numeric")
  zero2 <- as(rep(0, 4), "sparse_numeric")
  sum_zero <- sparse_add(zero1, zero2)
  expect_equal(sum_zero@pos, integer(0))
  expect_equal(sum_zero@value, numeric(0))

  x <- as(c(1, 0, -2, 0), "sparse_numeric")
  y <- as(c(-1, 0, 2, 0), "sparse_numeric")
  cancel <- sparse_add(x, y)
  expect_equal(cancel@pos, integer(0))
  expect_equal(cancel@value, numeric(0))
})

test_that("operator arithmetic dispatches to sparse methods", {
  x <- as(c(0, 1, 2), "sparse_numeric")
  y <- as(c(3, 0, -2), "sparse_numeric")
  expect_equal(as(x + y, "numeric"), c(3, 1, 0))
  expect_equal(as(x - y, "numeric"), c(-3, 1, 4))
  expect_equal(as(x * y, "numeric"), c(0, 0, -4))
})

test_that("sparse_sub returns zero vector when inputs match", {
  x <- as(c(0, 5, -1), "sparse_numeric")
  expect_equal(sparse_sub(x, x)@pos, integer(0))
})

test_that("sparse_mult covers empty, disjoint, and overlapping cases", {
  empty <- as(rep(0, 3), "sparse_numeric")
  y <- as(c(1, 0, 2), "sparse_numeric")
  expect_equal(sparse_mult(empty, y)@pos, integer(0))

  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 0), "sparse_numeric")
  expect_equal(sparse_mult(x, y)@pos, integer(0))

  x <- as(c(0, 3, 4, 0), "sparse_numeric")
  y <- as(c(5, 0, 2, 0), "sparse_numeric")
  prod_sparse <- sparse_mult(x, y)
  expect_equal(as(prod_sparse, "numeric"), c(0, 0, 8, 0))

  x <- new("sparse_numeric",
           value = 0,
           pos = 2L,
           length = 3L)
  y <- as(c(0, 5, 0), "sparse_numeric")
  prod_zero <- sparse_mult(x, y)
  expect_equal(prod_zero@pos, integer(0))
  expect_equal(prod_zero@value, numeric(0))
})

test_that("sparse_crossprod covers zero, disjoint, and overlapping", {
  empty <- as(rep(0, 5), "sparse_numeric")
  y <- as(c(1, 0, 0, 2, 0), "sparse_numeric")
  expect_equal(sparse_crossprod(empty, y), 0)

  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(0, 3, 0), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 0)

  x <- as(c(0, 4, 0, -1), "sparse_numeric")
  y <- as(c(2, 0, 0, 3), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), -3)
})

test_that("show prints summaries for zero and non-zero objects", {
  zero <- as(rep(0, 3), "sparse_numeric")
  out_zero <- capture.output(show(zero))
  expect_true(any(grepl("all entries are zero", out_zero)))

  nz <- as(c(0, 5, -1), "sparse_numeric")
  out_nz <- capture.output(show(nz))
  expect_true(any(grepl("Non-zero entries: 2", out_nz)))
  expect_true(any(grepl("pos", out_nz)))
})

test_that("plot method returns invisibly", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  y <- as(c(0, 0, 0, 0), "sparse_numeric")
  expect_invisible(plot(x, y))

  y2 <- as(c(3, 0, 4, 0), "sparse_numeric")
  expect_invisible(plot(x, y2))
})

test_that("length method returns stored length", {
  x <- new("sparse_numeric",
           value = c(1, 2),
           pos = c(1L, 3L),
           length = 5L)
  expect_identical(length(x), 5L)
})

test_that("standardize handles fully non-zero vectors", {
  x <- as(1:5, "sparse_numeric")
  expect_equal(as(standardize(x), "numeric"), as.numeric(scale(1:5)))
})
