## HW6 Class/Methods

#' Sparse numeric internal
#' @name sparse_numeric-internal
#' @keywords internal
#' @import methods
#' @importFrom graphics plot points
NULL

#' Sparse numeric vector class
#' @description An S4 class for representing numeric vectors in sparse form.
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions of the non-zero values.
#' @slot length Single integer giving the length of the full vector.
#'
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  len <- object@length
  pos <- object@pos
  vals <- object@value

  if (length(len) != 1L || is.na(len) || len < 0L) {
    return("slot 'length' must be a single non-negative integer")
  }

  if (length(vals) != length(pos)) {
    return("slots 'value' and 'pos' must have the same length")
  }

  if (length(pos)) {
    if (anyNA(pos)) {
      return("slot 'pos' cannot contain NA")
    }
    if (any(pos < 1L) || any(pos > len)) {
      return("slot 'pos' must be between 1 and 'length'")
    }
  }

  TRUE
})


#' Coerce numeric to sparse_numeric
#'
#' @name coerce-numeric-sparse_numeric
#' @description Convert a dense numeric vector to a sparse_numeric vector.
#'
#' @param from A numeric vector.
#'
#' @return A sparse_numeric object.
#'
#' @aliases coerce,numeric,sparse_numeric-method
#' @exportMethod coerce
setAs("numeric", "sparse_numeric", function(from) {
  n <- length(from)
  if (n == 0L) {
    return(new("sparse_numeric",
               value = numeric(0),
               pos = integer(0),
               length = 0L))
  }
  nz <- which(from != 0)
  new("sparse_numeric",
      value = if (length(nz) > 0L) from[nz] else numeric(0),
      pos = as.integer(nz),
      length = as.integer(n))
})


#' Coerce sparse_numeric to numeric
#'
#' @name coerce-sparse_numeric-numeric
#' @description Convert a sparse_numeric vector to a dense numeric vector.
#'
#' @param from A sparse_numeric object.
#'
#' @return A numeric vector.
#'
#' @aliases coerce,sparse_numeric,numeric-method
#' @exportMethod coerce
setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos) > 0L) {
    out[from@pos] <- from@value
  }
  out
})

## Helper: check matching length ---------------------------------------

.check_same_length <- function(x, y) {
  if (x@length != y@length) {
    stop("sparse_numeric objects must have the same 'length'")
  }
}

## Generics ------------------------------------------------------------

#' Addition for sparse_numeric vectors
#' @description Add two sparse vectors
#' @name sparse_add
#' @rdname sparse_add
#'
#' @param x, sparse numeric vector
#' @param y, sparse numeric vector
#'
#' @return A \code{sparse_numeric} containing the elementwise sum.
#' @export
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

#' Subtraction for sparse_numeric vectors
#' @description Subtract two sparse vectors
#' @name sparse_sub
#' @rdname sparse_sub
#' @param x, sparse numeric vector
#' @param y, sparse numeric vector
#'
#' @return A \code{sparse_numeric} containing the elementwise difference.
#' @export
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

#' Multiplication for sparse_numeric vectors
#' @description Elementwise multiply two sparse vectors
#' @name sparse_mult
#' @rdname sparse_mult
#' @param x, sparse numeric vector
#' @param y, sparse numeric vector.
#'
#' @return A \code{sparse_numeric} containing the elementwise product.
#' @export
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' Cross product for sparse_numeric vectors
#' @description Sparse crossproduct (dot product)
#' @name sparse_crossprod
#' @rdname sparse_crossprod
#' @param x, sparse numeric vector
#' @param y, sparse numeric vector
#'
#' @return A numeric scalar giving the dot product.
#' @export
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

#' Addition for sparse_numeric vectors
#' @description Add two sparse_numeric vectors.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#'
#' @return A sparse_numeric object containing the sum.
#'
#' @exportMethod sparse_add
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)

            if (length(x@pos) == 0L && length(y@pos) == 0L) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            all_pos <- c(x@pos, y@pos)
            all_val <- c(x@value, y@value)

            sums <- tapply(all_val, all_pos, sum)

            pos <- as.integer(names(sums))
            vals <- as.numeric(sums)

            nz <- vals != 0
            if (!any(nz)) {
              new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length)
            } else {
              new("sparse_numeric",
                  value = vals[nz],
                  pos = pos[nz],
                  length = x@length)
            }
          })

#' Subtraction for sparse_numeric vectors
#' @description Subtract one sparse_numeric vector from another.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#'
#' @return A sparse_numeric object containing the difference.
#'
#' @exportMethod sparse_sub
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)

            if (length(x@pos) == 0L && length(y@pos) == 0L) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            all_pos <- c(x@pos, y@pos)
            all_val <- c(x@value, -y@value)

            diffs <- tapply(all_val, all_pos, sum)

            pos <- as.integer(names(diffs))
            vals <- as.numeric(diffs)

            nz <- vals != 0
            if (!any(nz)) {
              new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length)
            } else {
              new("sparse_numeric",
                  value = vals[nz],
                  pos = pos[nz],
                  length = x@length)
            }
          })

#' Multiplication for sparse_numeric vectors
#' @description Multiply two sparse_numeric vectors elementwise.
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#'
#' @return A sparse_numeric object with elementwise products.
#'
#' @exportMethod sparse_mult
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)

            if (length(x@pos) == 0L || length(y@pos) == 0L) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            pos_common <- intersect(x@pos, y@pos)
            if (length(pos_common) == 0L) {
              return(new("sparse_numeric",
                         value = numeric(0),
                         pos = integer(0),
                         length = x@length))
            }

            idx_x <- match(pos_common, x@pos)
            idx_y <- match(pos_common, y@pos)
            vals <- x@value[idx_x] * y@value[idx_y]

            nz <- which(vals != 0)
            if (length(nz) == 0L) {
              new("sparse_numeric",
                  value = numeric(0),
                  pos = integer(0),
                  length = x@length)
            } else {
              new("sparse_numeric",
                  value = vals[nz],
                  pos = as.integer(pos_common[nz]),
                  length = x@length)
            }
          })

#' Crossproduct for sparse_numeric vectors
#' @description Compute the sparse crossproduct (dot product).
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#'
#' @return A numeric scalar.
#'
#' @exportMethod sparse_crossprod
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            .check_same_length(x, y)

            if (length(x@pos) == 0L || length(y@pos) == 0L) {
              return(0)
            }

            pos_common <- intersect(x@pos, y@pos)
            if (length(pos_common) == 0L) {
              return(0)
            }

            idx_x <- match(pos_common, x@pos)
            idx_y <- match(pos_common, y@pos)
            sum(x@value[idx_x] * y@value[idx_y])
          })


#' + operator
#' @description Add two sparse_numeric objects using the + operator.
#' @param e1 A sparse_numeric object.
#' @param e2 A sparse_numeric object.
#'
#' @return A sparse_numeric object.
#'
#' @exportMethod +
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' - operator
#' @description Subtract two sparse_numeric objects using the - operator.
#' @param e1 A sparse_numeric object.
#' @param e2 A sparse_numeric object.
#'
#' @return A sparse_numeric object.
#'
#' @exportMethod -
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' * operator
#' @description Multiply two sparse_numeric objects using the * operator.
#'
#' @param e1 A sparse_numeric object.
#' @param e2 A sparse_numeric object.
#'
#' @return A sparse_numeric object.
#'
#' @exportMethod *
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


#' sparse numeric display
#' @description Prints a summary of the sparse_numeric vector
#'
#' @param object A sparse_numeric object.
#' @return Prints summary output.
#' @exportMethod show
setMethod("show", "sparse_numeric", function(object) {
  cat("An object of class 'sparse_numeric'\n")
  cat(" Length: ", object@length, "\n", sep = "")
  cat(" Non-zero entries:", length(object@value), "\n")
  if (length(object@pos) > 0L) {
    df <- data.frame(
      pos = object@pos,
      value = object@value
    )
    print(df, row.names = FALSE)
  } else {
    cat(" (all entries are zero)\n")
  }
})


#' sparse numeric plot
#' @description Creates a scatterplot showing the values of two sparse_numeric vectors
#' at positions where both vectors have non-zero entries
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @return A plot.
#' @exportMethod plot
setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    plot(x@pos, x@value, pch = 16, col = "blue",
         xlab = "Position", ylab = "Value",
         main = "Sparse vectors (x in blue, y in red)")
    if (length(y@pos)) {
      points(y@pos, y@value, pch = 17, col = "red")
    }
    invisible(NULL)
  }
)

## Additional method: length of sparse_numeric vectors -----------------

#' Sparse numeric length
#' @description Length of a sparse_numeric object.
#' @param x A sparse_numeric object.
#'
#' @return Integer length.
#'
#' @exportMethod length
setMethod("length", "sparse_numeric", function(x) {
  x@length
})

#' Sparse numeric mean
#' @description Mean of a sparse_numeric vector, including zero entries.
#' @param x A sparse_numeric object.
#' @param ... Ignored.
#'
#' @return A numeric scalar.
#'
#' @exportMethod mean
setMethod("mean", "sparse_numeric", function(x, ...) {
  # Include all zeros: they contribute 0 to the sum.
  if (x@length == 0L) {
    return(NaN)
  }
  sum(x@value) / x@length
})

## ---- norm() generic + method ----------------------------------------


#' Euclidean norm
#'
#' Generic for computing the Euclidean (L2) norm.
#' @name norm
#' @rdname norm
#' @param x An object.
#' @param ... Further arguments (unused).
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' Euclidean norm
#' @description Euclidean norm of a sparse_numeric vector.
#' @aliases norm,sparse_numeric-method
#' @rdname norm
#' @param x A sparse_numeric object.
#'
#' @return A numeric scalar giving the norm.
#'
#' @exportMethod norm
setMethod("norm", "sparse_numeric", function(x, ...) {
  sqrt(sum(x@value^2))
})


#' Standardize sparse_numeric vector
#'
#' Computes (x - mean) / sd without dense coercion.
#' @name standardize
#' @rdname standardize
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))


#' Standardize sparse_numeric vector
#' @description Standardize a sparse_numeric vector.
#' @rdname standardize
#' @aliases standardize,sparse_numeric-method
#' @details Computes (x - mean) / sd over all entries including zeros.
#'
#' @param x A sparse_numeric object.
#' @param ... Ignored.
#'
#' @return A sparse_numeric object.
#'
#' @exportMethod standardize
setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n <= 1L) stop("Cannot standardize length <= 1.")

  mu <- sum(x@value) / n
  sumsq <- sum(x@value^2)
  ss <- sumsq - n * mu^2
  if (ss < 0) ss <- 0
  sd <- sqrt(ss / (n - 1))
  if (sd == 0) stop("Standard deviation is zero.")

  pos <- x@pos
  val <- x@value

  nz_std <- (val - mu) / sd
  zero_std <- (-mu) / sd

  if (length(pos) < n) {
    zeros <- setdiff(seq_len(n), pos)
    values <- c(nz_std, rep(zero_std, length(zeros)))
    positions <- c(pos, zeros)
  } else {
    values <- nz_std
    positions <- pos
  }

  keep <- values != 0
  values <- values[keep]
  positions <- positions[keep]

  ord <- order(positions)
  positions <- positions[ord]
  values <- values[ord]

  new("sparse_numeric",
      value = values,
      pos = as.integer(positions),
      length = n)
})


