## ============================================================
## sparse_numeric Class Definition & Methods
## ============================================================

#' Sparse Numeric Vector Class
#'
#' An S4 class that stores a numeric vector in sparse form, using only non-zero
#' values and their positions.
#'
#' @slot value A numeric vector of non-zero values.
#' @slot pos An integer vector giving the positions of the non-zero values.
#' @slot length An integer giving the full dense length of the vector.
#' @import methods
#' @importFrom graphics plot par text
#' @importFrom utils head
#' @return An object of class \code{sparse_numeric}.
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' Validity check for sparse_numeric objects (internal)
#'
#' @param object A sparse_numeric object.
#' @return TRUE if valid, otherwise an error message.
#' @noRd
setValidity("sparse_numeric", function(object) {
  # Check pos and value same length
  if (length(object@pos) != length(object@value)) {
    return("pos and value must be the same length")
  }

  # Must be sorted
  if (!isTRUE(all.equal(object@pos, sort(object@pos)))) {
    return("pos must be sorted in increasing order")
  }

  # No duplicates
  if (any(duplicated(object@pos))) {
    return("pos must not contain duplicates")
  }

  # Must be within valid range
  if (any(object@pos < 1) || any(object@pos > object@length)) {
    return("pos contains invalid positions")
  }

  TRUE
})


## ADD

#' Add Two sparse_numeric Vectors
#'
#' Performs elementwise addition of two \code{sparse_numeric} objects.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param e1 First operand (for arithmetic operator methods).
#' @param e2 Second operand (for arithmetic operator methods).
#' @param ... Additional arguments (ignored).
#' @return A sparse_numeric object.
#' @export
setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))

#' @rdname sparse_add
#' @export
setMethod("sparse_add",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            as(full_x + full_y, "sparse_numeric")
          })


## MULTIPLICATION

#' Elementwise Multiplication for sparse_numeric
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param e1 First operand (for arithmetic operator methods).
#' @param e2 Second operand (for arithmetic operator methods).
#' @param ... Additional arguments (ignored).
#' @return A sparse_numeric object.
#' @export
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))

#' @rdname sparse_mult
#' @export
setMethod("sparse_mult",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            as(full_x * full_y, "sparse_numeric")
          })


## SUBTRACTION

#' Subtract Two sparse_numeric Vectors
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param e1 First operand (for arithmetic operator methods).
#' @param e2 Second operand (for arithmetic operator methods).
#' @param ... Additional arguments (ignored).
#' @return A sparse_numeric object.
#' @export
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))

#' @rdname sparse_sub
#' @export
setMethod("sparse_sub",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            full_x <- as(x, "numeric")
            full_y <- as(y, "numeric")
            as(full_x - full_y, "sparse_numeric")
          })


## DOT PRODUCT


#' Dot Product for sparse_numeric
#'
#' Computes the dot product (crossproduct) of two sparse_numeric vectors.
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments (ignored).
#' @return Numeric scalar.
#' @export
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")
            sum(as(x, "numeric") * as(y, "numeric"))
          })


## ARITHMETIC OPERATORS

#' @rdname sparse_add
#' @export
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_sub
#' @export
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_mult
#' @export
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


## COERCION


#' Convert sparse_numeric to numeric
#'
#' @param from A sparse_numeric object.
#' @return A numeric vector.
#' @noRd
setAs("sparse_numeric", "numeric", function(from) {
  vec <- numeric(from@length)
  vec[from@pos] <- from@value
  vec
})

#' Convert numeric to sparse_numeric
#'
#' @param from A numeric vector.
#' @return A sparse_numeric object.
#' @noRd
setAs("numeric", "sparse_numeric", function(from) {
  nz <- which(from != 0)
  new("sparse_numeric",
      value = from[nz],
      pos = as.integer(nz),
      length = as.integer(length(from)))
})


## SHOW METHOD

#' Show Method for sparse_numeric
#'
#' @param object A sparse_numeric object.
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat("<sparse_numeric> vector of length", object@length, "\n")
  cat(" Nonzero elements:", length(object@value), "\n")

  if (length(object@value) > 0) {
    preview <- min(5, length(object@value))
    cat(" Positions:", paste(head(object@pos, preview), collapse = ", "))
    if (length(object@value) > preview) cat(", ...")
    cat("\n Values:", paste(head(object@value, preview), collapse = ", "))
    if (length(object@value) > preview) cat(", ...")
    cat("\n")
  }
})


## PLOT METHOD

#' Plot Overlapping Nonzero Elements
#'
#' @param x First sparse_numeric.
#' @param y Second sparse_numeric.
#' @param ... Additional plotting args passed to plot().
#' @return A scatter plot.
#' @export
setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {

    if (x@length != y@length)
      stop("Vectors must have same length")

    xv <- as(x, "numeric")
    yv <- as(y, "numeric")
    idx <- intersect(x@pos, y@pos)

    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    if (length(idx) == 0) {
      plot(0, 0, type="n", xlab="", ylab="")
      text(0, 0, "No overlapping non-zero positions")
      return(invisible(NULL))
    }

    plot(idx, yv[idx], pch = 16, xlab="Position", ylab="Value", ...)
    invisible(NULL)
  }
)


## LENGTH METHOD

#' Get Length of sparse_numeric
#'
#' @param x A sparse_numeric object.
#' @return Integer length.
#' @export
setMethod("length",
          signature(x = "sparse_numeric"),
          function(x) x@length)


## MEAN

#' Mean of sparse_numeric
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (ignored).
#' @return Numeric mean.
#' @export
setMethod("mean",
          signature(x = "sparse_numeric"),
          function(x, ...) sum(x@value) / x@length)



## NORM

#' Euclidean Norm (L2) of sparse_numeric
#'
#' @title Norm for sparse_numeric
#' @name norm
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (ignored).
#' @return Numeric scalar.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname norm
#' @export
setMethod("norm",
          signature(x = "sparse_numeric"),
          function(x, ...) sqrt(sum(x@value^2)))


## STANDARDIZE

#' Standardize a sparse_numeric Vector
#'
#' @title Standardize sparse_numeric
#' @name standardize
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (ignored).
#' @return A standardized sparse_numeric object.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname standardize
#' @export
setMethod("standardize",
          signature(x = "sparse_numeric"),
          function(x, ...) {

            # Check for NA in non-zero values
            if (any(is.na(x@value))) {
              stop("Cannot standardize: vector contains NA values")
            }

            # Check for NA in implicit zeros
            # If length > length of non-zero positions, then implicit zeros exist
            n <- x@length
            if (any(rep(NA, n)[-x@pos])) {
              stop("Cannot standardize: vector contains NA values")
            }

            # Compute mean and standard deviation using sparse logic
            vals <- x@value
            mu <- sum(vals) / n
            s2 <- (sum(vals^2) - n * mu^2) / (n - 1)
            sd_x <- sqrt(s2)

            if (sd_x == 0)
              stop("Standard deviation is zero, cannot standardize")

            std_vals <- (vals - mu) / sd_x
            std_zero <- (-mu) / sd_x

            # Return sparse vector
            if (std_zero == 0) {
              new("sparse_numeric",
                  value = std_vals,
                  pos = x@pos,
                  length = n)
            } else {
              full <- rep(std_zero, n)
              full[x@pos] <- std_vals
              as(full, "sparse_numeric")
            }
          })
