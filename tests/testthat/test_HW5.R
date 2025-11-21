## HW5_testscript.R

library(testthat)
library(HW5)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
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

test_that("coercion numeric to sparse_numeric works correctly", {
  x <- c(0, 6, 0, 7, 0)
  s <- as(x, "sparse_numeric")

  expect_equal(s@pos, c(2L, 4L))
  expect_equal(s@value, c(6, 7))
  expect_equal(s@length, 5L)
})

test_that("coercion sparse_numeric to numeric preserves values", {
  x <- as(c(0, 6, 0, 7), "sparse_numeric")
  dense <- as(x, "numeric")
  expect_equal(dense, c(0, 6, 0, 7))
})

test_that("sparse_sub works", {
  x <- as(c(1, 6, 7), "sparse_numeric")
  y <- as(c(1, 5, 0), "sparse_numeric")
  expect_equal(sparse_sub(x, y), as(c(0, 1, 7), "sparse_numeric"))
})

test_that("sparse_sub errors when lengths differ", {
  x <- as(c(1, 6, 7), "sparse_numeric")
  y <- as(c(1, 5), "sparse_numeric")
  expect_error(sparse_sub(x,y))
})

test_that("sparse_mult works", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(1, 5, 0), "sparse_numeric")
  expect_equal(sparse_mult(x, y), as(c(1, 10, 0), "sparse_numeric"))
})

test_that("sparse_mult errors when lengths differ", {
  x <- as(c(1, 0, 7), "sparse_numeric")
  y <- as(c(0, 5), "sparse_numeric")
  expect_error(sparse_mult(x,y))
})

test_that("sparse_crossprod computes inner product", {
  x <- as(c(2,6,1), "sparse_numeric")
  y <- as(c(3,0,5), "sparse_numeric")
  expect_equal(sparse_crossprod(x,y), 2*3 + 1*5)
})

test_that("sparse_crossprod errors when lengths differ", {
  x <- as(c(0, 0, 7), "sparse_numeric")
  y <- as(c(0, 3), "sparse_numeric")
  expect_error(sparse_crossprod(x,y))
})

test_that("operator + works", {
  x <- as(c(3,0,3), "sparse_numeric")
  y <- as(c(3,1,4), "sparse_numeric")
  expect_equal(x + y, as(c(6,1,7), "sparse_numeric"))
})

test_that("operator - works", {
  x <- as(c(7,0,1), "sparse_numeric")
  y <- as(c(1,2,1), "sparse_numeric")
  expect_equal(x - y, as(c(6,-2,0), "sparse_numeric"))
})

test_that("operator * works", {
  x <- as(c(2,0,3), "sparse_numeric")
  y <- as(c(9,1,2), "sparse_numeric")
  expect_equal(x * y, as(c(18,0,6), "sparse_numeric"))
})

test_that("mean works for sparse_numeric", {
  x <- as(c(0, 4, 0, 2), "sparse_numeric")
  expect_equal(mean(x), mean(c(0,4,0,2)))
})

test_that("norm works for sparse numeric", {
  x <- as(c(1, 0, 2, 0), "sparse_numeric")
  expect_equal(norm(x), sqrt(5))
})

test_that("standardize produces mean 0, sd 1", {
  x <- as(c(1,2,3,4), "sparse_numeric")
  s <- standardize(x)

  dense <- as(s, "numeric")
  expect_equal(round(mean(dense), 6), 0)
  expect_equal(round(sd(dense), 6), 1)
})

test_that("standardize errors on zero variance", {
  x <- as(c(5,5,5), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("length returns correct value", {
  x <- as(c(0,8,0,3), "sparse_numeric")
  expect_equal(length(x), 4)
})

test_that("plot produces no error when overlap exists", {
  x <- as(c(0,1,0,2), "sparse_numeric")
  y <- as(c(3,1,4,2), "sparse_numeric")
  expect_no_error(plot(x,y))
})

test_that("plot produces no error when no overlap", {
  x <- as(c(0,1,0), "sparse_numeric")
  y <- as(c(3,6,2), "sparse_numeric")
  expect_no_error(plot(x,y))
})

test_that("initialize handles zero case", {
  x <- new("sparse_numeric", value = numeric(), pos = integer(), length = 5L)
  expect_s4_class(x, "sparse_numeric")
  expect_equal(x@length, 5L)
})

test_that("show displays correct structure", {
  x <- as(c(0, 2, 0, 5), "sparse_numeric")
  expect_output(show(x), "sparse_numeric")
  expect_output(show(x), "Positions:")
})

test_that("invalid pos values are detected", {
  expect_error(new("sparse_numeric",
                   value = c(1,2),
                   pos = c(2L,2L),
                   length = 4L))
})

test_that("standardize handles NA", {
  x <- as(c(1, NA, 3), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("length is preserved after coercion to numeric and back", {
  x <- as(c(0,1,0,2), "sparse_numeric")
  dense <- as(x, "numeric")
  s <- as(dense, "sparse_numeric")
  expect_equal(length(s), length(x))
})

test_that("sparse_add with all zeros", {
  x <- as(rep(0, 5), "sparse_numeric")
  y <- as(rep(0, 5), "sparse_numeric")
  expect_equal(sparse_add(x, y), as(rep(0, 5), "sparse_numeric"))
})

test_that("sparse_mult resulting in all zeros", {
  x <- as(c(0, 0, 3), "sparse_numeric")
  y <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(sparse_mult(x, y), as(rep(0, 3), "sparse_numeric"))
})

test_that("sparse_sub resulting in negative values", {
  x <- as(c(1,2,3), "sparse_numeric")
  y <- as(c(4,1,5), "sparse_numeric")
  expect_equal(sparse_sub(x,y), as(c(-3,1,-2), "sparse_numeric"))
})

test_that("show prints preview with more than 5 non-zero elements", {
  x <- as(1:10, "sparse_numeric")  # 10 non-zero elements
  expect_output(show(x), "Positions: 1, 2, 3, 4, 5, ...")
  expect_output(show(x), "Values: 1, 2, 3, 4, 5, ...")
})

test_that("plot errors when lengths differ", {
  x <- as(c(1, 2, 0), "sparse_numeric")
  y <- as(c(1, 2), "sparse_numeric")
  expect_error(plot(x, y), "Vectors must have same length")
})

test_that("plot handles no overlapping non-zero positions", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 0), "sparse_numeric")
  expect_no_error(plot(x, y))  # should execute plot(0,0) + text
})

