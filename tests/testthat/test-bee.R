# First terms of 1d Halton sequence https://en.wikipedia.org/wiki/Van_der_Corput_sequence
h <- c(1/2, 1/4, 3/4, 1/8, 5/8, 3/8, 7/8, 1/16)
n <- length(h)

test_that("the first terms of halton() are correct", {
  expect_identical(sapply(seq_along(h), halton), h)
})

test_that("halton_sequence(n) equals h", {
  expect_identical(halton_sequence(n), h)
})

test_that("ave2() equals ave() for basic case", {
  x <- 1:20
  g <- rep(1:4, each = 5L)
  expect_identical(ave(x, g), ave2(x, g))
})

test_that("shifter(y) returns 0 if length(y) == 1", {
  expect_equal(shifter(3), 0)
})
