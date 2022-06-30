S <- cbind(
  x = c(0.1, 0.1, 0.1),
  `age low` = c(0.2, -0.1, 0.1),
  `age mid` = c(0, 0.2, -0.2),
  `age high` = c(1, -1, 0)
)
collapse <- list(age = c("age low", "age mid", "age high"))
out <- collapse_shap(S, collapse)

test_that("collapse_shap work", {
  expect_equal(colnames(out), c("x", "age"))
  expect_equal(out[, "x"], S[, "x"])
  expect_equal(out[, "age"], rowSums(S[, c("age low", "age mid", "age high")]))
})

test_that("collapse_shap does nothing if it is NULL", {
  expect_equal(collapse_shap(S), S)
})

test_that("collapse_shap provides errors if not correctly specified", {
  expect_error(collapse_shap(S, collapse = list(age = "age.low")))
  expect_error(collapse_shap(S, collapse = list(x = "age low")))
  expect_error(collapse_shap(S, collapse = list(x = "x", x = "age low")))
  expect_error(collapse_shap(S, collapse = list(y = "age low", x = "age low")))
})

S <- cbind(
  x = c(0.1, 0.1, 0.1),
  `age low` = c(0.2, -0.1, 0.1),
  `age high` = c(1, -1, 0),
  A = 1:3,
  B = 1:3
)
collapse <- list(age = c("age low", "age high"), letter = c("A", "B"))
out <- collapse_shap(S, collapse)

test_that("collapse_shap work for two groups", {
  expect_equal(colnames(out), c("x", "age", "letter"))
  expect_equal(out[, "age"], rowSums(S[, c("age low", "age high")]))
})
