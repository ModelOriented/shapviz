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

test_that("collapse_shap works for two groups", {
  expect_equal(colnames(out), c("x", "age", "letter"))
  expect_equal(out[, "age"], rowSums(S[, c("age low", "age high")]))
})

# Interactions

test_that("collapse_shap gives an error for arrays of dimension >3", {
  S_inter <- array(
    dim = c(2L, 2L, 2L, 2L),
    dimnames = replicate(4L, c("a", "b"), simplify = FALSE)
  )
  expect_error(collapse_shap(S_inter, collapse = list(ab = c("a", "b"))))
})

test_that("collapse_shap works for SHAP interactions (result is nx1x1)", {
  S_inter <- array(
    1,
    dim = c(2L, 2L, 2L),
    dimnames = list(NULL, c("a", "b"), c("a", "b"))
  )
  out <- collapse_shap(S_inter, collapse = list(ab = c("a", "b")))
  expected_value <- array(4, dim = c(2L, 1L, 1L), dimnames = list(NULL, "ab", "ab"))
  expect_equal(out, expected_value)
})

test_that("collapse_shap works for SHAP interactions and n = 1", {
  S_inter <- array(
    1,
    dim = c(1L, 2L, 2L),
    dimnames = list(NULL, c("a", "b"), c("a", "b"))
  )
  out <- collapse_shap(S_inter, collapse = list(ab = c("a", "b")))
  expected_value <- array(4, dim = c(1L, 1L, 1L), dimnames = list(NULL, "ab", "ab"))
  expect_equal(out, expected_value)
})

test_that("collapse_shap works for SHAP interactions and two collapses (result is nx2x2)", {
  S_inter <- array(
    1,
    dim = c(2L, 4L, 4L),
    dimnames = list(NULL, letters[1:4], letters[1:4])
  )
  out <- collapse_shap(S_inter, collapse = list(cd = c("c", "d"), ab = c("a", "b")))
  expected_value <- array(
    4, dim = c(2L, 2L, 2L), dimnames = list(NULL, c("cd", "ab"), c("cd", "ab"))
  )
  expect_equal(out, expected_value)
})

# # Real data example
# form <- Sepal.Length ~ Sepal.Width + Species - 1
# iris_dummy <- model.matrix(form, data = iris)
# dtrain <- xgboost::xgb.DMatrix(iris_dummy, label = iris[, 1L])
# fit <- xgboost::xgb.train(params = list(nthread = 1L), data = dtrain, nrounds = 1L)
# coll <- list(Species = paste0("Species", levels(iris$Species)))
#
# test_that("Collapse works using XGB API", {
#   expect_no_error(
#     x <- shapviz(fit, X_pred = dtrain, X = iris, collapse = coll, interactions = TRUE)
#   )
#   expect_identical(colnames(x), c("Sepal.Width", "Species"))
# })
