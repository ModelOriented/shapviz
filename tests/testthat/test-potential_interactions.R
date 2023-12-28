dtrain <- xgboost::xgb.DMatrix(
  data.matrix(iris[, -1L]), label = iris[, 1L], nthread = 1
)
fit <- xgboost::xgb.train(params = list(nthread = 1L), data = dtrain, nrounds = 10L)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1L])

test_that("n_bins has no effect for factor v", {
  expect_equal(
    potential_interactions(x, "Species", n_bins = NULL),
    potential_interactions(x, "Species", n_bins = 2)
  )
})

test_that("n_bins has an effect for numeric v", {
  expect_false(
    identical(
      potential_interactions(x, "Sepal.Width", n_bins = 2),
      potential_interactions(x, "Sepal.Width", n_bins = 3)
    )
  )
})

# Now with SHAP interactions
test_that("potential_interactions respects true SHAP interactions", {
  xi <- shapviz(fit, X_pred = dtrain, X = iris[, -1L], interactions = TRUE)
  i1 <- potential_interactions(xi, "Species")
  i2 <- sv_interaction(xi, kind = "no")[names(i1), "Species"]
  expect_equal(i1, i2, tolerance = 1e-5)
})

