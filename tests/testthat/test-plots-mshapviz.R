dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1L]), label = iris[, 1L])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1L])
x <- c(m1 = x, m2 = x)

test_that("plots work for basic example", {
  expect_s3_class(sv_waterfall(x, 2), "patchwork")
  suppressMessages(expect_s3_class(sv_waterfall(x, 2:3), "patchwork"))
  expect_s3_class(sv_force(x, 2), "patchwork")
  suppressMessages(expect_s3_class(sv_force(x, 2:3), "patchwork"))
  expect_s3_class(sv_importance(x), "patchwork")
  expect_s3_class(sv_importance(x, show_numbers = TRUE), "patchwork")
  expect_s3_class(sv_importance(x, kind = "beeswarm"), "patchwork")
  expect_s3_class(sv_dependence(x, "Petal.Length"), "patchwork")
})

test_that("using 'max_display' gives no error", {
  expect_s3_class(sv_waterfall(x, 2, max_display = 2L), "patchwork")
  suppressMessages(expect_s3_class(sv_waterfall(x, 2:10, max_display = 2L), "patchwork"))
  expect_s3_class(sv_force(x, 2, max_display = 2L), "patchwork")
  suppressMessages(expect_s3_class(sv_force(x, 2:10, max_display = 2L), "patchwork"))
  expect_s3_class(sv_importance(x, max_display = 2L), "patchwork")
  expect_s3_class(sv_importance(x, max_display = 2L, show_numbers = TRUE), "patchwork")
})

# SHAP interactions
x_inter <- shapviz(fit, X_pred = dtrain, X = iris[, -1L], interactions = TRUE)
x_inter <- c(m1 = x_inter, m2 = x_inter)

test_that("dependence plots work for interactions = TRUE", {
  expect_s3_class(
    sv_dependence(x_inter, v = "Petal.Length", interactions = TRUE),
    "patchwork"
  )
  expect_s3_class(
    sv_dependence(x_inter, v = "Petal.Length", interactions = TRUE),
    "patchwork"
  )
  expect_s3_class(
    sv_dependence(x_inter, "Petal.Length", color_var = "Species", interactions = TRUE),
    "patchwork"
  )
})

test_that("main effect plots equal case color_var = v", {
  expect_equal(
    sv_dependence(x_inter, "Petal.Length", color_var = NULL, interactions = TRUE),
    sv_dependence(
      x_inter, "Petal.Length", color_var = "Petal.Length", interactions = TRUE
    )
  )
})

test_that("Interaction plots provide patchwork object", {
  expect_s3_class(sv_interaction(x_inter), "patchwork")
})

# Non-standard name
ir <- iris
ir["strange name"] <- ir$Sepal.Width * ir$Petal.Length
dtrain <- xgboost::xgb.DMatrix(data.matrix(ir[, -1L]), label = ir[, 1L])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L)
x <- shapviz(fit, X_pred = dtrain, X = ir[, -1L])
x <- c(m1 = x, m2 = x)

test_that("plots work for non-syntactic column names", {
  expect_s3_class(sv_waterfall(x, 2), "patchwork")
  expect_s3_class(sv_force(x, 2), "patchwork")
  expect_s3_class(sv_importance(x), "patchwork")
  expect_s3_class(sv_importance(x, show_numbers = TRUE), "patchwork")
  expect_s3_class(sv_importance(x, max_display = 2, kind = "beeswarm"), "patchwork")
  expect_s3_class(sv_importance(x, kind = "beeswarm"), "patchwork")
  expect_s3_class(sv_dependence(x, "strange name"), "patchwork")
  expect_s3_class(
    sv_dependence(x, "Petal.Length", color_var = "strange name"), "patchwork"
  )
})

test_that("sv_importance() and sv_interaction() and kind = 'no' gives list", {
  X_pred <- data.matrix(iris[, -1L])
  dtrain <- xgboost::xgb.DMatrix(X_pred, label = iris[, 1L])
  fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L, nthread = 1L)
  x <- shapviz(fit, X_pred = X_pred, interactions = TRUE)
  x <- c(m1 = x, m2 = x)

  imp <- sv_importance(x, kind = "no")
  expect_true(is.list(imp) && length(imp) == length(x))

  inter <- sv_interaction(x, kind = "no")
  expect_true(is.list(inter) && all(dim(inter[[1L]]) == rep(ncol(X_pred), 2L)))
})


