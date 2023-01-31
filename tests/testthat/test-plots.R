dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1]), label = iris[, 1])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1])

test_that("plots work for basic example", {
  expect_s3_class(sv_waterfall(x, 2), "ggplot")
  expect_s3_class(sv_force(x, 2), "ggplot")
  expect_s3_class(sv_importance(x), "ggplot")
  expect_s3_class(sv_importance(x, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_dependence(x, "Petal.Length", color_var = "auto"), "ggplot")
})

test_that("using 'max_display' gives no error", {
  expect_s3_class(sv_waterfall(x, 2, max_display = 2L), "ggplot")
  expect_s3_class(sv_force(x, 2, max_display = 2L), "ggplot")
  expect_s3_class(sv_importance(x, max_display = 2L), "ggplot")
})

# Now with non-standard name
ir <- iris
ir["strange name"] <- ir$Sepal.Width * ir$Petal.Length
dtrain <- xgboost::xgb.DMatrix(data.matrix(ir[, -1]), label = ir[, 1])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
x <- shapviz(fit, X_pred = dtrain, X = ir[, -1])

test_that("plots work for non-syntactic column names", {
  expect_s3_class(sv_waterfall(x, 2), "ggplot")
  expect_s3_class(sv_force(x, 2), "ggplot")
  expect_s3_class(sv_importance(x), "ggplot")
  expect_s3_class(sv_importance(x, max_display = 2, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_importance(x, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_dependence(x, "strange name", color_var = "auto"), "ggplot")
  expect_s3_class(
    sv_dependence(x, "Petal.Length", color_var = "strange name"), "ggplot"
  )
})
