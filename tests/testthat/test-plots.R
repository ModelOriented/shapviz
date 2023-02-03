dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1L]), label = iris[, 1L])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1L])

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

# SHAP interactions
x_inter <- shapviz(fit, X_pred = dtrain, X = iris[, -1L], interactions = TRUE)

test_that("dependence plots work for interactions = TRUE", {
  expect_s3_class(
    sv_dependence(x_inter, v = "Petal.Length", interactions = TRUE), "ggplot"
  )
  expect_s3_class(
    sv_dependence(
      x_inter, v = "Petal.Length", color_var = "auto", interactions = TRUE
    ),
    "ggplot"
  )
  expect_s3_class(
    sv_dependence(x_inter, "Petal.Length", color_var = "Species", interactions = TRUE),
    "ggplot"
  )
})

test_that("main effect plots equal case color_var = v", {
  expect_equal(
    sv_dependence(x_inter, "Petal.Length", interactions = TRUE),
    sv_dependence(
      x_inter, "Petal.Length", color_var = "Petal.Length", interactions = TRUE
    )
  )
})

test_that("potential_interactions() depend on presence of S_inter", {
  expect_true(
    !any(potential_interactions(x, "Petal.Length") ==
           potential_interactions(x_inter, "Petal.Length")
    )
  )
})

test_that("Interaction plots provide ggplot object", {
  expect_s3_class(sv_interaction(x_inter), "ggplot")
})

# Non-standard name
ir <- iris
ir["strange name"] <- ir$Sepal.Width * ir$Petal.Length
dtrain <- xgboost::xgb.DMatrix(data.matrix(ir[, -1L]), label = ir[, 1L])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L)
x <- shapviz(fit, X_pred = dtrain, X = ir[, -1L])

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


