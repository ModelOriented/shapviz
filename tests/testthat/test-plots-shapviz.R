dtrain <- xgboost::xgb.DMatrix(data.matrix(iris[, -1L]), label = iris[, 1L])
fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1L])

test_that("plots work for basic example", {
  expect_s3_class(sv_waterfall(x, 2), "ggplot")
  suppressMessages(expect_s3_class(sv_waterfall(x, 2:3), "ggplot"))
  expect_s3_class(sv_force(x, 2), "ggplot")
  suppressMessages(expect_s3_class(sv_force(x, 2:3), "ggplot"))
  expect_s3_class(sv_importance(x), "ggplot")
  expect_s3_class(sv_importance(x, show_numbers = TRUE), "ggplot")
  expect_s3_class(sv_importance(x, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_dependence(x, "Petal.Length"), "ggplot")
  expect_s3_class(sv_dependence(x, c("Petal.Length", "Species")), "patchwork")
  expect_s3_class(
    sv_dependence(
      x,
      "Petal.Length",
      color_var = c("Petal.Length", "Species"),
      jitter_width = c(0, 0.1),
    ),
    "patchwork"
  )
})

test_that("using 'max_display' gives no error", {
  expect_s3_class(sv_waterfall(x, 2, max_display = 2L), "ggplot")
  suppressMessages(expect_s3_class(sv_waterfall(x, 2:10, max_display = 2L), "ggplot"))
  expect_s3_class(sv_force(x, 2, max_display = 2L), "ggplot")
  suppressMessages(expect_s3_class(sv_force(x, 2:10, max_display = 2L), "ggplot"))
  expect_s3_class(sv_importance(x, max_display = 2L), "ggplot")
  expect_s3_class(sv_importance(x, max_display = 2L, show_numbers = TRUE), "ggplot")
})

# SHAP interactions
x_inter <- shapviz(fit, X_pred = dtrain, X = iris[, -1L], interactions = TRUE)

test_that("dependence plots work for interactions = TRUE", {
  expect_s3_class(
    sv_dependence(x_inter, v = "Petal.Length", interactions = TRUE),
    "ggplot"
  )
  expect_s3_class(
    sv_dependence(x_inter, v = c("Petal.Length", "Species"), interactions = TRUE),
    "patchwork"
  )
  expect_s3_class(
    sv_dependence(
      x_inter,
      v = "Species",
      color_var = c("Petal.Length", "Species"),
      interactions = TRUE
    ),
    "patchwork"
  )

  expect_s3_class(
    sv_dependence(x_inter, "Petal.Length", color_var = "Species", interactions = TRUE),
    "ggplot"
  )
  expect_s3_class(
    sv_dependence(
      x_inter,
      v = c("Petal.Length", "Species"),
      color_var = "Species",
      interactions = TRUE
    ),
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
  expect_s3_class(sv_importance(x, show_numbers = TRUE), "ggplot")
  expect_s3_class(sv_importance(x, max_display = 2, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_importance(x, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_dependence(x, "strange name"), "ggplot")
  expect_s3_class(
    sv_dependence(x, "Petal.Length", color_var = "strange name"), "ggplot"
  )
})

# Miscellaneous tests
test_that("there are no default sv_*() methods", {
  for (f in c(sv_dependence, sv_importance, sv_force, sv_waterfall, sv_interaction)) {
    expect_error(f(1))
  }
})

test_that("sv_importance() and sv_interaction() and kind = 'no' gives numeric output", {
  X_pred <- data.matrix(iris[, -1L])
  dtrain <- xgboost::xgb.DMatrix(X_pred, label = iris[, 1L])
  fit <- xgboost::xgb.train(data = dtrain, nrounds = 50L, nthread = 1L)
  x <- shapviz(fit, X_pred = X_pred, interactions = TRUE)

  imp <- sv_importance(x, kind = "no")
  expect_true(is.numeric(imp) && length(imp) == ncol(X_pred))

  inter <- sv_interaction(x, kind = "no")
  expect_true(is.numeric(inter) && all(dim(inter) == rep(ncol(X_pred), 2L)))
})

