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

test_that("color_numeric has an effect", {
  p1 <- potential_interactions(x, "Sepal.Width", color_numeric = TRUE)
  p2 <- potential_interactions(x, "Sepal.Width", color_numeric = FALSE)
  num <- c("Petal.Width", "Petal.Length")
  expect_equal(p1[num], p2[num])
  expect_false(p1["Species"] == p2["Species"])
})

test_that("potential_interactions respects true SHAP interactions", {
  xi <- shapviz(fit, X_pred = dtrain, X = iris[, -1L], interactions = TRUE)
  i1 <- potential_interactions(xi, "Species")
  i2 <- sv_interaction(xi, kind = "no")[names(i1), "Species"]
  expect_equal(i1, i2, tolerance = 1e-5)
})

test_that("r2_adj_uni() returns R-squared adjusted", {
  fit_lm <- lm(Sepal.Length ~ Species, data = iris)
  expect_equal(
    r2_adj_uni(iris$Sepal.Length, iris$Species),
    summary(fit_lm)[["adj.r.squared"]]
  )
})

test_that("r2_adj_uni() fails with NA", {
  expect_equal(r2_adj_uni(0, 1:2), NA)
})

test_that("r2_adj() returns R-squared adjusted per column in df", {
  fit_lm1 <- lm(Sepal.Length ~ Species, data = iris)
  fit_lm2 <- lm(Sepal.Length ~ Sepal.Width, data = iris)

  expect_equal(
    r2_adj(iris$Sepal.Length, iris[c("Species", "Sepal.Width")]),
    c(summary(fit_lm1)[["adj.r.squared"]], summary(fit_lm2)[["adj.r.squared"]])
  )
})

test_that("r2_adj() can fail with NA", {
  expect_equal(r2_adj(0, df = data.frame(x = c(1, 1), y = 1:2)), c(NA_real_, NA_real_))
})



# r_sq <- function(s, x) {
#   suppressWarnings(stats::cor(s, data.matrix(x), use = "p")^2)
# }

