dtrain <- xgboost::xgb.DMatrix(
  data.matrix(iris[, -1L]), label = iris[, 1L], nthread = 1
)
fit <- xgboost::xgb.train(params = list(nthread = 1L), data = dtrain, nrounds = 10L)
x <- shapviz(fit, X_pred = dtrain, X = iris[, -1L])

test_that("nbins has no effect for factor v", {
  expect_equal(
    potential_interactions(x, "Species", nbins = NULL),
    potential_interactions(x, "Species", nbins = 2)
  )
})

test_that("nbins has an effect for numeric v", {
  expect_false(
    identical(
      potential_interactions(x, "Sepal.Width", nbins = 2),
      potential_interactions(x, "Sepal.Width", nbins = 3)
    )
  )
})

test_that("color_num has an effect only for non-numeric features", {
  p1 <- potential_interactions(x, "Sepal.Width", color_num = TRUE)
  p2 <- potential_interactions(x, "Sepal.Width", color_num = FALSE)
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

test_that("heuristic_in_bin() returns R-squared adjusted", {
  fit_lm <- lm(Sepal.Length ~ Species, data = iris)
  expect_equal(
    unname(heuristic_in_bin(iris$Species, iris$Sepal.Length)[1, 1]),
    summary(fit_lm)[["adj.r.squared"]]
  )

  expect_equal(
    unname(heuristic_in_bin(iris$Species, iris$Sepal.Length, scale = TRUE)[1, 1]),
    summary(fit_lm)[["adj.r.squared"]] * var(iris$Sepal.Length)
  )
})

test_that("Failing heuristic_in_bin() returns NA", {
  expect_equal(heuristic_in_bin(0, 1:2), cbind(stat = NA, n = 0))
})

test_that("heuristic() returns average R-squared adjusted", {
  ix <- c(rep(1, 60), rep(2, 90))
  y <- split(iris$Sepal.Length, ix)
  x1 <- split(iris$Sepal.Width, ix)
  x2 <- split(iris$Species, ix)
  f <- function(y, x) {
    summary(lm(y ~ x))[["adj.r.squared"]]
  }

  expect_equal(
    heuristic(
      iris$Sepal.Width, iris$Sepal.Length, bins = ix, color_num = TRUE, scale = FALSE
    ),
    weighted.mean(mapply(f, y, x1), c(60, 90))
  )

  expect_equal(
    heuristic(
      iris$Species, iris$Sepal.Length, bins = ix, color_num = FALSE, scale = FALSE
    ),
    weighted.mean(mapply(f, y, x2), c(60, 90))
  )
})

