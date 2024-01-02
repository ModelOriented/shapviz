dtrain <- xgboost::xgb.DMatrix(
  data.matrix(iris[, -1L]), label = iris[, 1L], nthread = 1L
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

test_that("'adjusted' leads to smaller values", {
  p1 <- potential_interactions(x, "Sepal.Width", adjusted = FALSE)
  p2 <- potential_interactions(x, "Sepal.Width", adjusted = TRUE)
  expect_true(all(p1 > p2))
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

test_that("heuristic_in_bin() returns R-squared", {
  fit_lm <- lm(Sepal.Length ~ Species, data = iris)
  expect_equal(
    unname(heuristic_in_bin(iris$Species, iris$Sepal.Length)[1, 1]),
    summary(fit_lm)[["r.squared"]]
  )
  expect_equal(
    unname(heuristic_in_bin(iris$Species, iris$Sepal.Length, adjusted = TRUE)[1, 1]),
    summary(fit_lm)[["adj.r.squared"]]
  )

  # Scaled
  expect_equal(
    unname(heuristic_in_bin(iris$Species, iris$Sepal.Length, scale = TRUE)[1, 1]),
    summary(fit_lm)[["r.squared"]] * var(iris$Sepal.Length)
  )
  expect_equal(
    unname(
      heuristic_in_bin(
        iris$Species, iris$Sepal.Length, scale = TRUE, adjusted = TRUE)[1, 1]
    ),
    summary(fit_lm)[["adj.r.squared"]] * var(iris$Sepal.Length)
  )
})

test_that("Failing heuristic_in_bin() returns 0", {
  expect_equal(heuristic_in_bin(c(NA, NA), 1:2), cbind(stat = 0, n = 0))
})

test_that("heuristic_in_bin() returns 0 for constant response", {
  expect_equal(
    heuristic_in_bin(color = 1:3, s = c(1, 1, 1)),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(color = 1:3, s = c(1, 1, 1), scale = TRUE),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(color = 1:3, s = c(1, 1, 1), adjust = TRUE),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(color = 1:3, s = c(1, 1, 1), adjust = TRUE, scale = TRUE),
    cbind(stat = 0, n = 3L)
  )
})

test_that("heuristic_in_bin() returns 0 for constant color", {
  expect_equal(
    heuristic_in_bin(s = 1:3, color = c(1, 1, 1)),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(s = 1:3, color = c(1, 1, 1), scale = TRUE),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(s = 1:3, color = c(1, 1, 1), adjust = TRUE),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(s = 1:3, color = c(1, 1, 1), adjust = TRUE, scale = TRUE),
    cbind(stat = 0, n = 3L)
  )
})

test_that("heuristic_in_bin() returns 0 if response and color are constant", {
  z <- c(1, 1)
  expect_equal(
    heuristic_in_bin(color = z, s = z),
    cbind(stat = 0, n = 2L)
  )
  expect_equal(
    heuristic_in_bin(color = z, s = z, scale = TRUE),
    cbind(stat = 0, n = 2L)
  )
  expect_equal(
    heuristic_in_bin(color = z, s = z, adjust = TRUE),
    cbind(stat = 0, n = 2L)
  )
  expect_equal(
    heuristic_in_bin(color = z, s = z, adjust = TRUE, scale = TRUE),
    cbind(stat = 0, n = 2L)
  )
})

test_that("heuristic_in_bin() returns 0 for single obs", {
  expect_equal(
    heuristic_in_bin(color = 2, s = 2),
    cbind(stat = 0, n = 1L)
  )
  expect_equal(
    heuristic_in_bin(color = 2, s = 2, scale = TRUE),
    cbind(stat = 0, n = 1L)
  )
  expect_equal(
    heuristic_in_bin(color = 2, s = 2, adjust = TRUE),
    cbind(stat = 0, n = 1L)
  )
  expect_equal(
    heuristic_in_bin(color = 2, s = 2, adjust = TRUE, scale = TRUE),
    cbind(stat = 0, n = 1L)
  )
})

test_that("heuristic_in_bin() returns NA for single obs", {
  cc <- factor(LETTERS[1:3])
  expect_equal(
    heuristic_in_bin(color = cc, s = 1:3),
    cbind(stat = 1, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(color = cc, s = 2*(1:3), scale = TRUE),
    cbind(stat = 4, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(color = cc, s = 1:3, adjust = TRUE),
    cbind(stat = 0, n = 3L)
  )
  expect_equal(
    heuristic_in_bin(color = cc, s = 2*(1:3), adjust = TRUE, scale = TRUE),
    cbind(stat = 0, n = 3L)
  )
})

test_that("heuristic() returns average R-squared", {
  ix <- c(rep(1, 60), rep(2, 90))
  y <- split(iris$Sepal.Length, ix)
  x1 <- split(iris$Sepal.Width, ix)
  x2 <- split(iris$Species, ix)
  f <- function(y, x) {
    summary(lm(y ~ x))[["r.squared"]]
  }

  expect_equal(
    heuristic(
      iris$Sepal.Width,
      iris$Sepal.Length,
      bins = ix,
      color_num = TRUE,
      scale = FALSE,
      adjusted = FALSE
    ),
    weighted.mean(mapply(f, y, x1), c(60, 90))
  )

  expect_equal(
    heuristic(
      iris$Species,
      iris$Sepal.Length,
      bins = ix,
      color_num = FALSE,
      scale = FALSE,
      adjusted = FALSE
    ),
    weighted.mean(mapply(f, y, x2), c(60, 90))
  )
})

