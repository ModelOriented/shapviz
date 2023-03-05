S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
X <- data.frame(x = c("a", "b"), y = c(100, 10))
shp <- shapviz(S, X, baseline = 4)

test_that("get_* functions work", {
  expect_equal(4, get_baseline(shp))
  expect_equal(S, get_shap_values(shp))
  expect_equal(X, get_feature_values(shp))
})

test_that("dim, nrow, ncol work", {
  expect_equal(dim(shp), c(2L, 2L))
  expect_equal(nrow(shp), 2L)
  expect_equal(ncol(shp), 2L)
})

test_that("column order of X does no matter", {
  expect_equal(shp, shapviz(S, X[, 2:1], baseline = 4))
})

test_that("X can contain additional columns", {
  expect_equal(shp, shapviz(S, X = cbind(X, z = 1:2), baseline = 4))
})

test_that("some input checks fire", {
  expect_error(shapviz(S[1, , drop = FALSE], X))
  expect_error(shapviz(S, X[1, ]))
  expect_error(shapviz(S, X[, 1]))
  expect_error(shapviz(matrix(S, ncol = 2, dimnames = list(NULL, c("d", "e"))), X))
  expect_error(shapviz(matrix(S, ncol = 2, dimnames = list(NULL, NULL)), X))
})

test_that("shapviz works with single row input", {
  shp1 <- shapviz(S[1L, , drop = FALSE], X[1L, , drop = FALSE], baseline = 4)
  expect_s3_class(sv_waterfall(shp1), "ggplot")
  expect_s3_class(sv_force(shp1), "ggplot")
  expect_s3_class(sv_importance(shp1, kind = "beeswarm"), "ggplot")
  expect_s3_class(sv_importance(shp1), "ggplot")
  expect_s3_class(sv_dependence(shp1, "x", color_var = "y"), "ggplot")
  expect_s3_class(sv_dependence(shp1, "x"), "ggplot")
})

test_that("shapviz works with single column input", {
  shp2 <- shapviz(S[, 1L, drop = FALSE], X[, 1L, drop = FALSE])
  expect_s3_class(sv_waterfall(shp2), "ggplot")
  expect_s3_class(sv_force(shp2), "ggplot")
  expect_s3_class(sv_importance(shp2, kind = "both"), "ggplot")
  expect_s3_class(sv_dependence(shp2, "x"), "ggplot")
  expect_s3_class(sv_dependence(shp2, "x", color_var = NULL), "ggplot")
})

# Interactions
test_that("shapviz accepts correct S_inter", {
  S_inter <- array(
    c(1, -1, 0, 0, 0, 0, -1, 1),
    dim = c(2L, 2L, 2L),
    dimnames = list(NULL, c("x", "y"), c("x", "y"))
  )
  expect_silent(shp_inter <- shapviz(S, X = X, baseline = 4, S_inter = S_inter))
  expect_silent(
    shapviz(
      S[1L, , drop = FALSE],
      X = X[1L, ],
      baseline = 4,
      S_inter = S_inter[1L, , , drop = FALSE]
    )
  )
  expect_silent(
    shapviz(
      S[, "x", drop = FALSE],
      X = X["x"],
      baseline = 4,
      S_inter = S_inter[, "x", "x", drop = FALSE]
    )
  )
})

test_that("shapviz does not accept bad S_inter", {
  S_inter_noname <- array(c(1, -1, 0, 0, 0, 0, -1, 1), dim = c(2L, 2L, 2L))
  expect_error(shapviz(S, X = X, baseline = 4, S_inter = S_inter_noname))
})
