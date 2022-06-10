S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
X <- data.frame(x = c("a", "b"), y = c(100, 10))
shp <- shapviz(S, X, baseline = 4)

test_that("get_* functions work", {
  expect_equal(4, get_baseline(shp))
  expect_equal(S, get_shap_values(shp))
  expect_equal(X, get_feature_values(shp))
})

test_that("column order of S does no matter", {
  expect_equal(shp, shapviz(S[, 2:1], X, baseline = 4))
})

test_that("some input checks fire", {
  expect_error(shapviz(S[1, , drop = FALSE], X))
  expect_error(shapviz(S, X[1, ]))
  expect_error(shapviz(S[, 1, drop = FALSE], X))
  expect_error(shapviz(S, X[, 1]))
  expect_error(shapviz(matrix(S, ncol = 2, dimnames = list(NULL, c("d", "e"))), X))
  expect_error(shapviz(matrix(S, ncol = 2, dimnames = list(NULL, NULL)), X))
})

test_that("shapviz works with single row input", {
  shp1 <- shapviz(S[1L, , drop = FALSE], X[1L, , drop = FALSE], baseline = 4)
  expect_s3_class(sv_waterfall(shp1), "ggplot")
  expect_s3_class(sv_force(shp1), "ggplot")
  expect_error(sv_importance(shp1))
  expect_s3_class(sv_importance(shp1, kind = "bar"), "ggplot")
  expect_s3_class(sv_dependence(shp1, "x", color_var = "y"), "ggplot")
  expect_s3_class(sv_dependence(shp1, "x", color_var = "auto"), "ggplot")
})


