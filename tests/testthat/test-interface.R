S <- matrix(c(1, -1, -1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
X <- data.frame(x = c("a", "b"), y = c(100, 10))
shp <- shapviz(S, X, baseline = 4)

test_that("get_* functions work", {
  expect_equal(4, get_baseline(shp))
  expect_equal(S, get_shap_values(shp))
  expect_equal(X, get_feature_values(shp))
})

test_that("dim, nrow, ncol, colnames work", {
  expect_equal(dim(shp), c(2L, 2L))
  expect_equal(nrow(shp), 2L)
  expect_equal(ncol(shp), 2L)
  expect_equal(colnames(shp), colnames(S))
})

test_that("subsetting works", {
  expect_equal(dim(shp[, "x"]$S), c(2L, 1L))
  expect_equal(dim(shp[, "x"]$X), c(2L, 1L))
  expect_equal(dim(shp[1, "x"]$S), c(1L, 1L))
  expect_equal(dim(shp[1, "x"]$X), c(1L, 1L))
  expect_equal(dim(shp[1, ]$S), c(1L, 2L))
  expect_equal(dim(shp[1, ]$X), c(1L, 2L))
  expect_equal(get_baseline(shp[1, ]), get_baseline(shp))
})

test_that("concatenating with + works", {
  expect_equal(dim((shp + shp)$S), c(4L, 2L))
  expect_equal(dim((shp + shp)$X), c(4L, 2L))
  expect_equal((shp + shp)$baseline, shp$baseline)
  expect_equal(dim((shp + shp + shp)$S), c(6L, 2L))
  expect_equal(dim((shp + shp + shp)$X), c(6L, 2L))
})

test_that("concatenating with rbind works", {
  expect_equal(dim(rbind(shp, shp)$S), c(4L, 2L))
  expect_equal(dim(rbind(shp, shp)$X), c(4L, 2L))
  expect_equal(rbind(shp, shp)$baseline, shp$baseline)
  expect_equal(dim(rbind(shp, shp, shp)$S), c(6L, 2L))
  expect_equal(dim(rbind(shp, shp, shp)$X), c(6L, 2L))
})

test_that("print() gives no error", {
  capture_output(expect_no_error(print(shp)))
})

test_that("shapviz() gives error for default method", {
  expect_error(shapviz(1))
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
    shapviz(S, X = X, baseline = 4, S_inter = S_inter[, c("y", "x"), c("y", "x")])
  )
  expect_identical(
    get_shap_interactions(
      shapviz(S, X = X, baseline = 4, S_inter = S_inter[, c("y", "x"), c("y", "x")])
    ),
    S_inter
  )
  expect_silent(
    shapviz(
      S[, "x", drop = FALSE],
      X = X["x"],
      baseline = 4,
      S_inter = S_inter[, "x", "x", drop = FALSE]
    )
  )
  expect_equal(dim(shp_inter[, "x"]$S_inter), c(2L, 1L, 1L))
  expect_equal(dim(shp_inter[1, ]$S_inter), c(1L, 2L, 2L))
  expect_equal(dim(shp_inter[1, "x"]$S_inter), c(1L, 1L, 1L))
})

test_that("shapviz does not accept S_inter with bad colnames", {
  S_inter_noname <- array(c(1, -1, 0, 0, 0, 0, -1, 1), dim = c(2L, 2L, 2L))
  expect_error(shapviz(S, X = X, baseline = 4, S_inter = S_inter_noname))

  S_inter_badname <- S_inter_noname
  dimnames(S_inter_badname) <- list(NULL, c("x", "z"), c("x", "z"))
  expect_error(shapviz(S, X = X, baseline = 4, S_inter = S_inter_badname))

  dimnames(S_inter_badname) <- list(NULL, c("x", "y"), c("y", "x"))
  expect_error(shapviz(S, X = X, baseline = 4, S_inter = S_inter_badname))
})

# More tests on interactions
S_inter <- array(
  c(1, -1, 0, 0, 0, 0, -1, 1),
  dim = c(2L, 2L, 2L),
  dimnames = list(NULL, c("x", "y"), c("x", "y"))
)
shp_inter <- shapviz(S, X = X, baseline = 4, S_inter = S_inter)

test_that("get_shap_interactions, +, rbind works for interactions", {
  expect_equal(S_inter, get_shap_interactions(shp_inter))
  expect_equal(dim((shp_inter + shp_inter)$S_inter)[1L], 2 * dim(shp_inter$S_inter)[1L])
  expect_equal(
    dim(rbind(shp_inter, shp_inter, shp_inter)$S_inter)[1L],
    3 * dim(shp_inter$S_inter)[1L]
  )
})

test_that("print() gives no error (with interactions)", {
  capture_output(expect_no_error(print(shp_inter)))
})

# Multiclass
X_pred <- data.matrix(iris[, -5L])
dtrain <- xgboost::xgb.DMatrix(X_pred, label = as.integer(iris[, 5L]) - 1L)
fit <- xgboost::xgb.train(
  data = dtrain,
  nrounds = 50L,
  nthread = 1L,
  objective="multi:softprob",
  num_class=3L
)

test_that("multiclass xgb model gives 'mshapviz' object", {
  expect_true(is.mshapviz(shapviz(fit, X_pred = X_pred, interactions = TRUE)))
})

test_that("multiclass xgb model with 'which_class' gives 'shapviz' object", {
  expect_true(
    is.shapviz(shapviz(fit, X_pred = X_pred, which_class = 3L, interactions = TRUE))
  )
})

